// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

// Magic numbers for message framing (same as elara)
#define BT_MAGIC_LO (0x8cb5f3192996a86bULL)
#define BT_MAGIC_HI (0x18c6659dc30d46f2ULL)

#include "bittide_ugn.h"
// ============================================================================
// Message Types and Encoding
// ============================================================================

// Encode node_id and port into a single uint64_t
static inline uint64_t ugn_encode_node_port(uint32_t node_id, uint32_t port) {
  // node_id in lower 32 bits, (port+1) in upper 32 bits
  return node_id | (((uint64_t)(port + 1)) << 32);
}

// Decode node_id from encoded value
static inline uint32_t ugn_decode_node_id(uint64_t encoded) {
  return (uint32_t)(encoded & 0xffffffff);
}

// Decode port from encoded value
static inline uint32_t ugn_decode_port(uint64_t encoded) {
  uint32_t port_plus_one = (uint32_t)(encoded >> 32);
  return port_plus_one - 1;
}

// ============================================================================
// Message Reading and Writing
// ============================================================================

// Read a complete message from scatter buffer
static bool ugn_read_message(ScatterUnit unit, uint32_t offset,
                             UgnMessage *msg) {
  uint64_t data[6];
  scatter_unit_read_slice_wrapping(unit, data, offset, 6);

  // Check magic bytes
  if (data[0] != BT_MAGIC_LO || data[1] != BT_MAGIC_HI) {
    return false;
  }

  msg->type = (UgnMessageType)data[2];
  msg->local_counter = data[3];
  msg->remote_counter = data[4];

  uint64_t encoded_node_port = data[5];
  msg->node_id = ugn_decode_node_id(encoded_node_port);
  msg->port = ugn_decode_port(encoded_node_port);

  return true;
}

// ============================================================================
// Protocol Helper Functions
// ============================================================================

// Initialize UGN object
static void initialize_ugn_object(UgnEdge *u, uint32_t src_node,
                                  uint32_t src_port, uint32_t dst_node,
                                  uint32_t dst_port, int64_t ugn) {
  u->src_node = src_node;
  u->src_port = src_port;
  u->dst_node = dst_node;
  u->dst_port = dst_port;
  u->ugn = ugn;
  u->is_valid = true;
}

// Process received message and update UGN edge
// Returns true if message was valid and processed
static bool process_ugn_message(const UgnMessage *msg, uint32_t local_port,
                                uint32_t local_node_id, uint64_t local_time,
                                UgnEdge *edge_in, UgnEdge *edge_out) {

  int64_t receive_delay = (int64_t)local_time - (int64_t)msg->local_counter;
  initialize_ugn_object(
      edge_in,
      msg->node_id,   // src_node (remote)
      msg->port,      // src_port (remote)
      local_node_id,  // dst_node (us)
      local_port,     // dst_port (us)
      receive_delay); // delay (receive_delay for incoming edge)

  if (msg->remote_counter != 0) {
    // Received acknowledgment from neighbor - this is an OUTGOING edge
    // Calculate send_delay = neighbor's receive_time - our original send_time
    // send_delay = local_counter - remote_counter
    int64_t send_delay =
        (int64_t)msg->local_counter - (int64_t)msg->remote_counter;

    initialize_ugn_object(edge_out,
                          local_node_id, // src_node (us)
                          local_port,    // src_port (us)
                          msg->node_id,  // dst_node (remote)
                          msg->port,     // dst_port (remote)
                          send_delay);   // delay (send_delay for outgoing edge)
    return true;
  }

  return true;
}
// ============================================================================
// Protocol Event Handlers
// ============================================================================

bool send_ugn_to_port(UgnContext *ctx, Timer timer, Event *event) {

  GatherUnit unit = ctx->gather_units[event->port];
  uint32_t port = event->port;
  uint32_t offset = event->event_time % GATHER_UNIT_GATHER_MEMORY_LEN;
  uint64_t deadline = event->event_time + GATHER_UNIT_GATHER_MEMORY_LEN;
  if (port >= ctx->num_ports)
    return false;

  // Use msg_type from event struct
  UgnMessageType msg_type = event->msg_type;

  UgnMessage msg = {.type = msg_type,
                    .local_counter = deadline,
                    .remote_counter = 0,
                    .node_id = ctx->node_id,
                    .port = port};

  // If sending ACKNOWLEDGE, calculate neighbor's original send time
  if (msg_type == MSG_TYPE_ACKNOWLEDGE) {
    // Calculate when neighbor should have sent their announcement to arrive
    // at this event_time.
    uint64_t receive_time = deadline;
    int64_t receive_delay = ctx->incoming_link_ugn_list[port].ugn;
    uint64_t send_time = (uint64_t)(int64_t)receive_time - receive_delay;
    msg.remote_counter = send_time;
  }

  // Format message into buffer
  uint64_t buffer[6];
  buffer[0] = BT_MAGIC_LO;
  buffer[1] = BT_MAGIC_HI;
  buffer[2] = msg.type;
  buffer[3] = msg.local_counter;
  buffer[4] = msg.remote_counter;
  buffer[5] = ugn_encode_node_port(msg.node_id, msg.port);

  // Wait for the arrival time of the event
  timer_wait_until_cycles(timer, event->event_time);

  // Do real time write
  gather_unit_write_slice_wrapping(unit, buffer, offset, 6);

  // Verify that we made the deadline
  CompareResult cmp_result = timer_compare_cycles(timer, deadline);

  // Do book keeping
  if (cmp_result == COMPARE_LESS) {
    ctx->met_send_count++;
  } else {
    ctx->missed_send_count++;
  }
  if (msg_type == MSG_TYPE_ACKNOWLEDGE) {
    ctx->outgoing_acknowledge_count++;
  } else {
    ctx->outgoing_announce_count++;
  }
  return (cmp_result == COMPARE_LESS);
}

bool check_incoming_buffer(UgnContext *ctx, Timer timer, Event *event) {
  ScatterUnit unit = ctx->scatter_units[event->port];
  if (event->port >= ctx->num_ports)
    return false;

  // If both incoming and outgoing UGNs are already valid, ignore further
  // messages for this port
  if (ctx->incoming_link_ugn_list[event->port].is_valid &&
      ctx->outgoing_link_ugn_list[event->port].is_valid) {
    return false;
  }

  UgnMessage msg;
  UgnEdge edge_in;
  UgnEdge edge_out;
  uint32_t offset = event->event_time % SCATTER_UNIT_SCATTER_MEMORY_LEN;
  uint64_t deadline = event->event_time + SCATTER_UNIT_SCATTER_MEMORY_LEN;

  timer_wait_until_cycles(timer, event->event_time);
  // Try to read message from scatter buffer
  bool message_available = ugn_read_message(unit, offset, &msg);
  CompareResult cmp_result = timer_compare_cycles(timer, deadline);

  // Do book keeping
  if (cmp_result == COMPARE_LESS) {
    ctx->met_receive_count++;
  } else {
    ctx->missed_receive_count++;
  }

  if (!message_available) {
    return false; // No valid message
  }

  bool valid_message = process_ugn_message(
      &msg, event->port, ctx->node_id, event->event_time, &edge_in, &edge_out);
  if (!valid_message) {
    return false;
  }

  ctx->incoming_link_ugn_list[event->port] = edge_in;
  if (msg.type == MSG_TYPE_ACKNOWLEDGE) {
    ctx->outgoing_link_ugn_list[event->port] = edge_out;
    ctx->incoming_acknowledge_count++;
  } else {
    ctx->incoming_announce_count++;
  }

  return (cmp_result == COMPARE_LESS);
}

void invalidate_port(UgnContext *ctx, Timer timer, Event *event) {
  uint32_t port = event->port;
  GatherUnit gather_unit = ctx->gather_units[port];
  uint64_t deadline = event->event_time + GATHER_UNIT_GATHER_MEMORY_LEN;
  uint32_t offset = event->event_time % GATHER_UNIT_GATHER_MEMORY_LEN;
  if (port >= ctx->num_ports)
    return;
  uint64_t zeros[6] = {0, 0, 0, 0, 0, 0};

  timer_wait_until_cycles(timer, event->event_time);
  gather_unit_write_slice_wrapping(gather_unit, zeros, offset, 6);
  CompareResult cmp_result = timer_compare_cycles(timer, deadline);

  // Do book keeping
  if (cmp_result == COMPARE_LESS) {
    ctx->met_invalidate_count++;
  } else {
    ctx->missed_invalidate_count++;
  }
}

// ============================================================================
// UGN Edge Helper Functions
// ============================================================================

void ugn_edge_init(UgnEdge *edge) {
  edge->src_node = 0;
  edge->src_port = 0;
  edge->dst_node = 0;
  edge->dst_port = 0;
  edge->ugn = 0;
  edge->is_valid = false;
}

// ============================================================================
// UGN Context Management
// ============================================================================

void ugn_context_init(UgnContext *ctx, ScatterUnit *scatter_units,
                      GatherUnit *gather_units, uint32_t num_ports,
                      uint32_t node_id, UgnEdge *incoming_list,
                      UgnEdge *outgoing_list, uint32_t max_degree) {
  ctx->scatter_units = scatter_units;
  ctx->gather_units = gather_units;
  ctx->num_ports = num_ports;
  ctx->node_id = node_id;
  ctx->incoming_link_ugn_list = incoming_list;
  ctx->outgoing_link_ugn_list = outgoing_list;
  ctx->max_degree = max_degree;

  // Initialize UGN edge lists
  for (uint32_t i = 0; i < max_degree; i++) {
    ugn_edge_init(&ctx->incoming_link_ugn_list[i]);
    ugn_edge_init(&ctx->outgoing_link_ugn_list[i]);
  }

  ctx->missed_send_count = 0;
  ctx->missed_receive_count = 0;
  ctx->missed_invalidate_count = 0;
  ctx->met_send_count = 0;
  ctx->met_receive_count = 0;
  ctx->met_invalidate_count = 0;
}

bool port_done(UgnContext *ctx, uint32_t port) {
  if (port < ctx->num_ports) {
    bool incoming_done = ctx->incoming_link_ugn_list[port].is_valid;
    bool outgoing_done = ctx->outgoing_link_ugn_list[port].is_valid;
    return incoming_done && outgoing_done;
  }
  return false;
}
bool all_ports_done(UgnContext *ctx) {
  for (uint32_t port = 0; port < ctx->num_ports; port++) {
    if (!port_done(ctx, port)) {
      return false;
    }
  }
  return true;
}
