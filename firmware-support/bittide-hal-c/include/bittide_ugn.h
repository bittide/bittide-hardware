// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#ifndef BITTIDE_UGN_H
#define BITTIDE_UGN_H

#include <stdbool.h>
#include <stdint.h>

// ============================================================================
// Event System Constants
// ============================================================================
typedef enum {
  EVENT_TYPE_SEND = 1,
  EVENT_TYPE_RECEIVE = 2,
  EVENT_TYPE_INVALIDATE = 3
} EventType;

static inline const char *show_event_type(EventType type) {
  switch (type) {
  case EVENT_TYPE_SEND:
    return "SEND";
  case EVENT_TYPE_RECEIVE:
    return "RECEIVE";
  case EVENT_TYPE_INVALIDATE:
    return "INVALIDATE";
  default:
    return "UNKNOWN";
  }
}
typedef enum {
  MSG_TYPE_ANNOUNCE = 0xF,    // Announcing presence/timing
  MSG_TYPE_ACKNOWLEDGE = 0xF0 // Acknowledging neighbor's announcement
} UgnMessageType;

static inline const char *show_msg_type(UgnMessageType type) {
  switch (type) {
  case MSG_TYPE_ANNOUNCE:
    return "ANNOUNCE";
  case MSG_TYPE_ACKNOWLEDGE:
    return "ACKNOWLEDGE";
  default:
    return "UNKNOWN";
  }
}

typedef struct {
  EventType type;
  UgnMessageType
      msg_type; // For SEND events: MSG_TYPE_ANNOUNCE or MSG_TYPE_ACKNOWLEDGE
  uint16_t port;
  uint64_t event_time;
} Event;

// Event encoding: [type(8 bits) | msg_type(8 bits) | port(16 bits) | offset(32
// bits)] Event time is stored separately in the priority queue's priority field
#define EVENT_PORT_SHIFT 32
#define EVENT_PORT_MASK (0x0000FFFF00000000ULL)
#define EVENT_OFFSET_SHIFT 0
#define EVENT_OFFSET_MASK (0x00000000FFFFFFFFULL)
#define EVENT_TYPE_SHIFT 56
#define EVENT_TYPE_MASK (0xFF00000000000000ULL)
#define EVENT_MSG_TYPE_SHIFT 48
#define EVENT_MSG_TYPE_MASK (0x00FF000000000000ULL)

// ============================================================================
// UGN Edge Structure
// ============================================================================
// Represents UGN (Uninterpretable Garbage Number) information for a network
// edge Stores delay measurement (direction-specific):
//   - For incoming edges: stores receive_delay (neighbor -> us)
//   - For outgoing edges: stores send_delay (us -> neighbor)
typedef struct {
  uint32_t src_node; // Source node ID
  uint32_t src_port; // Source port number
  uint32_t dst_node; // Destination node ID
  uint32_t dst_port; // Destination port number
  int64_t
      ugn; // Propagation delay in clock cycles (direction depends on edge type)
  bool is_valid; // Whether this UGN entry is valid
} UgnEdge;

// ============================================================================
// UGN Protocol Context
// ============================================================================
// Holds all state needed for UGN discovery protocol
typedef struct {
  // Scatter and gather units (one per port)
  ScatterUnit *scatter_units;
  GatherUnit *gather_units;
  uint32_t num_ports;

  // UGN edge lists (dynamically sized based on max_degree)
  UgnEdge *incoming_link_ugn_list;
  UgnEdge *outgoing_link_ugn_list;

  uint32_t max_degree; // Maximum number of neighbor nodes

  // Node identification
  uint32_t node_id;

  // Bookkeeping
  uint32_t incoming_announce_count;
  uint32_t incoming_acknowledge_count;
  uint32_t outgoing_announce_count;
  uint32_t outgoing_acknowledge_count;

  uint32_t missed_send_count;
  uint32_t missed_receive_count;
  uint32_t missed_invalidate_count;
  uint32_t met_send_count;
  uint32_t met_receive_count;
  uint32_t met_invalidate_count;
} UgnContext;

// Message structure (logical view)
typedef struct {
  UgnMessageType type;
  uint64_t local_counter;  // Sender's local time (event_time)
  uint64_t remote_counter; // For ACK: sender_time from incoming edge; For
                           // ANNOUNCE: 0
  uint32_t node_id;
  uint32_t port;
} UgnMessage;

// ============================================================================
// Event Helper Functions
// ============================================================================

// Encode event struct into uint64_t
static inline uint64_t encode_event(Event event) {
  // Extract just the lower 8 bits of msg_type for encoding
  uint8_t msg_type_bits = (uint8_t)((uint64_t)event.msg_type & 0xFF);
  return ((uint64_t)event.type << EVENT_TYPE_SHIFT) |
         ((uint64_t)msg_type_bits << EVENT_MSG_TYPE_SHIFT) |
         (((uint64_t)event.port << EVENT_PORT_SHIFT) & EVENT_PORT_MASK);
}

// Decode uint64_t into event struct
static inline Event decode_event(uint64_t encoded, uint64_t event_time) {
  Event event;
  event.type = (EventType)((encoded & EVENT_TYPE_MASK) >> EVENT_TYPE_SHIFT);
  event.msg_type = (UgnMessageType)(((encoded & EVENT_MSG_TYPE_MASK) >>
                                     EVENT_MSG_TYPE_SHIFT) &
                                    0xFF);
  event.port = (uint16_t)((encoded & EVENT_PORT_MASK) >> EVENT_PORT_SHIFT);
  event.event_time = event_time;
  return event;
}

// Legacy helpers for compatibility
static inline Event make_send_event(UgnMessageType msg_type,
                                    uint64_t event_time, uint32_t port) {
  Event event = {.type = EVENT_TYPE_SEND,
                 .msg_type = msg_type,
                 .port = (uint16_t)port,
                 .event_time = event_time};
  return event;
}

static inline Event make_invalidate_event(uint64_t event_time, uint32_t port) {
  Event event = {.type = EVENT_TYPE_INVALIDATE,
                 .msg_type = MSG_TYPE_ANNOUNCE,
                 .port = (uint16_t)port,
                 .event_time = event_time};
  return event;
}

static inline Event make_receive_event(uint64_t event_time, uint32_t port) {
  Event event = {.type = EVENT_TYPE_RECEIVE,
                 .msg_type = MSG_TYPE_ANNOUNCE,
                 .port = (uint16_t)port,
                 .event_time = event_time};
  return event;
}

// ============================================================================
// UGN Edge Helper Functions
// ============================================================================

// Initialize a UGN edge to invalid state
static inline void ugn_edge_init(UgnEdge *edge) {
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

// Initialize UGN protocol context
static inline void ugn_context_init(UgnContext *ctx, ScatterUnit *scatter_units,
                                    GatherUnit *gather_units,
                                    uint32_t num_ports, uint32_t node_id,
                                    UgnEdge *incoming_list,
                                    UgnEdge *outgoing_list,
                                    uint32_t max_degree) {
  ctx->scatter_units = scatter_units;
  ctx->gather_units = gather_units;
  ctx->num_ports = num_ports;
  ctx->node_id = node_id;
  ctx->incoming_link_ugn_list = incoming_list;
  ctx->outgoing_link_ugn_list = outgoing_list;
  ctx->max_degree = max_degree;

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
  ctx->incoming_announce_count = 0;
  ctx->incoming_acknowledge_count = 0;
  ctx->outgoing_announce_count = 0;
  ctx->outgoing_acknowledge_count = 0;
}

static inline bool port_done(UgnContext *ctx, uint32_t port) {
  if (port < ctx->num_ports) {
    bool incoming_done = ctx->incoming_link_ugn_list[port].is_valid;
    bool outgoing_done = ctx->outgoing_link_ugn_list[port].is_valid;
    return incoming_done && outgoing_done;
  }
  return false;
}

static inline bool all_ports_done(UgnContext *ctx) {
  for (uint32_t port = 0; port < ctx->num_ports; port++) {
    if (!port_done(ctx, port)) {
      return false;
    }
  }
  return true;
}

// ============================================================================
// Buffer Offset Calculation
// ============================================================================
// These functions centralize the event_time -> buffer_offset translation logic.
// Future changes to the offset calculation (e.g., for non-trivial calendar
// configurations) only need to be updated here.

// Calculate scatter buffer offset for a given event time and port
static inline uint32_t ugn_calculate_scatter_offset(const UgnContext *ctx,
                                                    uint32_t port,
                                                    uint64_t event_time) {
  (void)ctx;
  (void)port; // Unused in this implementation
  return (uint32_t)(event_time % SCATTER_UNIT_SCATTER_MEMORY_LEN);
}

// Calculate gather buffer offset for a given event time and port
static inline uint32_t ugn_calculate_gather_offset(const UgnContext *ctx,
                                                   uint32_t port,
                                                   uint64_t event_time) {
  (void)ctx;
  (void)port; // Unused in this implementation
  return (uint32_t)(event_time % GATHER_UNIT_GATHER_MEMORY_LEN);
}

// ============================================================================
// Protocol Event Handlers - Implementation
// ============================================================================

// Magic numbers for message framing (same as elara)
#define BT_MAGIC_LO (0x8cb5f3192996a86bULL)
#define BT_MAGIC_HI (0x18c6659dc30d46f2ULL)

// Encode node_id and port into a single uint64_t
static inline uint64_t ugn_encode_node_port(uint32_t node_id, uint32_t port) {
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

// Read a complete message from scatter buffer
static inline bool ugn_read_message(ScatterUnit *unit, uint32_t offset,
                                    UgnMessage *msg) {
  uint64_t data[6];
  scatter_unit_read_slice_wrapping(*unit, data, offset, 6);

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

// Initialize UGN object
static inline void initialize_ugn_object(UgnEdge *u, uint32_t src_node,
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
static inline bool process_ugn_message(const UgnMessage *msg,
                                       uint32_t local_port,
                                       uint32_t local_node_id,
                                       uint64_t local_time, UgnEdge *edge_in,
                                       UgnEdge *edge_out) {
  int64_t receive_delay = (int64_t)local_time - (int64_t)msg->local_counter;
  initialize_ugn_object(edge_in, msg->node_id, msg->port, local_node_id,
                        local_port, receive_delay);

  if (msg->remote_counter != 0) {
    int64_t send_delay =
        (int64_t)msg->local_counter - (int64_t)msg->remote_counter;
    initialize_ugn_object(edge_out, local_node_id, local_port, msg->node_id,
                          msg->port, send_delay);
    return true;
  }

  return true;
}

// Send UGN to a specific port
static inline bool send_ugn_to_port(UgnContext *ctx, Timer timer,
                                    Event *event) {
  GatherUnit *unit = &ctx->gather_units[event->port];
  uint32_t port = event->port;
  uint32_t offset = ugn_calculate_gather_offset(ctx, port, event->event_time);
  uint64_t deadline = event->event_time + GATHER_UNIT_GATHER_MEMORY_LEN;

  if (port >= ctx->num_ports)
    return false;

  UgnMessageType msg_type = event->msg_type;
  UgnMessage msg = {.type = msg_type,
                    .local_counter = deadline,
                    .remote_counter = 0,
                    .node_id = ctx->node_id,
                    .port = port};

  if (msg_type == MSG_TYPE_ACKNOWLEDGE) {
    uint64_t receive_time = deadline;
    int64_t receive_delay = ctx->incoming_link_ugn_list[port].ugn;
    uint64_t send_time = (uint64_t)((int64_t)receive_time - receive_delay);
    msg.remote_counter = send_time;
  }

  uint64_t buffer[6];
  buffer[0] = BT_MAGIC_LO;
  buffer[1] = BT_MAGIC_HI;
  buffer[2] = msg.type;
  buffer[3] = msg.local_counter;
  buffer[4] = msg.remote_counter;
  buffer[5] = ugn_encode_node_port(msg.node_id, msg.port);

  timer_wait_until_cycles(timer, event->event_time);
  gather_unit_write_slice_wrapping(*unit, buffer, offset, 6);
  CompareResult cmp_result = timer_compare_cycles(timer, deadline);

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

// Check incoming buffer for a specific port
static inline bool check_incoming_buffer(UgnContext *ctx, Timer timer,
                                         Event *event) {
  ScatterUnit *unit = &ctx->scatter_units[event->port];
  if (event->port >= ctx->num_ports)
    return false;

  if (ctx->incoming_link_ugn_list[event->port].is_valid &&
      ctx->outgoing_link_ugn_list[event->port].is_valid) {
    return false;
  }

  UgnMessage msg;
  UgnEdge edge_in;
  UgnEdge edge_out;
  uint32_t offset =
      ugn_calculate_scatter_offset(ctx, event->port, event->event_time);
  uint64_t deadline = event->event_time + SCATTER_UNIT_SCATTER_MEMORY_LEN;

  timer_wait_until_cycles(timer, event->event_time);
  bool message_available = ugn_read_message(unit, offset, &msg);
  CompareResult cmp_result = timer_compare_cycles(timer, deadline);

  if (cmp_result == COMPARE_LESS) {
    ctx->met_receive_count++;
  } else {
    ctx->missed_receive_count++;
  }

  if (!message_available) {
    return false;
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

// Invalidate old scatter buffer data for a specific port
static inline void invalidate_port(UgnContext *ctx, Timer timer, Event *event) {
  uint32_t port = event->port;
  GatherUnit *gather_unit = &ctx->gather_units[port];
  uint64_t deadline = event->event_time + GATHER_UNIT_GATHER_MEMORY_LEN;
  uint32_t offset = ugn_calculate_gather_offset(ctx, port, event->event_time);

  if (port >= ctx->num_ports)
    return;

  uint64_t zeros[6] = {0, 0, 0, 0, 0, 0};

  timer_wait_until_cycles(timer, event->event_time);
  gather_unit_write_slice_wrapping(*gather_unit, zeros, offset, 6);
  CompareResult cmp_result = timer_compare_cycles(timer, deadline);

  if (cmp_result == COMPARE_LESS) {
    ctx->met_invalidate_count++;
  } else {
    ctx->missed_invalidate_count++;
  }
}

#endif // BITTIDE_UGN_H
