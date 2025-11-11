// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#include "bittide_ugn.h"
#include "bittide_scatter.h"
#include "bittide_gather.h"

// Magic numbers for message framing (same as elara)
#define BT_MAGIC_LO (0x8cb5f3192996a86bULL)
#define BT_MAGIC_HI (0x18c6659dc30d46f2ULL)

// Message response codes
#define RECEIVED_INCOMING_UGN 1
#define RECEIVED_OUTGOING_UGN_ACK 2

// ============================================================================
// Low-level Message Helpers (similar to elara_snd_lib.c and elara_rec_lib.c)
// ============================================================================

// Send a 2-argument command to scatter unit at specified offset
static void send_cmd2(ScatterUnit* unit, uint32_t offset, uint64_t cmd,
                     uint64_t arg1, uint64_t arg2) {
    uint64_t buffer[5];

    buffer[0] = BT_MAGIC_LO;
    buffer[1] = BT_MAGIC_HI;
    buffer[2] = cmd;
    buffer[3] = arg1;
    buffer[4] = arg2;

    scatter_unit_write_slice(unit, buffer, offset, 5);
}

// Check if there's a valid message in gather unit at specified offset
static bool received_magic(GatherUnit* unit, uint32_t offset) {
    uint64_t data[2];

    gather_unit_read_slice(unit, data, offset, 2);
    return (data[0] == BT_MAGIC_LO) && (data[1] == BT_MAGIC_HI);
}

// Read a word from gather buffer at offset (with additional word offset)
static uint64_t read_at_offset(GatherUnit* unit, uint32_t offset, uint32_t word_offset) {
    uint64_t data;

    gather_unit_read_slice(unit, &data, offset + word_offset, 1);
    return data;
}

// Invalidate scatter buffer at specified offset
static void invalidate(ScatterUnit* unit, uint32_t offset) {
    uint64_t zeros[5] = {0, 0, 0, 0, 0};

    scatter_unit_write_slice(unit, zeros, offset, 5);
}

// ============================================================================
// Protocol Helper Functions (following simultaneousugn source)
// ============================================================================

// Initialize UGN object (similar to initialize_ugn_object in source)
static void initialize_ugn_object(UgnEdge* u, uint32_t src_node, uint32_t src_port,
                                  uint32_t dst_node, uint32_t dst_port, int64_t ugn) {
    u->src_node = src_node;
    u->src_port = src_port;
    u->dst_node = dst_node;
    u->dst_port = dst_port;
    u->ugn = ugn;
    u->is_valid = 1;
}

// Extract response meaning from received message (like receiver_extract_response_meaning)
static uint32_t receiver_extract_response_meaning(GatherUnit* unit, uint32_t offset,
                                                  uint32_t port, uint32_t node_id,
                                                  UgnEdge* u) {
    uint64_t cmd = read_at_offset(unit, offset, 2);

    if (cmd == BT_SENDING_UGN) {
        // Received a UGN timing message
        int64_t ugn = (int64_t)offset + 3 - (int64_t)read_at_offset(unit, offset, 3);
        uint64_t remote_nodeid_port = read_at_offset(unit, offset, 4);

        initialize_ugn_object(u,
            ((uint32_t)remote_nodeid_port) & 0xffff,          // src_node
            (((uint32_t)remote_nodeid_port) & 0xffff0000) >> 16,  // src_port
            node_id,                                           // dst_node (us)
            port,                                              // dst_port
            ugn);
        return RECEIVED_INCOMING_UGN;

    } else if (cmd == BT_FOUND_UGN) {
        // Received acknowledgement for our outgoing UGN
        int64_t ugn = (int64_t)read_at_offset(unit, offset, 3);
        uint64_t remote_nodeid_port = read_at_offset(unit, offset, 4);

        initialize_ugn_object(u,
            node_id,                                           // src_node (us)
            port,                                              // src_port
            ((uint32_t)remote_nodeid_port) & 0xffff,          // dst_node
            (((uint32_t)remote_nodeid_port) & 0xffff0000) >> 16,  // dst_port
            ugn);
        return RECEIVED_OUTGOING_UGN_ACK;
    }

    return 0;  // Unknown response type
}

// Send messages to all ports (like send_messages_to_all_ports in source)
// ============================================================================
// Protocol Event Handlers
// ============================================================================

void send_ugn_to_port(UgnContext* ctx, uint32_t port, uint32_t offset) {
    if (port >= ctx->num_ports) return;

    uint64_t port_nodeid = (uint64_t)(ctx->node_id) + ((port + 1) << 16);

    // Announce our local time to communicate outgoing UGN
    send_cmd2(&ctx->scatter_units[port], offset, BT_SENDING_UGN,
             offset + 3, port_nodeid);

    // If we have a valid incoming UGN for this port, send back ack
    if (ctx->incoming_link_ugn_list[port].is_valid) {
        send_cmd2(&ctx->scatter_units[port], offset + 5, BT_FOUND_UGN,
                 (uint64_t)(ctx->incoming_link_ugn_list[port].ugn),
                 (uint64_t)(ctx->node_id) + ((port + 1) << 16));
    }
}

void send_ugns_to_all_ports(UgnContext* ctx, uint32_t offset) {
    for (uint32_t port = 0; port < ctx->num_ports; port++) {
        send_ugn_to_port(ctx, port, offset);
    }
}

void check_incoming_buffer(UgnContext* ctx, uint32_t port, uint32_t offset) {
    if (port >= ctx->num_ports) return;

    GatherUnit* unit = &ctx->gather_units[port];

    // Check if scatter unit has signaled "received"
    if (!received_magic(unit, offset)) return;

    // Create temporary UgnEdge for receiver_extract_response_meaning
    UgnEdge temp_edge;
    uint32_t meaning =
        receiver_extract_response_meaning(unit, offset, port, ctx->node_id, &temp_edge);

    if (meaning == 0) return;

    if (meaning == RECEIVED_OUTGOING_UGN_ACK) {
        // Use the UGN from temp_edge
        ctx->outgoing_link_ugn_list[port].ugn = temp_edge.ugn;
        ctx->outgoing_link_ugn_list[port].is_valid = true;
        return;
    }

    if (meaning == RECEIVED_INCOMING_UGN) {
        // Use the UGN from temp_edge
        ctx->incoming_link_ugn_list[port].ugn = temp_edge.ugn;
        ctx->incoming_link_ugn_list[port].is_valid = true;
    }
}

// Check all incoming buffers (like check_all_incoming_buffers in source)
void check_all_incoming_buffers(UgnContext* ctx, uint32_t offset) {
    for (uint32_t port = 0; port < ctx->num_ports; port++) {
        check_incoming_buffer(ctx, port, offset);
    }
}

void invalidate_port(UgnContext* ctx, uint32_t port, uint32_t offset) {
    if (port >= ctx->num_ports) return;
    invalidate(&ctx->scatter_units[port], offset);
}

// Handle invalidate (like handle_invalidate in source)
void handle_invalidate(UgnContext* ctx, uint32_t offset) {
    for (uint32_t port = 0; port < ctx->num_ports; port++) {
        invalidate_port(ctx, port, offset);
    }
}

// ============================================================================
// UGN Edge Helper Functions
// ============================================================================

void ugn_edge_init(UgnEdge* edge) {
    edge->src_node = 0;
    edge->src_port = 0;
    edge->dst_node = 0;
    edge->dst_port = 0;
    edge->ugn = 0;
    edge->is_valid = 0;
}

void ugn_edge_set(UgnEdge* edge, uint32_t src_node, uint32_t src_port,
                  uint32_t dst_node, uint32_t dst_port, int64_t ugn) {
    edge->src_node = src_node;
    edge->src_port = src_port;
    edge->dst_node = dst_node;
    edge->dst_port = dst_port;
    edge->ugn = ugn;
    edge->is_valid = 1;
}

// ============================================================================
// UGN Context Management
// ============================================================================

void ugn_context_init(UgnContext* ctx, ScatterUnit* scatter_units,
                      GatherUnit* gather_units, uint32_t num_ports,
                      uint32_t node_id,
                      UgnEdge* incoming_list, UgnEdge* outgoing_list,
                      uint32_t max_degree) {
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

    // Initialize counters
    ctx->number_outgoing_link_ugns_acknowledged = 0;
    ctx->number_incoming_link_ugns_known = 0;
    ctx->announced_done = 0;
}
