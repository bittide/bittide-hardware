// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#include "bittide_ugn.h"
#include "bittide_scatter.h"
#include "bittide_gather.h"

// Magic numbers for message framing (same as elara)
#define BT_MAGIC_LO (0x8cb5f3192996a86bULL)
#define BT_MAGIC_HI (0x18c6659dc30d46f2ULL)

// ============================================================================
// Message Types and Encoding
// ============================================================================

typedef enum {
    MSG_TYPE_ANNOUNCE = 0xf0f00001ULL,      // Announcing presence/timing
    MSG_TYPE_ACKNOWLEDGE = 0xf0f00002ULL    // Acknowledging neighbor's announcement
} UgnMessageType;

// Message structure (logical view)
typedef struct {
    UgnMessageType type;
    int64_t timing_or_ugn;     // For ANNOUNCE: timing offset, for ACK: ugn value
    uint32_t node_id;
    uint32_t port;
} UgnMessage;

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

// Read a complete message from scatter buffer
static bool ugn_read_message(ScatterUnit* unit, uint32_t offset, UgnMessage* msg) {
    uint64_t data[5];
    scatter_unit_read_slice(unit, data, offset, 5);

    // Check magic bytes
    if (data[0] != BT_MAGIC_LO || data[1] != BT_MAGIC_HI) {
        return false;
    }

    msg->type = (UgnMessageType)data[2];
    msg->timing_or_ugn = (int64_t)data[3];

    uint64_t encoded_node_port = data[4];
    msg->node_id = ugn_decode_node_id(encoded_node_port);
    msg->port = ugn_decode_port(encoded_node_port);

    return true;
}

// Write a message to gather buffer
static void ugn_write_message(GatherUnit* unit, uint32_t offset, const UgnMessage* msg) {
    uint64_t buffer[5];

    buffer[0] = BT_MAGIC_LO;
    buffer[1] = BT_MAGIC_HI;
    buffer[2] = msg->type;
    buffer[3] = (uint64_t)msg->timing_or_ugn;
    buffer[4] = ugn_encode_node_port(msg->node_id, msg->port);

    gather_unit_write_slice(unit, buffer, offset, 5);
}

// Invalidate gather buffer at specified offset
static void invalidate(GatherUnit* unit, uint32_t offset) {
    uint64_t zeros[5] = {0, 0, 0, 0, 0};

    gather_unit_write_slice(unit, zeros, offset, 5);
}

// ============================================================================
// Protocol Helper Functions
// ============================================================================

// Initialize UGN object
static void initialize_ugn_object(UgnEdge* u, uint32_t src_node, uint32_t src_port,
                                  uint32_t dst_node, uint32_t dst_port, int64_t ugn) {
    u->src_node = src_node;
    u->src_port = src_port;
    u->dst_node = dst_node;
    u->dst_port = dst_port;
    u->ugn = ugn;
    u->is_valid = 1;
}

// Process received message and update UGN edge
// Returns true if message was valid and processed
static bool process_ugn_message(const UgnMessage* msg, uint32_t local_port,
                                uint32_t local_node_id, uint32_t offset, UgnEdge* edge) {
    if (msg->type == MSG_TYPE_ANNOUNCE) {
        // Received announcement from neighbor - calculate incoming UGN
        int64_t ugn = (int64_t)offset + 3 - msg->timing_or_ugn;

        initialize_ugn_object(edge,
            msg->node_id,        // src_node (remote)
            msg->port,           // src_port (remote)
            local_node_id,       // dst_node (us)
            local_port,          // dst_port (us)
            ugn);
        return true;

    } else if (msg->type == MSG_TYPE_ACKNOWLEDGE) {
        // Received acknowledgment from neighbor - store outgoing UGN
        initialize_ugn_object(edge,
            local_node_id,       // src_node (us)
            local_port,          // src_port (us)
            msg->node_id,        // dst_node (remote)
            msg->port,           // dst_port (remote)
            msg->timing_or_ugn); // ugn value
        return true;
    }

    return false;
}
// ============================================================================
// Protocol Event Handlers
// ============================================================================

void send_ugn_to_port(UgnContext* ctx, uint32_t port, uint32_t offset) {
    if (port >= ctx->num_ports) return;

    // Send announcement message
    UgnMessage announce = {
        .type = MSG_TYPE_ANNOUNCE,
        .timing_or_ugn = offset + 3,
        .node_id = ctx->node_id,
        .port = port
    };
    ugn_write_message(&ctx->gather_units[port], offset, &announce);

    // If we have received an announcement from this neighbor, send acknowledgment
    if (ctx->incoming_link_ugn_list[port].is_valid) {
        UgnMessage ack = {
            .type = MSG_TYPE_ACKNOWLEDGE,
            .timing_or_ugn = ctx->incoming_link_ugn_list[port].ugn,
            .node_id = ctx->node_id,
            .port = port
        };
        ugn_write_message(&ctx->gather_units[port], offset + 5, &ack);
    }
}

bool check_incoming_buffer(UgnContext* ctx, uint32_t port, uint32_t offset) {
    if (port >= ctx->num_ports) return false;

    ScatterUnit* unit = &ctx->scatter_units[port];

    // Try to read message from scatter buffer
    UgnMessage msg;
    if (!ugn_read_message(unit, offset, &msg)) {
        return false;  // No valid message
    }

    // Process the message based on its type
    if (msg.type == MSG_TYPE_ANNOUNCE) {
        // Received announcement - update incoming link UGN
        UgnEdge edge;
        if (process_ugn_message(&msg, port, ctx->node_id, offset, &edge)) {
            ctx->incoming_link_ugn_list[port] = edge;
            ctx->number_incoming_link_ugns_known++;
            return true;
        }
    } else if (msg.type == MSG_TYPE_ACKNOWLEDGE) {
        // Received acknowledgment - update outgoing link UGN
        UgnEdge edge;
        if (process_ugn_message(&msg, port, ctx->node_id, offset, &edge)) {
            ctx->outgoing_link_ugn_list[port] = edge;
            ctx->number_outgoing_link_ugns_acknowledged++;
            return true;
        }
    }

    return false;
}

void invalidate_port(UgnContext* ctx, uint32_t port, uint32_t offset) {
    if (port >= ctx->num_ports) return;
    invalidate(&ctx->gather_units[port], offset);
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
