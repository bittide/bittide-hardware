// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#ifndef BITTIDE_UGN_H
#define BITTIDE_UGN_H

#include <stdint.h>
#include <stdbool.h>
#include "bittide_scatter.h"
#include "bittide_gather.h"

// ============================================================================
// Event System Constants
// ============================================================================
// We encode event type in the top 8 bits, port in bits 48-51, and event time in bottom 48 bits
// This allows the priority queue to naturally order by time while preserving type and port
#define EVENT_TIME_MASK (0x0000ffffffffffffULL)
#define EVENT_PORT_MASK (0x000f000000000000ULL)
#define EVENT_PORT_SHIFT 48
#define EVENT_TYPE_SEND (0x0100000000000000ULL)
#define EVENT_TYPE_RECEIVE (0x0200000000000000ULL)
#define EVENT_TYPE_INVALIDATE (0x0400000000000000ULL)

// Magic numbers for protocol messages
#define BT_SENDING_UGN (0xf0f00001ULL)
#define BT_FOUND_UGN (0xf0f00002ULL)

// ============================================================================
// UGN Edge Structure
// ============================================================================
// Represents UGN (Universal Global Number) information for a network edge
typedef struct {
    uint32_t src_node;   // Source node ID
    uint32_t src_port;   // Source port number
    uint32_t dst_node;   // Destination node ID
    uint32_t dst_port;   // Destination port number
    int64_t ugn;         // The UGN value for this edge
    uint32_t is_valid;   // Whether this UGN entry is valid
} UgnEdge;

// ============================================================================
// UGN Protocol Context
// ============================================================================
// Holds all state needed for UGN discovery protocol
typedef struct {
    // Scatter and gather units (one per port)
    ScatterUnit* scatter_units;
    GatherUnit* gather_units;
    uint32_t num_ports;

    // UGN edge lists (dynamically sized based on max_degree)
    UgnEdge* incoming_link_ugn_list;
    UgnEdge* outgoing_link_ugn_list;
    uint32_t max_degree;  // Maximum number of neighbor nodes

    // Counters for protocol progress
    uint32_t number_outgoing_link_ugns_acknowledged;
    uint32_t number_incoming_link_ugns_known;
    uint32_t announced_done;

    // Node identification
    uint32_t node_id;
} UgnContext;

// ============================================================================
// Event Helper Functions
// ============================================================================

// Encode event type, port, and time into a single 64-bit value
static inline uint64_t ugn_encode_event(uint64_t event_type, uint64_t time) {
    return event_type | (time & EVENT_TIME_MASK);
}

// Encode event with port information (for SEND and RECEIVE events)
static inline uint64_t ugn_encode_event_with_port(uint64_t event_type, uint32_t port, uint64_t time) {
    return event_type | (((uint64_t)port << EVENT_PORT_SHIFT) & EVENT_PORT_MASK) | (time & EVENT_TIME_MASK);
}

// Extract event type from encoded event
static inline uint64_t get_event_type(uint64_t event) {
    return event & ~(EVENT_TIME_MASK | EVENT_PORT_MASK);
}

// Extract event time from encoded event
static inline uint64_t get_event_time(uint64_t event) {
    return event & EVENT_TIME_MASK;
}

// Extract port number from encoded event
static inline uint32_t get_event_port(uint64_t event) {
    return (uint32_t)((event & EVENT_PORT_MASK) >> EVENT_PORT_SHIFT);
}

// ============================================================================
// UGN Edge Helper Functions
// ============================================================================

// Initialize a UGN edge to invalid state
void ugn_edge_init(UgnEdge* edge);

// Set UGN edge information
void ugn_edge_set(UgnEdge* edge, uint32_t src_node, uint32_t src_port,
                  uint32_t dst_node, uint32_t dst_port, int64_t ugn);

// ============================================================================
// UGN Context Management
// ============================================================================

// Initialize UGN protocol context
void ugn_context_init(UgnContext* ctx, ScatterUnit* scatter_units,
                      GatherUnit* gather_units, uint32_t num_ports,
                      uint32_t node_id,
                      UgnEdge* incoming_list, UgnEdge* outgoing_list,
                      uint32_t max_degree);

// ============================================================================
// Protocol Event Handlers
// ============================================================================

// Send UGN to a specific port
void send_ugn_to_port(UgnContext* ctx, uint32_t port, uint32_t offset);

// Send UGNs to all ports
void send_ugns_to_all_ports(UgnContext* ctx, uint32_t offset);

// Check incoming buffer for a specific port
// Returns true if new data was received, false otherwise
bool check_incoming_buffer(UgnContext* ctx, uint32_t port, uint32_t offset);

// Check all incoming buffers for received UGNs
void check_all_incoming_buffers(UgnContext* ctx, uint32_t offset);

// Invalidate old scatter buffer data for a specific port
void invalidate_port(UgnContext* ctx, uint32_t port, uint32_t offset);

// Invalidate old scatter buffer data
void handle_invalidate(UgnContext* ctx, uint32_t offset);

#endif // BITTIDE_UGN_H
