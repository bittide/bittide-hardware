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
// We encode event type in the top 8 bits and port in bits 48-51
// Event time is stored separately in the priority queue's priority field
#define EVENT_PORT_MASK (0x000f000000000000ULL)
#define EVENT_PORT_SHIFT 48
#define EVENT_TYPE_SEND (0x0100000000000000ULL)
#define EVENT_TYPE_RECEIVE (0x0200000000000000ULL)
#define EVENT_TYPE_INVALIDATE (0x0400000000000000ULL)

// Magic numbers for protocol messages
#define UGN_MSG_ANNOUNCE (0xf0f00001ULL)      // Announcing presence/timing
#define UGN_MSG_ACKNOWLEDGE (0xf0f00002ULL)   // Acknowledging neighbor's announcement

// Legacy aliases (for compatibility during transition)
#define BT_SENDING_UGN UGN_MSG_ANNOUNCE
#define BT_FOUND_UGN UGN_MSG_ACKNOWLEDGE

// ============================================================================
// UGN Edge Structure
// ============================================================================
// Represents UGN (Uninterpretable Garbage Number) information for a network edge
// Stores delay measurement (direction-specific):
//   - For incoming edges: stores receive_delay (neighbor -> us)
//   - For outgoing edges: stores send_delay (us -> neighbor)
typedef struct {
    uint32_t src_node;           // Source node ID
    uint32_t src_port;           // Source port number
    uint32_t dst_node;           // Destination node ID
    uint32_t dst_port;           // Destination port number
    int64_t ugn;                 // Propagation delay in clock cycles (direction depends on edge type)
    bool is_valid;           // Whether this UGN entry is valid
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

    // Node identification
    uint32_t node_id;
} UgnContext;

typedef enum {
    MSG_TYPE_ANNOUNCE = 0xf0f00001ULL,      // Announcing presence/timing
    MSG_TYPE_ACKNOWLEDGE = 0xf0f00002ULL    // Acknowledging neighbor's announcement
} UgnMessageType;

// Message structure (logical view)
typedef struct {
    UgnMessageType type;
    uint64_t local_counter;    // Sender's local time (event_time)
    uint64_t remote_counter;   // For ACK: sender_time from incoming edge; For ANNOUNCE: 0
    uint32_t node_id;
    uint32_t port;
} UgnMessage;

// ============================================================================
// Event Helper Functions
// ============================================================================

// Encode event with port information (for SEND and RECEIVE events)
// Time is passed separately to the priority queue
static inline uint64_t ugn_encode_event(uint64_t event_type, uint32_t port) {
    return event_type | (((uint64_t)port << EVENT_PORT_SHIFT) & EVENT_PORT_MASK);
}

// Extract event type from encoded event
static inline uint64_t get_event_type(uint64_t event) {
    return event & ~EVENT_PORT_MASK;
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

// ============================================================================
// UGN Context Management
// ============================================================================

// Initialize UGN protocol context
void ugn_context_init(UgnContext* ctx, ScatterUnit* scatter_units,
                      GatherUnit* gather_units, uint32_t num_ports,
                      uint32_t node_id,
                      UgnEdge* incoming_list, UgnEdge* outgoing_list,
                      uint32_t max_degree);

bool port_done(UgnContext* ctx, uint32_t port);

bool all_ports_done(UgnContext* ctx);

// ============================================================================
// Buffer Offset Calculation
// ============================================================================
// These functions centralize the event_time -> buffer_offset translation logic.
// Future changes to the offset calculation (e.g., for non-trivial calendar
// configurations) only need to be updated here.

// Calculate scatter buffer offset for a given event time and port
static inline uint32_t ugn_calculate_scatter_offset(const UgnContext* ctx, uint32_t port, uint64_t event_time) {
    return (uint32_t)(event_time % ctx->scatter_units[port].memory_len);
}

// Calculate gather buffer offset for a given event time and port
static inline uint32_t ugn_calculate_gather_offset(const UgnContext* ctx, uint32_t port, uint64_t event_time) {
    return (uint32_t)(event_time % ctx->gather_units[port].memory_len);
}

// ============================================================================
// Protocol Event Handlers
// ============================================================================
// Note: These functions take event_time (uint64_t timestamp in clock cycles)
// and internally compute the buffer offset via: offset = event_time % memory_len

// Send UGN to a specific port
// @param event_time: Current time in clock cycles (used for message payload and buffer offset)
void send_ugn_to_port(UgnContext* ctx, uint32_t port, uint64_t event_time);

// Check incoming buffer for a specific port
// @param event_time: Current time in clock cycles (used for UGN calculation and buffer offset)
// Returns true if new data was received, false otherwise
bool check_incoming_buffer(UgnContext* ctx, uint32_t port, uint64_t event_time);

// Invalidate old scatter buffer data for a specific port
// @param event_time: Current time in clock cycles (used to compute buffer offset)
void invalidate_port(UgnContext* ctx, uint32_t port, uint64_t event_time);

#endif // BITTIDE_UGN_H
