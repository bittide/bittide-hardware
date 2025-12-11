// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#ifndef BITTIDE_UGN_H
#define BITTIDE_UGN_H

#include "hals/soft_ugn_demo_gppe/device_instances.h"

#include "bittide_gather.h"
#include "bittide_scatter.h"
#include "bittide_timer.h"
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

static inline char *show_event_type(EventType type) {
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

static inline char *show_msg_type(UgnMessageType type) {
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
void ugn_edge_init(UgnEdge *edge);

// ============================================================================
// UGN Context Management
// ============================================================================

// Initialize UGN protocol context
void ugn_context_init(UgnContext *ctx, ScatterUnit *scatter_units,
                      GatherUnit *gather_units, uint32_t num_ports,
                      uint32_t node_id, UgnEdge *incoming_list,
                      UgnEdge *outgoing_list, uint32_t max_degree);

bool port_done(UgnContext *ctx, uint32_t port);

bool all_ports_done(UgnContext *ctx);

// ============================================================================
// Buffer Offset Calculation
// ============================================================================
// These functions centralize the event_time -> buffer_offset translation logic.
// Future changes to the offset calculation (e.g., for non-trivial calendar
// configurations) only need to be updated here.

// Calculate scatter buffer offset for a given event time and port
static inline uint32_t ugn_calculate_scatter_offset(uint64_t event_time) {
  return (uint32_t)(event_time % SCATTER_UNIT_SCATTER_MEMORY_LEN);
}

// Calculate gather buffer offset for a given event time and port
static inline uint32_t ugn_calculate_gather_offset(uint64_t event_time) {
  return (uint32_t)(event_time % GATHER_UNIT_GATHER_MEMORY_LEN);
}

// ============================================================================
// Protocol Event Handlers
// ============================================================================
// Note: These functions take event_time (uint64_t timestamp in clock cycles)
// and internally compute the buffer offset via: offset = event_time %
// memory_len

// Send UGN to a specific port
// @param event_time: Current time in clock cycles (used for message payload and
// buffer offset)
bool send_ugn_to_port(UgnContext *ctx, Timer timer, Event *event);

// Check incoming buffer for a specific port
// @param event_time: Current time in clock cycles (used for UGN calculation and
// buffer offset) Returns true if new data was received, false otherwise
bool check_incoming_buffer(UgnContext *ctx, Timer timer, Event *event);

// Invalidate old scatter buffer data for a specific port
// @param event_time: Current time in clock cycles (used to compute buffer
// offset)
void invalidate_port(UgnContext *ctx, Timer timer, Event *event);

#endif // BITTIDE_UGN_H
