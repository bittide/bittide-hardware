// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#include "hals/soft_ugn_demo_gppe/device_instances.h"

#include "bittide_dna.h"
#include "bittide_gather.h"  // Linter says we dont need this include, but we do
#include "bittide_scatter.h" // Linter says we dont need this include, but we do
#include "bittide_timer.h"
#include "bittide_ugn.h" // Requires the `bittide_scatter.h` and `bittide_gather.h`

#include "messages.h"
#include "priority_queue.h"
#include "ringbuffer_align.h"

#include <stdbool.h>
#include <stdint.h>

// ============================================================================
// Application Configuration
// ============================================================================

ScatterUnit su;
// Number of event loop iterations
#define NUM_PERIODS 10000
#define NUM_PORTS 7

// Protocol timing parameter
#define BUFFER_SIZE 1000

// SEND timing parameters
#define SEND_SPACING (10 * BUFFER_SIZE) // Spacing between SEND events
#define SEND_PERIOD (SEND_SPACING * NUM_PORTS)
#define FINAL_SEND_DELAY (2 * SEND_PERIOD + BUFFER_SIZE)

// RECEIVE timing parameters
#define RECEIVE_SPACING (9 * BUFFER_SIZE) // Spacing between RECEIVE events
#define RECEIVE_PERIOD (RECEIVE_SPACING * (2 * NUM_PORTS + 1))

// INVALIDATE timing parameters
#define INVALIDATE_DELAY BUFFER_SIZE

// Initial offsets for scheduling events
#define STARTING_DELAY_WRITE (BUFFER_SIZE * 100)
#define STARTING_DELAY_READ (BUFFER_SIZE * 10)

// ============================================================================
// Main Entry Point
// ============================================================================

int c_main(void) {
  // Initialize all peripherals

  Uart uart = hal.uart;
  ScatterUnit scatter_units[NUM_PORTS] = {
      hal.scatter_unit_0, hal.scatter_unit_1, hal.scatter_unit_2,
      hal.scatter_unit_3, hal.scatter_unit_4, hal.scatter_unit_5,
      hal.scatter_unit_6};

  GatherUnit gather_units[NUM_PORTS] = {hal.gather_unit_0, hal.gather_unit_1,
                                        hal.gather_unit_2, hal.gather_unit_3,
                                        hal.gather_unit_4, hal.gather_unit_5,
                                        hal.gather_unit_6};
  Timer timer = hal.timer;
  dna_t dna;
  dna_read(hal.dna, dna);

  // Verify assumptions
  ASSERT_PERIOD_BUFFER_MULTIPLE(uart, SEND_PERIOD, BUFFER_SIZE);
  ASSERT_PERIOD_BUFFER_MULTIPLE(uart, RECEIVE_PERIOD, BUFFER_SIZE);
  ASSERT_PERIODS_COPRIME(uart, SEND_PERIOD, RECEIVE_PERIOD, BUFFER_SIZE);

  // Initialize UGN protocol context
  // TODO: Get actual node ID from hardware or configuration
  uint32_t node_id = dna[0]; // Example: use first 32 bits of DNA as node ID

  // Allocate UGN edge lists
  UgnEdge incoming_link_ugn_list[NUM_PORTS];
  UgnEdge outgoing_link_ugn_list[NUM_PORTS];

  UgnContext ugn_ctx;
  ugn_context_init(&ugn_ctx, scatter_units, gather_units, NUM_PORTS, node_id,
                   incoming_link_ugn_list, outgoing_link_ugn_list, NUM_PORTS);

  // Print consolidated initialization information
  PRINT_INIT_INFO(uart, &ugn_ctx, BUFFER_SIZE, SEND_PERIOD, RECEIVE_PERIOD,
                  NUM_PORTS);

  // Align ringbuffers before starting event loop
  int16_t incoming_offsets[NUM_PORTS] = {0};
  align_ringbuffers(&ugn_ctx, incoming_offsets, uart);

  // Event loop variables
  FixedIntPriorityQueue event_queue;

  // Schedule initial events (in cycles, aligned to metacycle boundaries)
  pq_init(&event_queue);

  // Get current time
  uint64_t start_cycles = timer_now_cycles(timer);

  // Align start_cycles to start of buffer
  start_cycles = start_cycles + (BUFFER_SIZE - (start_cycles % BUFFER_SIZE));
  uint64_t start_cycles_write = start_cycles + STARTING_DELAY_WRITE;
  uint64_t start_cycles_read = start_cycles + STARTING_DELAY_READ;

  // Schedule SEND and RECEIVE events
  for (uint32_t port = 0; port < NUM_PORTS; port++) {
    uint64_t send_time = start_cycles_write + port * SEND_SPACING;
    uint32_t send_offset = send_time % BUFFER_SIZE;

    uint64_t receive_time =
        start_cycles_read + port * RECEIVE_SPACING + incoming_offsets[port];
    uint32_t receive_offset = receive_time % BUFFER_SIZE;

    Event send_event = make_send_event(MSG_TYPE_ANNOUNCE, send_offset, port);
    Event receive_event = make_receive_event(receive_offset, port);

    pq_insert(&event_queue, encode_event(send_event), send_time);
    pq_insert(&event_queue, encode_event(receive_event), receive_time);
  }

  PRINT_EVENT_LOOP_START(uart, &event_queue);

  // Main event loop - process events metacycle by metacycle
  uint64_t k;
  for (k = 0; k < NUM_PERIODS; k++) {

    // Check if there are events in the queue.
    if (pq_is_empty(&event_queue)) {
      return false; // Stop processing - no more events
    }

    // Check if queue is full
    if (pq_is_full(&event_queue)) {
      uart_puts(uart, MSG_QUEUE_FULL);
      return false; // Stop processing
    }

    // Extract the next event to process
    PriorityQueueItem item = pq_extract_min(&event_queue);
    uint64_t event_time = item.priority;
    Event current_event = decode_event(item.data, event_time);

    // Process the event
    if (current_event.type == EVENT_TYPE_SEND) {
      send_ugn_to_port(&ugn_ctx, timer, &current_event);
      UgnMessageType msg_type;
      // Schedule ACKNOWLEDGE instead of ANNOUNCE If we captured the incoming
      // ugn
      if (ugn_ctx.incoming_link_ugn_list[current_event.port].is_valid) {
        msg_type = MSG_TYPE_ACKNOWLEDGE;
      } else {
        msg_type = MSG_TYPE_ANNOUNCE;
      }
      uint64_t next_event_time = event_time + SEND_PERIOD;
      Event next_send_event =
          make_send_event(msg_type, next_event_time, current_event.port);
      pq_insert(&event_queue, encode_event(next_send_event), next_event_time);

      // Schedule invalidate event
      uint64_t invalidate_time = event_time + INVALIDATE_DELAY;
      Event invalidate_event =
          make_invalidate_event(invalidate_time, current_event.port);
      pq_insert(&event_queue, encode_event(invalidate_event), invalidate_time);
    } else if (current_event.type == EVENT_TYPE_INVALIDATE) {

      // Invalidate the specific port and offset encoded in the event
      invalidate_port(&ugn_ctx, timer, &current_event);
    } else if (current_event.type == EVENT_TYPE_RECEIVE) {

      check_incoming_buffer(&ugn_ctx, timer, &current_event);

      // If we have both UGNs for this port, no need to schedule further RECEIVE
      // events
      if (port_done(&ugn_ctx, current_event.port) == false) {
        // Reschedule next receive event
        uint64_t next_receive_time = event_time + RECEIVE_PERIOD;
        Event next_receive_event =
            make_receive_event(next_receive_time, current_event.port);
        pq_insert(&event_queue, encode_event(next_receive_event),
                  next_receive_time);
      }
    }
  }

  uart_puts(uart, "========================================\n");

  // Print completion statistics
  PRINT_COMPLETION_STATS(uart, timer, k, start_cycles);

  // Print deadline miss statistics
  PRINT_DEADLINE_STATS(uart, &ugn_ctx);

  // Print message count statistics
  PRINT_MESSAGE_COUNT_STATES(uart, &ugn_ctx);

  // Print discovery results
  uart_puts(uart, "\nDiscovery Protocol Results:\n");
  uart_puts(uart, "----------------------------\n");

  // Print incoming and outgoing link UGNs
  PRINT_UGN_EDGE_LIST(uart, "Incoming Link UGNs:\n",
                      ugn_ctx.incoming_link_ugn_list, ugn_ctx.num_ports);
  PRINT_UGN_EDGE_LIST(uart, "Outgoing Link UGNs:\n",
                      ugn_ctx.outgoing_link_ugn_list, ugn_ctx.num_ports);
  uart_puts(uart, "End of UGN Edge edges\n");

  // Calculate and print roundtrip latencies
  PRINT_ROUNDTRIP_LATENCIES(uart, &ugn_ctx);

  uart_puts(uart, "\n========================================\n");
  uart_puts(uart, "UGN discovery protocol complete!\n");

  while (1) {
    // Protocol complete - idle loop
  }
}
