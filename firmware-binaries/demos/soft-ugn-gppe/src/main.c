// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#include "bittide_timer.h"
#include "bittide_ugn.h"
#include "messages.h"
#include "peripherals.h"
#include "priority_queue.h"
#include <stdbool.h>
#include <stdint.h>

// ============================================================================
// Application Configuration
// ============================================================================

// Number of event loop iterations
#define NUM_PERIODS 5000000

// Protocol timing parameter
#define BUFFER_SIZE 4000

// SEND timing parameters
#define SEND_SPACING (BUFFER_SIZE * 3) // Spacing between SEND events
#define SEND_PERIOD (SEND_SPACING * NUM_PORTS)
#define FINAL_SEND_DELAY (2 * SEND_PERIOD + BUFFER_SIZE)

// RECEIVE timing parameters
#define RECEIVE_SPACING (BUFFER_SIZE * 3) // Spacing between RECEIVE events
#define RECEIVE_PERIOD (RECEIVE_SPACING * NUM_PORTS + 1)

// INVALIDATE timing parameters
#define INVALIDATE_DELAY BUFFER_SIZE

// Initial offsets for scheduling events
#define STARTING_DELAY_WRITE (BUFFER_SIZE * 1000)
#define STARTING_DELAY_READ (BUFFER_SIZE * 1002)

// ============================================================================
// Main Entry Point
// ============================================================================

int c_main(void) {
  // Initialize all peripherals
  Peripherals peripherals;
  peripherals_init(&peripherals);

//   dna_t dna_value;
//   dna_read(DNA_MAYBE_DNA, dna_value);

//   uart_puts(&uart, "DNA: ");
//   uart_putdna(&uart, dna_value);
//   uart_puts(&uart, "\n");

//   uart_puts(&uart, "Running on RISC-V\n");
//   uart_puts(&uart, "Hello from C!\n");

  // Verify assumptions
  ASSERT_PERIODS_COPRIME(&peripherals, SEND_PERIOD, RECEIVE_PERIOD);

  // Initialize UGN protocol context
  // TODO: Get actual node ID from hardware or configuration
  uint32_t node_id = peripherals.dna[0]; // Example: use first 32 bits of DNA as node ID

  // Allocate UGN edge lists
  UgnEdge incoming_link_ugn_list[NUM_PORTS];
  UgnEdge outgoing_link_ugn_list[NUM_PORTS];

  UgnContext ugn_ctx;
  ugn_context_init(&ugn_ctx, peripherals.scatter_units,
                   peripherals.gather_units, NUM_PORTS, node_id,
                   incoming_link_ugn_list, outgoing_link_ugn_list, NUM_PORTS);

  // Print consolidated initialization information
  PRINT_INIT_INFO(&peripherals, &ugn_ctx, BUFFER_SIZE, SEND_PERIOD,
                  RECEIVE_PERIOD, NUM_PORTS);

  // Event loop variables
  FixedIntPriorityQueue event_queue;

  // Schedule initial events (in cycles, aligned to metacycle boundaries)
  pq_init(&event_queue);

  // Get current time
  uint64_t start_cycles = timer_now_cycles(&peripherals.timer);
  uint64_t start_cycles_write = start_cycles + STARTING_DELAY_WRITE;
  uint64_t start_cycles_read = start_cycles + STARTING_DELAY_READ;

  // Schedule SEND and RECEIVE events
  for (uint32_t port = 0; port < NUM_PORTS; port++) {
    uint64_t send_time = start_cycles_write + port * SEND_SPACING;
    uint32_t send_offset = send_time % BUFFER_SIZE;

    uint64_t receive_time = start_cycles_read + port * RECEIVE_SPACING;
    uint32_t receive_offset = receive_time % BUFFER_SIZE;

    Event send_event = make_send_event(MSG_TYPE_ANNOUNCE, send_offset, port);
    Event receive_event = make_receive_event(receive_offset, port);

    pq_insert(&event_queue, encode_event(send_event), send_time);
    pq_insert(&event_queue, encode_event(receive_event), receive_time);
  }

  PRINT_EVENT_LOOP_START(&peripherals, &event_queue);

  // Main event loop - process events metacycle by metacycle
  uint64_t k;
  for (k = 0; k < NUM_PERIODS; k++) {

    // Check if there are events in the queue.
    if (pq_is_empty(&event_queue)) {
      return false; // Stop processing - no more events
    }

    // Check if queue is full
    if (pq_is_full(&event_queue)) {
      uart_puts(&peripherals.uart, MSG_QUEUE_FULL);
      return false; // Stop processing
    }

    // Extract the next event to process
    PriorityQueueItem item = pq_extract_min(&event_queue);
    uint64_t event_time = item.priority;
    Event current_event = decode_event(item.data, event_time);

    // Process the event
    if (current_event.type == EVENT_TYPE_SEND) {
        send_ugn_to_port(&ugn_ctx, &peripherals.timer, &current_event);
        UgnMessageType msg_type;
        // Schedule ACKNOWLEDGE instead of ANNOUNCE If we captured the incoming ugn
        if (ugn_ctx.incoming_link_ugn_list[current_event.port].is_valid) {
            msg_type = MSG_TYPE_ACKNOWLEDGE;
        } else {
            msg_type = MSG_TYPE_ANNOUNCE;
        }
        uint64_t next_event_time = event_time + SEND_PERIOD;
        Event next_send_event = make_send_event(msg_type, next_event_time, current_event.port);
        pq_insert(&event_queue, encode_event(next_send_event), next_event_time);

        // Schedule invalidate event
        uint64_t invalidate_time = event_time + INVALIDATE_DELAY;
        Event invalidate_event = make_invalidate_event(invalidate_time, current_event.port);
        pq_insert(&event_queue, encode_event(invalidate_event), invalidate_time);
    } else if (current_event.type == EVENT_TYPE_INVALIDATE) {

        // Invalidate the specific port and offset encoded in the event
        invalidate_port(&ugn_ctx, &peripherals.timer, &current_event);
    } else if (current_event.type == EVENT_TYPE_RECEIVE) {

        check_incoming_buffer(&ugn_ctx, &peripherals.timer, &current_event);

        // If we have both UGNs for this port, no need to schedule further RECEIVE events
        if (port_done(&ugn_ctx, current_event.port) == false) {
            // Reschedule next receive event
            uint64_t next_receive_time = event_time + RECEIVE_PERIOD;
            Event next_receive_event = make_receive_event(next_receive_time, current_event.port);
            pq_insert(&event_queue, encode_event(next_receive_event), next_receive_time);
        }
    }

    // Check if protocol is complete
    if (all_ports_done(&ugn_ctx)) {
      uart_puts(&peripherals.uart, MSG_PROTOCOL_COMPLETE);
      break; // Stop processing
    }
  }

  uart_puts(&peripherals.uart, "========================================\n");

  // Print completion statistics
  PRINT_COMPLETION_STATS(&peripherals, k, start_cycles, BUFFER_SIZE);

  // Print deadline miss statistics
  PRINT_DEADLINE_STATS(&peripherals, &ugn_ctx);

  // Print message count statistics
  PRINT_MESSAGE_COUNT_STATES(&peripherals, &ugn_ctx);

  // Print discovery results
  uart_puts(&peripherals.uart, "\nDiscovery Protocol Results:\n");
  uart_puts(&peripherals.uart, "----------------------------\n");

  // Print incoming and outgoing link UGNs
  PRINT_UGN_EDGE_LIST(&peripherals, "Incoming Link UGNs:\n",
                      ugn_ctx.incoming_link_ugn_list, ugn_ctx.num_ports);
  uart_puts(&peripherals.uart, "\n");
  PRINT_UGN_EDGE_LIST(&peripherals, "Outgoing Link UGNs:\n",
                      ugn_ctx.outgoing_link_ugn_list, ugn_ctx.num_ports);

  // Calculate and print roundtrip latencies
  PRINT_ROUNDTRIP_LATENCIES(&peripherals, &ugn_ctx);

  uart_puts(&peripherals.uart, "\n========================================\n");
  uart_puts(&peripherals.uart, "UGN discovery protocol complete!\n");

  while (1) {
    // Protocol complete - idle loop
  }
}
