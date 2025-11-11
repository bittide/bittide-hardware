// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#include <stdbool.h>
#include "peripherals.h"
#include "bittide_ugn.h"
#include "priority_queue.h"
#include "messages.h"

// ============================================================================
// Application Configuration
// ============================================================================

// Number of event loop iterations
#define NUM_PERIODS 5000

// Maximum number of nodes including myself (degree of the network)
#define MAXDEG 9

// Protocol timing parameter
#define METACYCLE_CLOCKS 2000
// Schedule one SEND task per neighbor every 3 metacycles
#define SEND_PERIOD (METACYCLE_CLOCKS * 3)
// Schedule one RECEIVE task per link each metacycle
#define RECEIVE_PERIOD METACYCLE_CLOCKS
#define MAX_SEND_PRIORITY_OVERRIDE 5000                    // Max delay before RECEIVE overrides SEND priority
// Invalidate 2 metacycles after send
#define INVALIDATE_DELAY (METACYCLE_CLOCKS * 2)

// Initial offsets for starting events in metacycles
// Increased to allow time for startup printing (UART is slow)
#define STARTING_TICK_SND_OFFSET (METACYCLE_CLOCKS * 1000)
#define STARTING_TICK_REC_OFFSET (METACYCLE_CLOCKS * 1001)

bool find_min_send_event(const FixedIntPriorityQueue* pq, uint32_t* found_index) {
  bool found = false;
  uint64_t min_value;
  uint32_t min_index;
  uint64_t etime;
  for (uint32_t i = 0; i < pq->size; i++) {
    etime = pq->items[i] & EVENT_TIME_MASK;
    if (pq->items[i] & EVENT_TYPE_SEND) {
      if (found) {
        if (etime < min_value) {
          min_value = etime;
          min_index = i;
        }
      } else {
        found = true;
        min_value = etime;
        min_index = i;
      }
    }
  }
  if (found) {
    *found_index = min_index;
    return true;
  }
  return false;
}


// ============================================================================
// Event Processing Functions
// ============================================================================

// Process a single event and schedule any follow-up events
static void process_event(uint64_t event, UgnContext* ugn_ctx, FixedIntPriorityQueue* event_queue) {
    uint64_t event_time = get_event_time(event);
    uint64_t metacycle_offset = event_time % METACYCLE_CLOCKS;
    uint32_t port = get_event_port(event);

    if (event & EVENT_TYPE_SEND) {
        // Send UGN to the specific port encoded in the event
        send_ugn_to_port(ugn_ctx, port, metacycle_offset);
        // Schedule next send event for this port
        pq_insert(event_queue, ugn_encode_event_with_port(EVENT_TYPE_SEND, port, event_time + SEND_PERIOD));
        // Schedule invalidate for this port 2 metacycles after this send
        pq_insert(event_queue, ugn_encode_event_with_port(EVENT_TYPE_INVALIDATE, port, event_time + INVALIDATE_DELAY));

    } else if (event & EVENT_TYPE_INVALIDATE) {
        // Invalidate the specific port encoded in the event
        invalidate_port(ugn_ctx, port, metacycle_offset);

    } else if (event & EVENT_TYPE_RECEIVE) {
        // Check incoming buffer for the specific port encoded in the event
        check_incoming_buffer(ugn_ctx, port, metacycle_offset);
        // Schedule next receive event for this port
        pq_insert(event_queue, ugn_encode_event_with_port(EVENT_TYPE_RECEIVE, port, event_time + RECEIVE_PERIOD));
    }
}

// Process all events in a single metacycle
// Assumes: timer scratchpad is already set to metacycle_end for deadline checking
// Returns true if processing should continue, false if we should stop
static bool process_metacycle(
    uint64_t metacycle_start,
    uint64_t* current_event,
    Timer* timer,
    Uart* uart,
    UgnContext* ugn_ctx,
    FixedIntPriorityQueue* event_queue
) {
    // Process all events scheduled in this metacycle
    bool on_time = true;
    while (on_time) {

        // Process the current event
        process_event(*current_event, ugn_ctx, event_queue);

        // Check if protocol is complete
        if (ugn_ctx->number_incoming_link_ugns_known == ugn_ctx->num_ports &&
            ugn_ctx->number_outgoing_link_ugns_acknowledged == ugn_ctx->num_ports) {
            uart_puts(uart, MSG_PROTOCOL_COMPLETE);
            return false;  // Stop processing
        }

        // Check if there are more events in the queue
        if (pq_is_empty(event_queue)) {
            return false;  // Stop processing - no more events
        }

        // Check if queue is full
        if (pq_is_full(event_queue)) {
            uart_puts(uart, MSG_QUEUE_FULL);
            return false;  // Stop processing
        }

        // Peek at the next event
        uint64_t next_event = pq_peek_min(event_queue);
        uint64_t next_event_time = get_event_time(next_event);
        uint64_t next_metacycle_start = next_event_time - (next_event_time % METACYCLE_CLOCKS);

        // If next event is in a different metacycle, exit without removing it
        if (next_metacycle_start != metacycle_start) {
            return true;  // Continue processing in next metacycle
        }

        // Otherwise, extract and process this event in the current metacycle
        *current_event = pq_extract_min(event_queue);
        CompareResult cmp_result = get_compare_result(timer);
        on_time = cmp_result == COMPARE_LESS;

        // Report deadline miss (but continue processing)
        if (!on_time) {
            uart_puts(uart, MSG_DEADLINE_MISS);
        }
    }

    // If we exit the loop because on_time became false, continue to next metacycle
    return true;
}

// ============================================================================
// Main Entry Point
// ============================================================================

int c_main(void) {
    // Initialize all peripherals
    Peripherals peripherals;
    peripherals_init(&peripherals);

    // Get current metacycle count and convert to cycles (aligned to metacycle boundary)
    scatter_unit_wait_for_new_metacycle(&peripherals.scatter_units[0]);
    uint32_t start_metacycle = scatter_unit_metacycle_count(&peripherals.scatter_units[0]) + 1;
    uint64_t start_cycles = (uint64_t)start_metacycle * METACYCLE_CLOCKS;

    // Initialize UGN protocol context
    // TODO: Get actual node ID from hardware or configuration
    uint32_t node_id = 0xDEADBEEF;  // Placeholder

    // Allocate UGN edge lists
    UgnEdge incoming_link_ugn_list[MAXDEG];
    UgnEdge outgoing_link_ugn_list[MAXDEG];

    UgnContext ugn_ctx;
    ugn_context_init(&ugn_ctx, peripherals.scatter_units, peripherals.gather_units,
                     NUM_PORTS, node_id,
                     incoming_link_ugn_list, outgoing_link_ugn_list, MAXDEG);

    // Print consolidated initialization information
    PRINT_INIT_INFO(&peripherals, &ugn_ctx, METACYCLE_CLOCKS, SEND_PERIOD, RECEIVE_PERIOD, MAXDEG);

    // Event loop variables
    FixedIntPriorityQueue event_queue;

    // Schedule initial events (in cycles, aligned to metacycle boundaries)
    pq_init(&event_queue);

    // Schedule SEND events: one per port every 3 metacycles, staggered across ports
    for (uint32_t port = 0; port < MAXDEG; port++) {
        pq_insert(&event_queue, ugn_encode_event_with_port(EVENT_TYPE_SEND, port,
            start_cycles + STARTING_TICK_SND_OFFSET + port * SEND_PERIOD));
    }

    // Schedule RECEIVE events: one per port each metacycle, staggered across the first MAXDEG metacycles
    for (uint32_t port = 0; port < MAXDEG; port++) {
        pq_insert(&event_queue, ugn_encode_event_with_port(EVENT_TYPE_RECEIVE, port,
            start_cycles + STARTING_TICK_REC_OFFSET + port * RECEIVE_PERIOD));
    }

    // Get the first event before starting the loop
    if (pq_is_empty(&event_queue)) {
        uart_puts(&peripherals.uart, MSG_QUEUE_EMPTY);
        return 0;
    }
    uint64_t current_event = pq_extract_min(&event_queue);

    PRINT_EVENT_LOOP_START(&peripherals, &event_queue);

    // Track progress (print every 1000 iterations to avoid UART overhead)
    uint32_t last_progress_report = 0;
    const uint32_t PROGRESS_INTERVAL = 1000;

    // Main event loop - process events metacycle by metacycle
    uint32_t k;
    for (k = 0; k < NUM_PERIODS; k++) {
        uint64_t event_time = get_event_time(current_event);

        // Determine the metacycle this event belongs to
        uint64_t metacycle_offset = event_time % METACYCLE_CLOCKS;
        uint64_t metacycle_start = event_time - metacycle_offset;
        uint64_t metacycle_end = metacycle_start + METACYCLE_CLOCKS - 1;

        // Wait until the start of this metacycle
        WaitResult wait_result = timer_wait_until_cycles(&peripherals.timer, metacycle_start);

        if (wait_result == WAIT_ALREADY_PASSED) {
            PRINT_MISSED_METACYCLE(&peripherals, metacycle_start, METACYCLE_CLOCKS);
        }

        // Set the scratchpad to the end of this metacycle for deadline checking
        *peripherals.timer.scratchpad = metacycle_end;

        // Process all events in this metacycle
        bool continue_processing = process_metacycle(
            metacycle_start,
            &current_event,
            &peripherals.timer,
            &peripherals.uart,
            &ugn_ctx,
            &event_queue
        );

        if (!continue_processing) {
            break;
        }

        // Periodic progress update (to avoid excessive UART traffic)
        if (k > 0 && (k - last_progress_report) >= PROGRESS_INTERVAL) {
            PRINT_PROTOCOL_PROGRESS(&peripherals, &ugn_ctx);
            last_progress_report = k;
        }

        // Get the next event for the next iteration
        if (pq_is_empty(&event_queue)) {
            break;
        }
        current_event = pq_extract_min(&event_queue);
    }

    uart_puts(&peripherals.uart, "========================================\n");

    // Print completion statistics
    PRINT_COMPLETION_STATS(&peripherals, k, start_cycles, METACYCLE_CLOCKS);

    // Print discovery results
    uart_puts(&peripherals.uart, "\nDiscovery Protocol Results:\n");
    uart_puts(&peripherals.uart, "----------------------------\n");
    PRINT_UGN_SUMMARY(&peripherals, &ugn_ctx);

    // Print incoming and outgoing link UGNs
    PRINT_UGN_EDGE_LIST(&peripherals, "Incoming Link UGNs:\n", ugn_ctx.incoming_link_ugn_list, ugn_ctx.num_ports);
    uart_puts(&peripherals.uart, "\n");
    PRINT_UGN_EDGE_LIST(&peripherals, "Outgoing Link UGNs:\n", ugn_ctx.outgoing_link_ugn_list, ugn_ctx.num_ports);

    uart_puts(&peripherals.uart, "\n========================================\n");
    uart_puts(&peripherals.uart, "UGN discovery protocol complete!\n");

    while (1) {
        // Protocol complete - idle loop
    }
}
