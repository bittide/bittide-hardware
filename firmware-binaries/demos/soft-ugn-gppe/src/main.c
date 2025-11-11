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
#define MAXDEG 8

// Protocol timing parameter
#define METACYCLE_CLOCKS 2000
#define SEND_PERIOD (METACYCLE_CLOCKS * MAXDEG)            // How often to send UGN broadcasts
#define RECEIVE_PERIOD (METACYCLE_CLOCKS * MAXDEG + 1)     // How often to check for incoming UGNs
#define MAX_SEND_PRIORITY_OVERRIDE 5000                    // Max delay before RECEIVE overrides SEND priority

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

uint64_t pq_extract_min_send_first(FixedIntPriorityQueue* pq) {
  uint32_t min_send_index;
  uint32_t min_all_index;
  uint32_t extract_index;
  uint64_t item_out;
  bool has_send_event = find_min_send_event(pq, &min_send_index);
  if (has_send_event) {
    min_all_index = find_min_index(pq);
    extract_index = min_send_index;
    if (min_all_index < min_send_index - MAX_SEND_PRIORITY_OVERRIDE) {
      extract_index = min_all_index;
    }
    item_out = pq->items[extract_index];
    // Replace the extracted item with the last item in the array
    pq->items[extract_index] = pq->items[pq->size - 1];
    pq->size--;  // Decrement size
    return item_out;
  }
  return pq_extract_min(pq);
}

// ============================================================================
// Event Processing Functions
// ============================================================================

// Process a single event and schedule any follow-up events
static void process_event(uint64_t event, UgnContext* ugn_ctx, FixedIntPriorityQueue* event_queue) {
    uint64_t event_time = get_event_time(event);

    if (event & EVENT_TYPE_SEND) {
        send_ugns_to_all_ports(ugn_ctx, event_time);
        // Schedule next send and invalidate events
        pq_insert(event_queue, ugn_encode_event(EVENT_TYPE_SEND, event_time + SEND_PERIOD));
        pq_insert(event_queue, ugn_encode_event(EVENT_TYPE_INVALIDATE, event_time));

    } else if (event & EVENT_TYPE_INVALIDATE) {
        handle_invalidate(ugn_ctx, event_time);

    } else if (event & EVENT_TYPE_RECEIVE) {
        check_all_incoming_buffers(ugn_ctx, event_time);
        // Schedule next receive event
        pq_insert(event_queue, ugn_encode_event(EVENT_TYPE_RECEIVE, event_time + RECEIVE_PERIOD));
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
        *current_event = pq_extract_min_send_first(event_queue);
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

    uart_puts(&peripherals.uart, "========================================\n");
    uart_puts(&peripherals.uart, "Bittide UGN Discovery Protocol\n");
    uart_puts(&peripherals.uart, "========================================\n");
    uart_puts(&peripherals.uart, "Config: METACYCLE_CLOCKS=");
    uart_putdec(&peripherals.uart, METACYCLE_CLOCKS);
    uart_puts(&peripherals.uart, ", MAXDEG=");
    uart_putdec(&peripherals.uart, MAXDEG);
    uart_puts(&peripherals.uart, "\n");
    uart_puts(&peripherals.uart, "Initialized scatter/gather units\n");

    // Get current metacycle count and convert to cycles (aligned to metacycle boundary)
    scatter_unit_wait_for_new_metacycle(&peripherals.scatter_units[0]);
    uint32_t start_metacycle = scatter_unit_metacycle_count(&peripherals.scatter_units[0]) + 1;
    uint64_t start_cycles = (uint64_t)start_metacycle * METACYCLE_CLOCKS;

    PRINT_START_INFO(&peripherals, METACYCLE_CLOCKS);

    // Initialize UGN protocol context
    // TODO: Get actual node ID from hardware or configuration
    uint32_t node_id = 0;  // Placeholder

    // Allocate UGN edge lists
    UgnEdge incoming_link_ugn_list[MAXDEG];
    UgnEdge outgoing_link_ugn_list[MAXDEG];

    UgnContext ugn_ctx;
    ugn_context_init(&ugn_ctx, peripherals.scatter_units, peripherals.gather_units,
                     NUM_PORTS, node_id,
                     incoming_link_ugn_list, outgoing_link_ugn_list, MAXDEG);

    uart_puts(&peripherals.uart, "Initialized UGN protocol context\n");
    uart_puts(&peripherals.uart, "Node ID: ");
    uart_putdec(&peripherals.uart, node_id);
    uart_puts(&peripherals.uart, ", Ports: ");
    uart_putdec(&peripherals.uart, NUM_PORTS);
    uart_puts(&peripherals.uart, "\n");
    uart_puts(&peripherals.uart, "Starting UGN discovery protocol...\n");

    // Event loop variables
    FixedIntPriorityQueue event_queue;

    // Schedule initial events (in cycles, aligned to metacycle boundaries)
    pq_init(&event_queue);

    for (uint32_t degree = 0; degree < MAXDEG; degree++) {
        pq_insert(&event_queue, ugn_encode_event(EVENT_TYPE_SEND, start_cycles + STARTING_TICK_SND_OFFSET + degree * METACYCLE_CLOCKS));
        pq_insert(&event_queue, ugn_encode_event(EVENT_TYPE_RECEIVE, start_cycles + STARTING_TICK_REC_OFFSET + degree * METACYCLE_CLOCKS));
    }

    PRINT_QUEUE_STATS(&peripherals, &event_queue);

    // Get the first event before starting the loop
    if (pq_is_empty(&event_queue)) {
        uart_puts(&peripherals.uart, MSG_QUEUE_EMPTY);
        return 0;
    }
    uint64_t current_event = pq_extract_min_send_first(&event_queue);

    // Print pre-loop information about what we're about to do
    uart_puts(&peripherals.uart, "\n========================================\n");
    uart_puts(&peripherals.uart, "Starting Real-Time Event Loop\n");
    uart_puts(&peripherals.uart, "========================================\n");

    PRINT_FIRST_EVENT_INFO(&peripherals, current_event, METACYCLE_CLOCKS);
    PRINT_LOOP_CONFIG(&peripherals, NUM_PERIODS, SEND_PERIOD, RECEIVE_PERIOD, METACYCLE_CLOCKS);
    PRINT_PROTOCOL_GOALS(&peripherals, &ugn_ctx);

    uart_puts(&peripherals.uart, "\nWaiting for first event...\n");
    uart_puts(&peripherals.uart, "========================================\n\n");

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
        current_event = pq_extract_min_send_first(&event_queue);
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
