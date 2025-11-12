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

// Progress reporting interval (print progress every N iterations)
#define PROGRESS_INTERVAL 1000

// Protocol timing parameter
#define METACYCLE_CLOCKS 2000
// Schedule one SEND task per neighbor every 3 metacycles
#define SEND_PERIOD (METACYCLE_CLOCKS * 3 * NUM_PORTS)
// Schedule one RECEIVE task per link each metacycle
#define RECEIVE_PERIOD (METACYCLE_CLOCKS * NUM_PORTS)
#define MAX_SEND_PRIORITY_OVERRIDE 5000                    // Max delay before RECEIVE overrides SEND priority
// Invalidate 2 metacycles after send
#define INVALIDATE_DELAY (METACYCLE_CLOCKS * 2)

// Initial offsets for starting events in metacycles
// Increased to allow time for startup printing (UART is slow)
#define STARTING_TICK_SND_OFFSET (METACYCLE_CLOCKS * 1000)
#define STARTING_TICK_REC_OFFSET (METACYCLE_CLOCKS * 1001)

// ============================================================================
// Global Variables for Deadline Miss Tracking
// ============================================================================

static uint32_t missed_send_count = 0;
static uint32_t missed_receive_count = 0;
static uint32_t missed_invalidate_count = 0;

static uint32_t met_send_count = 0;
static uint32_t met_receive_count = 0;
static uint32_t met_invalidate_count = 0;

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
// Returns true if event should be executed, false if it should be skipped (deadline missed)
static bool process_event(uint64_t event, UgnContext* ugn_ctx, FixedIntPriorityQueue* event_queue,
                          bool execute, Peripherals* peripherals) {
    uint64_t event_time = get_event_time(event);
    uint64_t metacycle_offset = event_time % METACYCLE_CLOCKS;
    uint32_t port = get_event_port(event);

    if (event & EVENT_TYPE_SEND) {
        if (execute) {
            // Send UGN to the specific port encoded in the event
            send_ugn_to_port(ugn_ctx, port, metacycle_offset);
            met_send_count++;
        } else {
            missed_send_count++;
        }
        // Always reschedule next send event for this port
        pq_insert(event_queue, ugn_encode_event_with_port(EVENT_TYPE_SEND, port, event_time + SEND_PERIOD));
        // Schedule invalidate for this port 2 metacycles after this send
        pq_insert(event_queue, ugn_encode_event_with_port(EVENT_TYPE_INVALIDATE, port, event_time + INVALIDATE_DELAY));

    } else if (event & EVENT_TYPE_INVALIDATE) {
        if (execute) {
            // Invalidate the specific port encoded in the event
            invalidate_port(ugn_ctx, port, metacycle_offset);
            met_invalidate_count++;
        } else {
            missed_invalidate_count++;
        }

    } else if (event & EVENT_TYPE_RECEIVE) {
        if (execute) {
            // Check incoming buffer for the specific port encoded in the event
            bool received = check_incoming_buffer(ugn_ctx, port, metacycle_offset);

            // If we received new data, print all discovered edges
            if (received) {
                PRINT_UGN_EDGE_LIST(peripherals, "Incoming Link UGNs:\n",
                                    ugn_ctx->incoming_link_ugn_list, ugn_ctx->num_ports);
                uart_puts(&peripherals->uart, "\n");
                PRINT_UGN_EDGE_LIST(peripherals, "Outgoing Link UGNs:\n",
                                    ugn_ctx->outgoing_link_ugn_list, ugn_ctx->num_ports);
                uart_puts(&peripherals->uart, "\n");
            }
            met_receive_count++;
        } else {
            missed_receive_count++;
        }
        // Always reschedule next receive event for this port
        pq_insert(event_queue, ugn_encode_event_with_port(EVENT_TYPE_RECEIVE, port, event_time + RECEIVE_PERIOD));
    }

    return execute;
}
// Process all events in a single metacycle
// Extracts the next event from the queue, waits for its metacycle, and processes all events in that metacycle
// Returns true if processing should continue, false if we should stop
static bool process_metacycle(
    Timer* timer,
    Uart* uart,
    UgnContext* ugn_ctx,
    FixedIntPriorityQueue* event_queue,
    Peripherals* peripherals
) {
    // Check if there are events to process
    if (pq_is_empty(event_queue)) {
        return false;  // Stop processing - no more events
    }

    // Check if queue is full
    if (pq_is_full(event_queue)) {
        uart_puts(uart, MSG_QUEUE_FULL);
        return false;  // Stop processing
    }

    // Extract the next event to process
    uint64_t current_event = pq_extract_min(event_queue);
    uint64_t event_time = get_event_time(current_event);

    // Determine the metacycle this event belongs to
    uint64_t metacycle_offset = event_time % METACYCLE_CLOCKS;
    uint64_t metacycle_start = event_time - metacycle_offset;
    uint64_t metacycle_end = metacycle_start + METACYCLE_CLOCKS - 1;

    // Wait until the start of this metacycle
    WaitResult wait_result = timer_wait_until_cycles(timer, metacycle_start);

    if (wait_result == WAIT_ALREADY_PASSED) {
        PRINT_MISSED_METACYCLE(peripherals, metacycle_start, METACYCLE_CLOCKS);
    }

    // Set the scratchpad to the end of this metacycle for deadline checking
    *timer->scratchpad = metacycle_end;

    // Process all events scheduled in this metacycle
    bool on_time = true;

    // Process the first event (already extracted)
    process_event(current_event, ugn_ctx, event_queue, on_time, peripherals);

    while (true) {
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

        // If next event is in a different metacycle, exit without removing it
        if (next_event_time > metacycle_end) {
            return true;  // Continue processing in next metacycle
        }

        // Otherwise, extract this event to process in the current metacycle
        current_event = pq_extract_min(event_queue);

        // Check if we're still on time
        CompareResult cmp_result = get_compare_result(timer);
        on_time = cmp_result == COMPARE_LESS;

        // Process the event (skip execution if deadline missed)
        process_event(current_event, ugn_ctx, event_queue, on_time, peripherals);
    }

    // Should never reach here
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
    UgnEdge incoming_link_ugn_list[NUM_PORTS];
    UgnEdge outgoing_link_ugn_list[NUM_PORTS];

    UgnContext ugn_ctx;
    ugn_context_init(&ugn_ctx, peripherals.scatter_units, peripherals.gather_units,
                     NUM_PORTS, node_id,
                     incoming_link_ugn_list, outgoing_link_ugn_list, NUM_PORTS);

    // Print consolidated initialization information
    PRINT_INIT_INFO(&peripherals, &ugn_ctx, METACYCLE_CLOCKS, SEND_PERIOD, RECEIVE_PERIOD, NUM_PORTS);

    // Event loop variables
    FixedIntPriorityQueue event_queue;

    // Schedule initial events (in cycles, aligned to metacycle boundaries)
    pq_init(&event_queue);

    // Schedule SEND events: one per port every 3 metacycles, staggered across ports
    for (uint32_t port = 0; port < NUM_PORTS; port++) {
        pq_insert(&event_queue, ugn_encode_event_with_port(EVENT_TYPE_SEND, port,
            start_cycles + STARTING_TICK_SND_OFFSET + port * SEND_PERIOD));
    }

    // Schedule RECEIVE events: one per port each metacycle, staggered across the first NUM_PORTS metacycles
    for (uint32_t port = 0; port < NUM_PORTS; port++) {
        pq_insert(&event_queue, ugn_encode_event_with_port(EVENT_TYPE_RECEIVE, port,
            start_cycles + STARTING_TICK_REC_OFFSET + port * RECEIVE_PERIOD));
    }

    PRINT_EVENT_LOOP_START(&peripherals, &event_queue);

    // Main event loop - process events metacycle by metacycle
    uint32_t k;
    uint32_t last_progress_report = 0;
    for (k = 0; k < NUM_PERIODS; k++) {
        // Process all events in the next metacycle
        bool continue_processing = process_metacycle(
            &peripherals.timer,
            &peripherals.uart,
            &ugn_ctx,
            &event_queue,
            &peripherals
        );

        // Check if protocol is complete
        if (ugn_ctx.number_incoming_link_ugns_known == ugn_ctx.num_ports &&
            ugn_ctx.number_outgoing_link_ugns_acknowledged == ugn_ctx.num_ports) {
            uart_puts(&peripherals.uart, MSG_PROTOCOL_COMPLETE);
            break;  // Stop processing
        }

        if (!continue_processing) {
            break;
        }

        // Periodic progress update (to avoid excessive UART traffic)
        if (k > 0 && (k - last_progress_report) >= PROGRESS_INTERVAL) {
            PRINT_PROTOCOL_PROGRESS(&peripherals, &ugn_ctx);
            last_progress_report = k;
        }
    }

    uart_puts(&peripherals.uart, "========================================\n");

    // Print completion statistics
    PRINT_COMPLETION_STATS(&peripherals, k, start_cycles, METACYCLE_CLOCKS);

    // Print deadline miss statistics
    uart_puts(&peripherals.uart, "\nDeadline Statistics:\n");
    uart_puts(&peripherals.uart, "--------------------\n");

    uart_puts(&peripherals.uart, "SEND events:\n");
    uart_puts(&peripherals.uart, "  Met deadlines: ");
    uart_putdec(&peripherals.uart, (uint64_t)met_send_count);
    uart_puts(&peripherals.uart, "\n");
    uart_puts(&peripherals.uart, "  Missed deadlines: ");
    uart_putdec(&peripherals.uart, (uint64_t)missed_send_count);
    uart_puts(&peripherals.uart, "\n");

    uart_puts(&peripherals.uart, "RECEIVE events:\n");
    uart_puts(&peripherals.uart, "  Met deadlines: ");
    uart_putdec(&peripherals.uart, (uint64_t)met_receive_count);
    uart_puts(&peripherals.uart, "\n");
    uart_puts(&peripherals.uart, "  Missed deadlines: ");
    uart_putdec(&peripherals.uart, (uint64_t)missed_receive_count);
    uart_puts(&peripherals.uart, "\n");

    uart_puts(&peripherals.uart, "INVALIDATE events:\n");
    uart_puts(&peripherals.uart, "  Met deadlines: ");
    uart_putdec(&peripherals.uart, (uint64_t)met_invalidate_count);
    uart_puts(&peripherals.uart, "\n");
    uart_puts(&peripherals.uart, "  Missed deadlines: ");
    uart_putdec(&peripherals.uart, (uint64_t)missed_invalidate_count);
    uart_puts(&peripherals.uart, "\n");

    uart_puts(&peripherals.uart, "Total:\n");
    uart_puts(&peripherals.uart, "  Met deadlines: ");
    uart_putdec(&peripherals.uart, (uint64_t)(met_send_count + met_receive_count + met_invalidate_count));
    uart_puts(&peripherals.uart, "\n");
    uart_puts(&peripherals.uart, "  Missed deadlines: ");
    uart_putdec(&peripherals.uart, (uint64_t)(missed_send_count + missed_receive_count + missed_invalidate_count));
    uart_puts(&peripherals.uart, "\n");

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
