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
#define CYCLES_PER_BUFFER_ENTRY 100
#define BUFFER_SIZE 1000
#define METACYCLE_CLOCKS (CYCLES_PER_BUFFER_ENTRY * BUFFER_SIZE)

// Schedule one SEND task per neighbor every 3 metacycles
#define SEND_PERIOD (METACYCLE_CLOCKS * 3 * NUM_PORTS + 1)

// Schedule one RECEIVE task (checking all ports) each metacycle
#define RECEIVE_PERIOD METACYCLE_CLOCKS

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

// ============================================================================
// Event Processing Functions
// ============================================================================

// Process a single event and schedule any follow-up events
// Returns true if event should be executed, false if it should be skipped (deadline missed)
static bool process_event(uint64_t event, uint64_t event_time, UgnContext* ugn_ctx,
                          FixedIntPriorityQueue* event_queue, bool execute, Peripherals* peripherals) {
    uint64_t buffer_offset = event_time % BUFFER_SIZE;
    uint32_t port = get_event_port(event);

    if (event & EVENT_TYPE_SEND) {
        if (execute) {
            // Send UGN to the specific port encoded in the event
            send_ugn_to_port(ugn_ctx, port, buffer_offset);
            met_send_count++;
        } else {
            missed_send_count++;
        }
        // Always reschedule next send event for this port
        uint64_t next_send_time = event_time + SEND_PERIOD;
        uint64_t next_send_event = ugn_encode_event_with_port(EVENT_TYPE_SEND, port);
        pq_insert(event_queue, next_send_event, next_send_time);

        // Schedule invalidate for this port 2 metacycles after this send
        uint64_t invalidate_time = event_time + INVALIDATE_DELAY;
        uint64_t invalidate_event = ugn_encode_event_with_port(EVENT_TYPE_INVALIDATE, port);
        pq_insert(event_queue, invalidate_event, invalidate_time);

    } else if (event & EVENT_TYPE_INVALIDATE) {
        if (execute) {
            // Invalidate the specific port encoded in the event
            invalidate_port(ugn_ctx, port, buffer_offset);
            met_invalidate_count++;
        } else {
            missed_invalidate_count++;
        }

    } else if (event & EVENT_TYPE_RECEIVE) {
        if (execute) {
            // Check incoming buffer for all ports
            for (uint32_t i = 0; i < ugn_ctx->num_ports; i++) {
                (void)check_incoming_buffer(ugn_ctx, i, buffer_offset);
            }
            met_receive_count++;
        } else {
            missed_receive_count++;
        }
        // Always reschedule next receive event
        uint64_t next_receive_time = event_time + RECEIVE_PERIOD;
        uint64_t next_receive_event = ugn_encode_event_with_port(EVENT_TYPE_RECEIVE, 0);
        pq_insert(event_queue, next_receive_event, next_receive_time);
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
    PriorityQueueItem item = pq_extract_min(event_queue);
    uint64_t current_event = item.data;
    uint64_t event_time = item.priority;

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

    // Track which ports had SEND events in this metacycle (for error detection)
    // Using a simple bitmap - supports up to 32 ports
    uint32_t send_ports_bitmap = 0;

    // Process all events scheduled in this metacycle
    bool on_time = true;

    // Track the first event type and port
    if (current_event & EVENT_TYPE_SEND) {
        uint32_t port = get_event_port(current_event);
        send_ports_bitmap |= (1u << port);
    } else if (current_event & EVENT_TYPE_INVALIDATE) {
        uint32_t port = get_event_port(current_event);
        // Check if we already processed a SEND for this port in this metacycle
        if (send_ports_bitmap & (1u << port)) {
            PRINT_SEND_INVALIDATE_ERROR(peripherals, port);
        }
    }

    // Process the first event (already extracted)
    process_event(current_event, event_time, ugn_ctx, event_queue, on_time, peripherals);
    bool processing_metacycle = true;
    while (processing_metacycle) {
        // Check if there are more events in the queue
        if (pq_is_empty(event_queue)) {
            return false;  // Stop processing - no more events
        }

        // Check if queue is full
        if (pq_is_full(event_queue)) {
            uart_puts(uart, MSG_QUEUE_FULL);
            return false;  // Stop processing
        }

        // Peek at the next event to check its time
        PriorityQueueItem next_item = pq_peek_min(event_queue);

        // If next event is in a different metacycle, exit without removing it
        if (next_item.priority > metacycle_end) {
            return true;  // Continue processing in next metacycle
        }

        // Otherwise, extract this event to process in the current metacycle
        item = pq_extract_min(event_queue);
        current_event = item.data;
        event_time = item.priority;

        // Track SEND/INVALIDATE events for error detection
        if (current_event & EVENT_TYPE_SEND) {
            uint32_t port = get_event_port(current_event);
            send_ports_bitmap |= (1u << port);
        } else if (current_event & EVENT_TYPE_INVALIDATE) {
            uint32_t port = get_event_port(current_event);
            // Check if we already processed a SEND for this port in this metacycle
            if (send_ports_bitmap & (1u << port)) {
                PRINT_SEND_INVALIDATE_ERROR(peripherals, port);
            }
        }

        // Check if we're still on time
        CompareResult cmp_result = get_compare_result(timer);
        on_time = cmp_result == COMPARE_LESS;

        // Process the event (skip execution if deadline missed)
        process_event(current_event, event_time, ugn_ctx, event_queue, on_time, peripherals);
    }

    // Should never reach here, but add return to satisfy compiler
    return false;
}

// ============================================================================
// Main Entry Point
// ============================================================================

int c_main(void) {
    // Initialize all peripherals
    Peripherals peripherals;
    peripherals_init(&peripherals);

    // Initialize and test gather/scatter units in a single loop per port
    uart_puts(&peripherals.uart, "Testing gather/scatter units...\n");

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
        uint64_t send_time = start_cycles + STARTING_TICK_SND_OFFSET + port * SEND_PERIOD;
        uint64_t send_event = ugn_encode_event_with_port(EVENT_TYPE_SEND, port);
        pq_insert(&event_queue, send_event, send_time);
    }

    // Schedule a single RECEIVE event that checks all ports each metacycle
    uint64_t receive_time = start_cycles + STARTING_TICK_REC_OFFSET;
    uint64_t receive_event = ugn_encode_event_with_port(EVENT_TYPE_RECEIVE, 0);
    pq_insert(&event_queue, receive_event, receive_time);

    PRINT_EVENT_LOOP_START(&peripherals, &event_queue);

    // Main event loop - process events metacycle by metacycle
    uint32_t k;
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
