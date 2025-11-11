// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#ifndef MESSAGES_H
#define MESSAGES_H

#include <stdint.h>

// Note: This header is included after peripherals.h in main.c
// so we have access to Uart type and uart_* functions

// ============================================================================
// Message Printing Macros
// ============================================================================
// These macros follow the pattern from elara_experiments for setting up
// formatted messages that can be printed to UART.

// Message for missed metacycle - calculates metacycle numbers from cycles and timer
#define PRINT_MISSED_METACYCLE(PERIPHERALS_PTR, METACYCLE_START_CYCLES, METACYCLE_CLOCKS) \
    do { \
        uint32_t expected_mc = (uint32_t)((METACYCLE_START_CYCLES) / (METACYCLE_CLOCKS)); \
        uint64_t current_cycles = timer_now_cycles(&((PERIPHERALS_PTR)->timer)); \
        uint32_t current_mc = (uint32_t)(current_cycles / (METACYCLE_CLOCKS)); \
        uart_puts(&((PERIPHERALS_PTR)->uart), "*** Missed metacycle "); \
        uart_putdec(&((PERIPHERALS_PTR)->uart), (uint64_t)expected_mc); \
        uart_puts(&((PERIPHERALS_PTR)->uart), ", currently in metacycle "); \
        uart_putdec(&((PERIPHERALS_PTR)->uart), (uint64_t)current_mc); \
        uart_puts(&((PERIPHERALS_PTR)->uart), " ***\r\n"); \
    } while(0)

// Message for protocol completion progress - takes UgnContext pointer
#define PRINT_PROTOCOL_PROGRESS(PERIPHERALS_PTR, UGN_CTX_PTR) \
    do { \
        uart_puts(&((PERIPHERALS_PTR)->uart), "Progress: IN="); \
        uart_putdec(&((PERIPHERALS_PTR)->uart), (uint64_t)((UGN_CTX_PTR)->number_incoming_link_ugns_known)); \
        uart_puts(&((PERIPHERALS_PTR)->uart), "/"); \
        uart_putdec(&((PERIPHERALS_PTR)->uart), (uint64_t)((UGN_CTX_PTR)->num_ports)); \
        uart_puts(&((PERIPHERALS_PTR)->uart), ", OUT="); \
        uart_putdec(&((PERIPHERALS_PTR)->uart), (uint64_t)((UGN_CTX_PTR)->number_outgoing_link_ugns_acknowledged)); \
        uart_puts(&((PERIPHERALS_PTR)->uart), "/"); \
        uart_putdec(&((PERIPHERALS_PTR)->uart), (uint64_t)((UGN_CTX_PTR)->num_ports)); \
        uart_puts(&((PERIPHERALS_PTR)->uart), "\n"); \
    } while(0)

// Message for starting cycle information - takes scatter unit and calculates values
#define PRINT_START_INFO(PERIPHERALS_PTR, METACYCLE_CLOCKS) \
    do { \
        uint32_t start_mc = scatter_unit_metacycle_count(&((PERIPHERALS_PTR)->scatter_units[0])) + 1; \
        uint64_t start_cyc = (uint64_t)start_mc * (METACYCLE_CLOCKS); \
        uart_puts(&((PERIPHERALS_PTR)->uart), "Start: metacycle="); \
        uart_putdec(&((PERIPHERALS_PTR)->uart), (uint64_t)start_mc); \
        uart_puts(&((PERIPHERALS_PTR)->uart), ", cycles="); \
        uart_putdec(&((PERIPHERALS_PTR)->uart), start_cyc); \
        uart_puts(&((PERIPHERALS_PTR)->uart), "\n"); \
    } while(0)

// Message for event queue statistics - takes queue pointer
#define PRINT_QUEUE_STATS(PERIPHERALS_PTR, EVENT_QUEUE_PTR) \
    do { \
        uart_puts(&((PERIPHERALS_PTR)->uart), "Queue: "); \
        uart_putdec(&((PERIPHERALS_PTR)->uart), (uint64_t)((EVENT_QUEUE_PTR)->size)); \
        uart_puts(&((PERIPHERALS_PTR)->uart), "/"); \
        uart_putdec(&((PERIPHERALS_PTR)->uart), (uint64_t)MAX_FIXED_PQ_SIZE); \
        uart_puts(&((PERIPHERALS_PTR)->uart), " events\n"); \
    } while(0)

// Message for completion statistics - calculates elapsed time
#define PRINT_COMPLETION_STATS(PERIPHERALS_PTR, ITERATIONS, START_CYCLES, METACYCLE_CLOCKS) \
    do { \
        uart_puts(&((PERIPHERALS_PTR)->uart), "Protocol terminated after "); \
        uart_putdec(&((PERIPHERALS_PTR)->uart), (uint64_t)(ITERATIONS)); \
        uart_puts(&((PERIPHERALS_PTR)->uart), " iterations\n"); \
        uint64_t final_cyc = timer_now_cycles(&((PERIPHERALS_PTR)->timer)); \
        uint64_t elapsed_cyc = final_cyc - (START_CYCLES); \
        uart_puts(&((PERIPHERALS_PTR)->uart), "Elapsed cycles: "); \
        uart_putdec(&((PERIPHERALS_PTR)->uart), elapsed_cyc); \
        uart_puts(&((PERIPHERALS_PTR)->uart), " ("); \
        uart_putdec(&((PERIPHERALS_PTR)->uart), elapsed_cyc / (METACYCLE_CLOCKS)); \
        uart_puts(&((PERIPHERALS_PTR)->uart), " metacycles)\n"); \
    } while(0)

// Message for UGN discovery summary - takes UgnContext pointer
#define PRINT_UGN_SUMMARY(PERIPHERALS_PTR, UGN_CTX_PTR) \
    do { \
        uart_puts(&((PERIPHERALS_PTR)->uart), "Node ID: "); \
        uart_putdec(&((PERIPHERALS_PTR)->uart), (uint64_t)((UGN_CTX_PTR)->node_id)); \
        uart_puts(&((PERIPHERALS_PTR)->uart), "\n"); \
        uart_puts(&((PERIPHERALS_PTR)->uart), "Number of ports: "); \
        uart_putdec(&((PERIPHERALS_PTR)->uart), (uint64_t)((UGN_CTX_PTR)->num_ports)); \
        uart_puts(&((PERIPHERALS_PTR)->uart), "\n"); \
        uart_puts(&((PERIPHERALS_PTR)->uart), "Incoming UGNs known: "); \
        uart_putdec(&((PERIPHERALS_PTR)->uart), (uint64_t)((UGN_CTX_PTR)->number_incoming_link_ugns_known)); \
        uart_puts(&((PERIPHERALS_PTR)->uart), "/"); \
        uart_putdec(&((PERIPHERALS_PTR)->uart), (uint64_t)((UGN_CTX_PTR)->num_ports)); \
        uart_puts(&((PERIPHERALS_PTR)->uart), "\n"); \
        uart_puts(&((PERIPHERALS_PTR)->uart), "Outgoing UGNs acknowledged: "); \
        uart_putdec(&((PERIPHERALS_PTR)->uart), (uint64_t)((UGN_CTX_PTR)->number_outgoing_link_ugns_acknowledged)); \
        uart_puts(&((PERIPHERALS_PTR)->uart), "/"); \
        uart_putdec(&((PERIPHERALS_PTR)->uart), (uint64_t)((UGN_CTX_PTR)->num_ports)); \
        uart_puts(&((PERIPHERALS_PTR)->uart), "\n\n"); \
    } while(0)

// Message for printing a single UGN edge (valid)
#define PRINT_UGN_EDGE(PERIPHERALS_PTR, PORT_NUM, EDGE_PTR) \
    do { \
        uart_puts(&((PERIPHERALS_PTR)->uart), "  Port "); \
        uart_putdec(&((PERIPHERALS_PTR)->uart), (uint64_t)(PORT_NUM)); \
        uart_puts(&((PERIPHERALS_PTR)->uart), ": src_node="); \
        uart_putdec(&((PERIPHERALS_PTR)->uart), (uint64_t)((EDGE_PTR)->src_node)); \
        uart_puts(&((PERIPHERALS_PTR)->uart), ", src_port="); \
        uart_putdec(&((PERIPHERALS_PTR)->uart), (uint64_t)((EDGE_PTR)->src_port)); \
        uart_puts(&((PERIPHERALS_PTR)->uart), ", dst_node="); \
        uart_putdec(&((PERIPHERALS_PTR)->uart), (uint64_t)((EDGE_PTR)->dst_node)); \
        uart_puts(&((PERIPHERALS_PTR)->uart), ", dst_port="); \
        uart_putdec(&((PERIPHERALS_PTR)->uart), (uint64_t)((EDGE_PTR)->dst_port)); \
        uart_puts(&((PERIPHERALS_PTR)->uart), ", ugn="); \
        uart_putdec(&((PERIPHERALS_PTR)->uart), (uint64_t)((EDGE_PTR)->ugn)); \
        uart_puts(&((PERIPHERALS_PTR)->uart), "\n"); \
    } while(0)

// Message for printing an invalid port
#define PRINT_INVALID_PORT(PERIPHERALS_PTR, PORT_NUM) \
    do { \
        uart_puts(&((PERIPHERALS_PTR)->uart), "  Port "); \
        uart_putdec(&((PERIPHERALS_PTR)->uart), (uint64_t)(PORT_NUM)); \
        uart_puts(&((PERIPHERALS_PTR)->uart), ": (not valid)\n"); \
    } while(0)

// Message for printing all edges in a list (incoming or outgoing)
#define PRINT_UGN_EDGE_LIST(PERIPHERALS_PTR, TITLE, EDGE_LIST_PTR, NUM_PORTS) \
    do { \
        uart_puts(&((PERIPHERALS_PTR)->uart), TITLE); \
        for (uint32_t i = 0; i < (NUM_PORTS); i++) { \
            UgnEdge* edge = &((EDGE_LIST_PTR)[i]); \
            if (edge->is_valid) { \
                PRINT_UGN_EDGE(PERIPHERALS_PTR, i, edge); \
            } else { \
                PRINT_INVALID_PORT(PERIPHERALS_PTR, i); \
            } \
        } \
    } while(0)

// Message for printing first event information - takes event and metacycle_clocks
#define PRINT_FIRST_EVENT_INFO(PERIPHERALS_PTR, EVENT, METACYCLE_CLOCKS) \
    do { \
        uint64_t event_time = get_event_time(EVENT); \
        uint32_t event_metacycle = (uint32_t)(event_time / (METACYCLE_CLOCKS)); \
        uart_puts(&((PERIPHERALS_PTR)->uart), "First event scheduled at:\n"); \
        uart_puts(&((PERIPHERALS_PTR)->uart), "  Metacycle: "); \
        uart_putdec(&((PERIPHERALS_PTR)->uart), (uint64_t)event_metacycle); \
        uart_puts(&((PERIPHERALS_PTR)->uart), "\n  Cycle: "); \
        uart_putdec(&((PERIPHERALS_PTR)->uart), event_time); \
        uart_puts(&((PERIPHERALS_PTR)->uart), "\n  Type: "); \
        if ((EVENT) & EVENT_TYPE_SEND) { \
            uart_puts(&((PERIPHERALS_PTR)->uart), "SEND\n"); \
        } else if ((EVENT) & EVENT_TYPE_RECEIVE) { \
            uart_puts(&((PERIPHERALS_PTR)->uart), "RECEIVE\n"); \
        } else { \
            uart_puts(&((PERIPHERALS_PTR)->uart), "INVALIDATE\n"); \
        } \
    } while(0)

// Message for printing loop configuration
#define PRINT_LOOP_CONFIG(PERIPHERALS_PTR, MAX_ITERATIONS, SEND_PERIOD, RECEIVE_PERIOD, METACYCLE_CLOCKS) \
    do { \
        uart_puts(&((PERIPHERALS_PTR)->uart), "\nLoop configuration:\n"); \
        uart_puts(&((PERIPHERALS_PTR)->uart), "  Max iterations: "); \
        uart_putdec(&((PERIPHERALS_PTR)->uart), (uint64_t)(MAX_ITERATIONS)); \
        uart_puts(&((PERIPHERALS_PTR)->uart), "\n  Send period: "); \
        uart_putdec(&((PERIPHERALS_PTR)->uart), (uint64_t)(SEND_PERIOD)); \
        uart_puts(&((PERIPHERALS_PTR)->uart), " cycles ("); \
        uart_putdec(&((PERIPHERALS_PTR)->uart), (uint64_t)((SEND_PERIOD) / (METACYCLE_CLOCKS))); \
        uart_puts(&((PERIPHERALS_PTR)->uart), " metacycles)\n"); \
        uart_puts(&((PERIPHERALS_PTR)->uart), "  Receive period: "); \
        uart_putdec(&((PERIPHERALS_PTR)->uart), (uint64_t)(RECEIVE_PERIOD)); \
        uart_puts(&((PERIPHERALS_PTR)->uart), " cycles ("); \
        uart_putdec(&((PERIPHERALS_PTR)->uart), (uint64_t)((RECEIVE_PERIOD) / (METACYCLE_CLOCKS))); \
        uart_puts(&((PERIPHERALS_PTR)->uart), " metacycles)\n"); \
    } while(0)

// Message for printing protocol goals
#define PRINT_PROTOCOL_GOALS(PERIPHERALS_PTR, UGN_CTX_PTR) \
    do { \
        uart_puts(&((PERIPHERALS_PTR)->uart), "\nProtocol goals:\n"); \
        uart_puts(&((PERIPHERALS_PTR)->uart), "  Discover incoming UGNs for all "); \
        uart_putdec(&((PERIPHERALS_PTR)->uart), (uint64_t)((UGN_CTX_PTR)->num_ports)); \
        uart_puts(&((PERIPHERALS_PTR)->uart), " ports\n"); \
        uart_puts(&((PERIPHERALS_PTR)->uart), "  Acknowledge outgoing UGNs for all "); \
        uart_putdec(&((PERIPHERALS_PTR)->uart), (uint64_t)((UGN_CTX_PTR)->num_ports)); \
        uart_puts(&((PERIPHERALS_PTR)->uart), " ports\n"); \
    } while(0)

// Message for printing initialization information - compact summary
#define PRINT_INIT_INFO(PERIPHERALS_PTR, UGN_CTX_PTR, METACYCLE_CLOCKS, SEND_PERIOD, RECEIVE_PERIOD, MAXDEG) \
    do { \
        uart_puts(&((PERIPHERALS_PTR)->uart), "========================================\n"); \
        uart_puts(&((PERIPHERALS_PTR)->uart), "Bittide UGN Discovery Protocol\n"); \
        uart_puts(&((PERIPHERALS_PTR)->uart), "========================================\n"); \
        uart_puts(&((PERIPHERALS_PTR)->uart), "Node ID: "); \
        uart_putdec(&((PERIPHERALS_PTR)->uart), (uint64_t)((UGN_CTX_PTR)->node_id)); \
        uart_puts(&((PERIPHERALS_PTR)->uart), " | Ports: "); \
        uart_putdec(&((PERIPHERALS_PTR)->uart), (uint64_t)((UGN_CTX_PTR)->num_ports)); \
        uart_puts(&((PERIPHERALS_PTR)->uart), " | Metacycle: "); \
        uart_putdec(&((PERIPHERALS_PTR)->uart), (uint64_t)(METACYCLE_CLOCKS)); \
        uart_puts(&((PERIPHERALS_PTR)->uart), " cycles\n"); \
        uart_puts(&((PERIPHERALS_PTR)->uart), "Send period: "); \
        uart_putdec(&((PERIPHERALS_PTR)->uart), (uint64_t)(SEND_PERIOD)); \
        uart_puts(&((PERIPHERALS_PTR)->uart), " | Receive period: "); \
        uart_putdec(&((PERIPHERALS_PTR)->uart), (uint64_t)(RECEIVE_PERIOD)); \
        uart_puts(&((PERIPHERALS_PTR)->uart), " | Max degree: "); \
        uart_putdec(&((PERIPHERALS_PTR)->uart), (uint64_t)(MAXDEG)); \
        uart_puts(&((PERIPHERALS_PTR)->uart), "\n========================================\n"); \
    } while(0)

// Message for printing event queue start information
#define PRINT_EVENT_LOOP_START(PERIPHERALS_PTR, EVENT_QUEUE_PTR) \
    do { \
        uart_puts(&((PERIPHERALS_PTR)->uart), "Starting event loop with "); \
        uart_putdec(&((PERIPHERALS_PTR)->uart), (uint64_t)((EVENT_QUEUE_PTR)->size)); \
        uart_puts(&((PERIPHERALS_PTR)->uart), " initial events...\n\n"); \
    } while(0)

// ============================================================================
// Pre-defined Message Strings
// ============================================================================

#define MSG_QUEUE_FULL "*** ERROR: Queue full ***\n"
#define MSG_QUEUE_EMPTY "*** ERROR: Queue empty ***\n"
#define MSG_PROTOCOL_COMPLETE "Protocol complete - all UGNs discovered!\n"
#define MSG_DEADLINE_MISS "*** Deadline miss in metacycle ***\n"

#endif // MESSAGES_H
