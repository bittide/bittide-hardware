// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#ifndef MESSAGES_H
#define MESSAGES_H

#include "bittide_uart.h"
#include "bittide_ugn.h"
#include <stdint.h>

// Note: This header is included after peripherals.h in main.c
// so we have access to Uart type and uart_* functions

// ============================================================================
// Ringbuffer Alignment State
// ============================================================================

enum RingbufferAlignState {
  RINGBUFFER_ALIGN_EMPTY = 0,
  RINGBUFFER_ALIGN_ANNOUNCE = 1,
  RINGBUFFER_ALIGN_ACKNOWLEDGE = 2,
};

// ============================================================================
// Message Printing Macros
// ============================================================================
// These macros follow the pattern from elara_experiments for setting up
// formatted messages that can be printed to UART.

// Message for missed metacycle - calculates metacycle numbers from cycles and
// timer
#define PRINT_MISSED_METACYCLE(UART, TIMER, METACYCLE_START_CYCLES,            \
                               METACYCLE_CLOCKS)                               \
  do {                                                                         \
    uint32_t expected_mc =                                                     \
        (uint32_t)((METACYCLE_START_CYCLES) / (METACYCLE_CLOCKS));             \
    uint64_t current_cycles = timer_now_cycles(TIMER);                         \
    uint32_t current_mc = (uint32_t)(current_cycles / (METACYCLE_CLOCKS));     \
    uart_puts(UART, "*** Missed metacycle ");                                  \
    uart_putdec(UART, (uint64_t)expected_mc);                                  \
    uart_puts(UART, ", currently in metacycle ");                              \
    uart_putdec(UART, (uint64_t)current_mc);                                   \
    uart_puts(UART, " ***\r\n");                                               \
  } while (0)

// Message for event queue statistics - takes queue pointer
#define PRINT_QUEUE_STATS(UART, EVENT_QUEUE_PTR)                               \
  do {                                                                         \
    uart_puts(UART, "Queue: ");                                                \
    uart_putdec(UART, (uint64_t)((EVENT_QUEUE_PTR)->size));                    \
    uart_puts(UART, "/");                                                      \
    uart_putdec(UART, (uint64_t)MAX_FIXED_PQ_SIZE);                            \
    uart_puts(UART, " events\n");                                              \
  } while (0)

// Message for completion statistics - calculates elapsed time
#define PRINT_COMPLETION_STATS(UART, TIMER, ITERATIONS, START_CYCLES)          \
  do {                                                                         \
    uart_puts(UART, "Protocol terminated after ");                             \
    uart_putdec(UART, (uint64_t)(ITERATIONS));                                 \
    uart_puts(UART, " iterations\n");                                          \
    uint64_t final_cyc = timer_now_cycles(TIMER);                              \
    uint64_t elapsed_cyc = final_cyc - (START_CYCLES);                         \
    uart_puts(UART, "Elapsed cycles: ");                                       \
    uart_putdec(UART, elapsed_cyc);                                            \
  } while (0)

// Message for printing a single UGN edge (valid)
#define PRINT_UGN_EDGE(UART, PORT_NUM, EDGE_PTR)                               \
  do {                                                                         \
    uart_puts(UART, "  Port ");                                                \
    uart_putdec(UART, (uint64_t)(PORT_NUM));                                   \
    uart_puts(UART, ": src_node=");                                            \
    uart_putdec(UART, (uint64_t)((EDGE_PTR)->src_node));                       \
    uart_puts(UART, ", src_port=");                                            \
    uart_putdec(UART, (uint64_t)((EDGE_PTR)->src_port));                       \
    uart_puts(UART, ", dst_node=");                                            \
    uart_putdec(UART, (uint64_t)((EDGE_PTR)->dst_node));                       \
    uart_puts(UART, ", dst_port=");                                            \
    uart_putdec(UART, (uint64_t)((EDGE_PTR)->dst_port));                       \
    uart_puts(UART, ", ugn=");                                                 \
    uart_putdec_signed(UART, (EDGE_PTR)->ugn);                                 \
    uart_puts(UART, "\n");                                                     \
  } while (0)

// Message for printing an invalid port
#define PRINT_INVALID_PORT(UART, PORT_NUM)                                     \
  do {                                                                         \
    uart_puts(UART, "  Port ");                                                \
    uart_putdec(UART, (uint64_t)(PORT_NUM));                                   \
    uart_puts(UART, ": (not valid)\n");                                        \
  } while (0)

// Message for printing all edges in a list (incoming or outgoing)
#define PRINT_UGN_EDGE_LIST(UART, TITLE, EDGE_LIST_PTR, NUM_PORTS)             \
  do {                                                                         \
    uart_puts(UART, TITLE);                                                    \
    for (uint32_t i = 0; i < (NUM_PORTS); i++) {                               \
      UgnEdge *edge = &((EDGE_LIST_PTR)[i]);                                   \
      if (edge->is_valid) {                                                    \
        PRINT_UGN_EDGE(UART, i, edge);                                         \
      } else {                                                                 \
        PRINT_INVALID_PORT(UART, i);                                           \
      }                                                                        \
    }                                                                          \
  } while (0)

// Message for printing first event information - takes event and
// metacycle_clocks
#define PRINT_FIRST_EVENT_INFO(UART, EVENT_STRUCT, METACYCLE_CLOCKS)           \
  do {                                                                         \
    uint64_t event_time = get_event_time(EVENT_STRUCT);                        \
    uint32_t event_metacycle = (uint32_t)(event_time / (METACYCLE_CLOCKS));    \
    uart_puts(UART, "First event scheduled at:\n");                            \
    uart_puts(UART, "  Metacycle: ");                                          \
    uart_putdec(UART, (uint64_t)event_metacycle);                              \
    uart_puts(UART, "\n  Cycle: ");                                            \
    uart_putdec(UART, event_time);                                             \
    uart_puts(UART, "\n  Type: ");                                             \
    if ((EVENT_STRUCT).type == EVENT_TYPE_SEND) {                              \
      uart_puts(UART, "SEND (");                                               \
      if ((EVENT_STRUCT).msg_type == MSG_TYPE_ANNOUNCE) {                      \
        uart_puts(UART, "ANNOUNCE");                                           \
      } else {                                                                 \
        uart_puts(UART, "ACKNOWLEDGE");                                        \
      }                                                                        \
      uart_puts(UART, ")\n");                                                  \
    } else if ((EVENT_STRUCT).type == EVENT_TYPE_RECEIVE) {                    \
      uart_puts(UART, "RECEIVE\n");                                            \
    } else {                                                                   \
      uart_puts(UART, "INVALIDATE\n");                                         \
    }                                                                          \
  } while (0)

// Message for printing loop configuration
#define PRINT_LOOP_CONFIG(UART, MAX_ITERATIONS, SEND_PERIOD, RECEIVE_PERIOD,   \
                          METACYCLE_CLOCKS)                                    \
  do {                                                                         \
    uart_puts(UART, "\nLoop configuration:\n");                                \
    uart_puts(UART, "  Max iterations: ");                                     \
    uart_putdec(UART, (uint64_t)(MAX_ITERATIONS));                             \
    uart_puts(UART, "\n  Send period: ");                                      \
    uart_putdec(UART, (uint64_t)(SEND_PERIOD));                                \
    uart_puts(UART, " cycles (");                                              \
    uart_putdec(UART, (uint64_t)((SEND_PERIOD) / (METACYCLE_CLOCKS)));         \
    uart_puts(UART, " metacycles)\n");                                         \
    uart_puts(UART, "  Receive period: ");                                     \
    uart_putdec(UART, (uint64_t)(RECEIVE_PERIOD));                             \
    uart_puts(UART, " cycles (");                                              \
    uart_putdec(UART, (uint64_t)((RECEIVE_PERIOD) / (METACYCLE_CLOCKS)));      \
    uart_puts(UART, " metacycles)\n");                                         \
  } while (0)

// Message for printing protocol goals
#define PRINT_PROTOCOL_GOALS(UART, UGN_CTX_PTR)                                \
  do {                                                                         \
    uart_puts(UART, "\nProtocol goals:\n");                                    \
    uart_puts(UART, "  Discover incoming UGNs for all ");                      \
    uart_putdec(UART, (uint64_t)((UGN_CTX_PTR)->num_ports));                   \
    uart_puts(UART, " ports\n");                                               \
    uart_puts(UART, "  Acknowledge outgoing UGNs for all ");                   \
    uart_putdec(UART, (uint64_t)((UGN_CTX_PTR)->num_ports));                   \
    uart_puts(UART, " ports\n");                                               \
  } while (0)

// Message for printing initialization information - compact summary
#define PRINT_INIT_INFO(UART, UGN_CTX_PTR, BUFFER_SIZE, SEND_PERIOD,           \
                        RECEIVE_PERIOD, NUM_PORTS)                             \
  do {                                                                         \
    uart_puts(UART, "========================================\n");             \
    uart_puts(UART, "Bittide UGN Discovery Protocol\n");                       \
    uart_puts(UART, "========================================\n");             \
    uart_puts(UART, "Node ID: ");                                              \
    uart_putdec(UART, (uint64_t)((UGN_CTX_PTR)->node_id));                     \
    uart_puts(UART, " | Ports: ");                                             \
    uart_putdec(UART, (uint64_t)((UGN_CTX_PTR)->num_ports));                   \
    uart_puts(UART, " | Buffer size: ");                                       \
    uart_putdec(UART, (uint64_t)(BUFFER_SIZE));                                \
    uart_puts(UART, " cycles\n");                                              \
    uart_puts(UART, "Send period: ");                                          \
    uart_putdec(UART, (uint64_t)(SEND_PERIOD));                                \
    uart_puts(UART, " | Receive period: ");                                    \
    uart_putdec(UART, (uint64_t)(RECEIVE_PERIOD));                             \
    uart_puts(UART, " | Max degree: ");                                        \
    uart_putdec(UART, (uint64_t)(NUM_PORTS));                                  \
    uart_puts(UART, "\n========================================\n");           \
  } while (0)

// Message for printing event queue start information
#define PRINT_EVENT_LOOP_START(UART, EVENT_QUEUE_PTR)                          \
  do {                                                                         \
    uart_puts(UART, "Starting event loop with ");                              \
    uart_putdec(UART, (uint64_t)((EVENT_QUEUE_PTR)->size));                    \
    uart_puts(UART, " initial events...\n\n");                                 \
  } while (0)

// Message for printing deadline statistics
#define PRINT_DEADLINE_STATS(UART, UGN_CTX_PTR)                                \
  do {                                                                         \
    uart_puts(UART, "\nDeadline Statistics:\n");                               \
    uart_puts(UART, "--------------------\n");                                 \
    uart_puts(UART, "SEND events:\n");                                         \
    uart_puts(UART, "  Met deadlines: ");                                      \
    uart_putdec(UART, (uint64_t)((UGN_CTX_PTR)->met_send_count));              \
    uart_puts(UART, "\n  Missed deadlines: ");                                 \
    uart_putdec(UART, (uint64_t)((UGN_CTX_PTR)->missed_send_count));           \
    uart_puts(UART, "\n");                                                     \
    uart_puts(UART, "RECEIVE events:\n");                                      \
    uart_puts(UART, "  Met deadlines: ");                                      \
    uart_putdec(UART, (uint64_t)((UGN_CTX_PTR)->met_receive_count));           \
    uart_puts(UART, "\n  Missed deadlines: ");                                 \
    uart_putdec(UART, (uint64_t)((UGN_CTX_PTR)->missed_receive_count));        \
    uart_puts(UART, "\n");                                                     \
    uart_puts(UART, "INVALIDATE events:\n");                                   \
    uart_puts(UART, "  Met deadlines: ");                                      \
    uart_putdec(UART, (uint64_t)((UGN_CTX_PTR)->met_invalidate_count));        \
    uart_puts(UART, "\n  Missed deadlines: ");                                 \
    uart_putdec(UART, (uint64_t)((UGN_CTX_PTR)->missed_invalidate_count));     \
    uart_puts(UART, "\n");                                                     \
    uart_puts(UART, "Total:\n");                                               \
    uart_puts(UART, "  Met deadlines: ");                                      \
    uart_putdec(UART, (uint64_t)((UGN_CTX_PTR)->met_send_count +               \
                                 (UGN_CTX_PTR)->met_receive_count +            \
                                 (UGN_CTX_PTR)->met_invalidate_count));        \
    uart_puts(UART, "\n  Missed deadlines: ");                                 \
    uart_putdec(UART, (uint64_t)((UGN_CTX_PTR)->missed_send_count +            \
                                 (UGN_CTX_PTR)->missed_receive_count +         \
                                 (UGN_CTX_PTR)->missed_invalidate_count));     \
    uart_puts(UART, "\n");                                                     \
  } while (0)

// Message for printing roundtrip latencies
#define PRINT_ROUNDTRIP_LATENCIES(UART, UGN_CTX_PTR)                           \
  do {                                                                         \
    uart_puts(UART, "\nRoundtrip Latencies:\n");                               \
    for (uint32_t port = 0; port < (UGN_CTX_PTR)->num_ports; port++) {         \
      UgnEdge *incoming = &((UGN_CTX_PTR)->incoming_link_ugn_list[port]);      \
      UgnEdge *outgoing = &((UGN_CTX_PTR)->outgoing_link_ugn_list[port]);      \
      if (incoming->is_valid && outgoing->is_valid) {                          \
        int64_t roundtrip = incoming->ugn + outgoing->ugn;                     \
        uart_puts(UART, "  Port ");                                            \
        uart_putdec(UART, (uint64_t)port);                                     \
        uart_puts(UART, ": ");                                                 \
        uart_putdec_signed(UART, roundtrip);                                   \
        uart_puts(UART, " cycles\n");                                          \
      } else {                                                                 \
        uart_puts(UART, "  Port ");                                            \
        uart_putdec(UART, (uint64_t)port);                                     \
        uart_puts(UART, ": (incomplete - missing ");                           \
        if (!incoming->is_valid)                                               \
          uart_puts(UART, "incoming");                                         \
        if (!incoming->is_valid && !outgoing->is_valid)                        \
          uart_puts(UART, " and ");                                            \
        if (!outgoing->is_valid)                                               \
          uart_puts(UART, "outgoing");                                         \
        uart_puts(UART, ")\n");                                                \
      }                                                                        \
    }                                                                          \
  } while (0)

// Message for printing found incoming UGN
#define PRINT_FOUND_INCOMING_UGN(UART, EVENT_TIME, PORT, UGN)                  \
  do {                                                                         \
    uart_puts(UART, "I P");                                                    \
    uart_putdec(UART, (uint64_t)(PORT));                                       \
    uart_puts(UART, ": ");                                                     \
    uart_putdec_signed(UART, (UGN));                                           \
    uart_puts(UART, " @ ");                                                    \
    uart_putdec(UART, (uint64_t)(EVENT_TIME));                                 \
    uart_puts(UART, "\n");                                                     \
  } while (0)

// Message for printing found outgoing UGN
#define PRINT_FOUND_OUTGOING_UGN(UART, EVENT_TIME, PORT, UGN)                  \
  do {                                                                         \
    uart_puts(UART, "O P");                                                    \
    uart_putdec(UART, (uint64_t)(PORT));                                       \
    uart_puts(UART, ": ");                                                     \
    uart_putdec_signed(UART, (UGN));                                           \
    uart_puts(UART, " @ ");                                                    \
    uart_putdec(UART, (uint64_t)(EVENT_TIME));                                 \
    uart_puts(UART, "\n");                                                     \
  } while (0)

// Message for printing final send scheduled
#define PRINT_FINAL_SEND_SCHEDULED(UART, PORT, TIME)                           \
  do {                                                                         \
    uart_puts(UART, "FS P");                                                   \
    uart_putdec(UART, (uint64_t)(PORT));                                       \
    uart_puts(UART, " @ ");                                                    \
    uart_putdec(UART, (uint64_t)(TIME));                                       \
    uart_puts(UART, "\n");                                                     \
  } while (0)

// Message for printing statistics regarding what kind of messages were sent and
// received
#define PRINT_MESSAGE_COUNT_STATES(UART, UGN_CTX_PTR)                          \
  do {                                                                         \
    uart_puts(UART, "\nMessage Counts:\n");                                    \
    uart_puts(UART, "  Received announces: ");                                 \
    uart_putdec(UART, (uint64_t)((UGN_CTX_PTR)->incoming_announce_count));     \
    uart_puts(UART, "\n  Received acknowledges: ");                            \
    uart_putdec(UART, (uint64_t)((UGN_CTX_PTR)->incoming_acknowledge_count));  \
    uart_puts(UART, "\n  Announces sent: ");                                   \
    uart_putdec(UART, (uint64_t)((UGN_CTX_PTR)->outgoing_announce_count));     \
    uart_puts(UART, "\n  Acknowledges sent: ");                                \
    uart_putdec(UART, (uint64_t)((UGN_CTX_PTR)->outgoing_acknowledge_count));  \
    uart_puts(UART, "\n");                                                     \
  } while (0);

// Assertion macro to check that SEND event_time is correctly aligned
#define ASSERT_SEND_TIME_VALID(UART, event_time, offset)                       \
  do {                                                                         \
    uint64_t expected_time =                                                   \
        ((event_time / METACYCLE_CLOCKS) * METACYCLE_CLOCKS) +                 \
        (offset) if ((event_time) != expected_time) {                          \
      uart_puts(UART, "[UGN WARNING] SEND event_time misaligned: ");           \
      uart_putdec(UART, event_time);                                           \
      uart_puts(UART, " (expected ");                                          \
      uart_putdec(UART, expected_time);                                        \
      uart_puts(UART, ")\n");                                                  \
    }                                                                          \
  } while (0)

// Assertion macro to check that SEND and RECEIVE periods are coprime
#define ASSERT_PERIODS_COPRIME(UART, SEND_PERIOD, RECEIVE_PERIOD)              \
  do {                                                                         \
    uint64_t a = (SEND_PERIOD);                                                \
    uint64_t b = (RECEIVE_PERIOD);                                             \
    while (b != 0) {                                                           \
      uint64_t temp = b;                                                       \
      b = a % b;                                                               \
      a = temp;                                                                \
    }                                                                          \
    if (a != 1) {                                                              \
      uart_puts(UART,                                                          \
                "[UGN WARNING] SEND and RECEIVE periods are not coprime: ");   \
      uart_putdec(UART, (SEND_PERIOD));                                        \
      uart_puts(UART, " and ");                                                \
      uart_putdec(UART, (RECEIVE_PERIOD));                                     \
      uart_puts(UART, "\n");                                                   \
    }                                                                          \
  } while (0)

#define PRINT_DEADLINE_MISSED(UART, TIMER, EVENT_PTR, EVENT_TIME)              \
  do {                                                                         \
    uart_puts(UART, "[UGN WARNING] Event missed deadline: type=");             \
    uart_puts(UART, show_event_type((EVENT_PTR)->type));                       \
    uart_puts(UART, ", scheduled_time=");                                      \
    uart_putdec(UART, (uint64_t)(EVENT_TIME));                                 \
    uart_puts(UART, ", now=");                                                 \
    uint64_t now_cycles = timer_now_cycles(TIMER);                             \
    uart_putdec(UART, now_cycles);                                             \
    uart_puts(UART, "\n");                                                     \
  } while (0);

// ============================================================================
// Ringbuffer Alignment Messages
// ============================================================================

#define PRINT_ALIGN_START(UART)                                                \
  do {                                                                         \
    uart_puts(UART, "========================================\n");             \
    uart_puts(UART, "Starting ringbuffer alignment...\n");                     \
    uart_puts(UART, "========================================\n");             \
  } while (0)

#define PRINT_ALIGN_ITERATION(UART, ITER)                                      \
  do {                                                                         \
    uart_puts(UART, "\nAlign iteration ");                                     \
    uart_putdec(UART, (uint64_t)(ITER));                                       \
    uart_puts(UART, ":\n");                                                    \
  } while (0)

#define PRINT_ALIGN_STATE_CHANGE(UART, PORT, STATE, IN_OFF)                    \
  do {                                                                         \
    uart_puts(UART, "  P");                                                    \
    uart_putdec(UART, (uint64_t)(PORT));                                       \
    uart_puts(UART, ": state=");                                               \
    if ((STATE) == RINGBUFFER_ALIGN_ANNOUNCE)                                  \
      uart_puts(UART, "ANNOUNCE");                                             \
    else if ((STATE) == RINGBUFFER_ALIGN_ACKNOWLEDGE)                          \
      uart_puts(UART, "ACK");                                                  \
    else                                                                       \
      uart_puts(UART, "EMPTY");                                                \
    uart_puts(UART, ", in_off=");                                              \
    uart_putdec_signed(UART, (IN_OFF));                                        \
    uart_puts(UART, "\n");                                                     \
  } while (0)

#define PRINT_ALIGN_OUTGOING_OFFSET(UART, PORT, OUT_OFF)                       \
  do {                                                                         \
    uart_puts(UART, "  P");                                                    \
    uart_putdec(UART, (uint64_t)(PORT));                                       \
    uart_puts(UART, ": out_off=");                                             \
    uart_putdec_signed(UART, (OUT_OFF));                                       \
    uart_puts(UART, "\n");                                                     \
  } while (0)

#define PRINT_ALIGN_PORT_ALIGNED(UART, PORT)                                   \
  do {                                                                         \
    uart_puts(UART, "  P");                                                    \
    uart_putdec(UART, (uint64_t)(PORT));                                       \
    uart_puts(UART, ": ALIGNED\n");                                            \
  } while (0)

#define PRINT_ALIGN_COMPLETE(UART, IN_OFFS, NUM_PORTS)                         \
  do {                                                                         \
    uart_puts(UART, "\n========================================\n");           \
    uart_puts(UART, "Ringbuffer alignment complete!\n");                       \
    uart_puts(UART, "Final offsets:\n");                                       \
    for (int32_t port = 0; port < (NUM_PORTS); port++) {                       \
      uart_puts(UART, "  P");                                                  \
      uart_putdec(UART, (uint64_t)port);                                       \
      uart_puts(UART, ": in=");                                                \
      uart_putdec_signed(UART, (IN_OFFS)[port]);                               \
      uart_puts(UART, "\n");                                                   \
    }                                                                          \
    uart_puts(UART, "========================================\n\n");           \
  } while (0)

// ========== =================================================================
// Pre-defined Message Strings
// ============================================================================

#define MSG_QUEUE_FULL "*** ERROR: Queue full ***\n"
#define MSG_QUEUE_EMPTY "*** ERROR: Queue empty ***\n"
#define MSG_PROTOCOL_COMPLETE "Protocol complete - all UGNs discovered!\n"
#define MSG_DEADLINE_MISS "*** Deadline miss in metacycle ***\n"

#endif // MESSAGES_H
