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
// RingBuffer Alignment State
// ============================================================================

enum RingBufferAlignState {
  RING_BUFFER_ALIGN_EMPTY = 0,
  RING_BUFFER_ALIGN_ANNOUNCE = 0xBADC0FFEE,
  RING_BUFFER_ALIGN_ACKNOWLEDGE = 0xDEADABBA,
};

// ============================================================================
// Message Printing Macros
// ============================================================================
// These macros follow the pattern from elara_experiments for setting up
// formatted messages that can be printed to UART.

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

// Assertion macro to check that SEND and RECEIVE periods are coprime
#define ASSERT_PERIODS_COPRIME(UART, SEND_PERIOD, RECEIVE_PERIOD, BUFFER_SIZE) \
  do {                                                                         \
    uint64_t a = (SEND_PERIOD / BUFFER_SIZE);                                  \
    uint64_t b = (RECEIVE_PERIOD / BUFFER_SIZE);                               \
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

// Assertion macro to check that a period is a multiple of buffer size
#define ASSERT_PERIOD_BUFFER_MULTIPLE(UART, PERIOD, BUFFER_SIZE)               \
  do {                                                                         \
    if ((PERIOD) % (BUFFER_SIZE) != 0) {                                       \
      uart_puts(UART, "[UGN ERROR] Period ");                                  \
      uart_putdec(UART, (uint64_t)(PERIOD));                                   \
      uart_puts(UART, " is not a multiple of buffer size ");                   \
      uart_putdec(UART, (uint64_t)(BUFFER_SIZE));                              \
      uart_puts(UART, "\n");                                                   \
      while (1) {                                                              \
      }                                                                        \
    }                                                                          \
  } while (0)

// ============================================================================
// RingBuffer Alignment Messages
// ============================================================================

#define PRINT_ALIGN_START(UART)                                                \
  do {                                                                         \
    uart_puts(UART, "========================================\n");             \
    uart_puts(UART, "Starting ring_buffer alignment...\n");                    \
    uart_puts(UART, "========================================\n");             \
  } while (0)

#define PRINT_ALIGN_STATE_CHANGE(UART, ITER, PORT, STATE, IN_OFF)              \
  do {                                                                         \
    uart_puts(UART, "  [");                                                    \
    uart_putdec(UART, (uint64_t)(ITER));                                       \
    uart_puts(UART, "] P");                                                    \
    uart_putdec(UART, (uint64_t)(PORT));                                       \
    uart_puts(UART, ": state=");                                               \
    if ((STATE) == RING_BUFFER_ALIGN_ANNOUNCE)                                 \
      uart_puts(UART, "ANNOUNCE");                                             \
    else if ((STATE) == RING_BUFFER_ALIGN_ACKNOWLEDGE)                         \
      uart_puts(UART, "ACK");                                                  \
    else                                                                       \
      uart_puts(UART, "EMPTY");                                                \
    uart_puts(UART, ", in_off=");                                              \
    uart_putdec_signed(UART, (IN_OFF));                                        \
    uart_puts(UART, "\n");                                                     \
  } while (0)

// ============================================================================
// Pre-defined Message Strings
// ============================================================================

#define MSG_QUEUE_FULL "*** [ERROR] Queue full ***\n"

#endif // MESSAGES_H
