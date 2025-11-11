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

// Message for missed metacycle with 2 uint32_t parameters
#define PRINT_MISSED_METACYCLE(UART_PTR, EXPECTED_MC, CURRENT_MC) \
    do { \
        uart_puts(UART_PTR, "*** Missed metacycle "); \
        uart_putdec(UART_PTR, (uint64_t)(EXPECTED_MC)); \
        uart_puts(UART_PTR, ", currently in metacycle "); \
        uart_putdec(UART_PTR, (uint64_t)(CURRENT_MC)); \
        uart_puts(UART_PTR, " ***\r\n"); \
    } while(0)

// ============================================================================
// Pre-defined Message Strings
// ============================================================================

#define MSG_QUEUE_FULL "Queue full.\n"
#define MSG_QUEUE_EMPTY "Queue empty.\n"

#endif // MESSAGES_H
