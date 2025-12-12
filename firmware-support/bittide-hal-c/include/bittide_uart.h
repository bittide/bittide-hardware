// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#ifndef BITTIDE_UART_H
#define BITTIDE_UART_H

#include "shared_devices/uart.h"
#include <stdint.h>

// ============================================================================
// UART Status Flags
// ============================================================================

#define UART_STATUS_TX_FULL 0x01
#define UART_STATUS_RX_EMPTY 0x02

// ============================================================================
// UART Function Declarations
// ============================================================================

/// Check if UART transmit buffer is full
int uart_tx_full(Uart uart);

/// Check if UART receive buffer is empty
int uart_rx_empty(Uart uart);

/// Put a single character to UART (blocking)
void uart_putc(Uart uart, char c);

/// Put a string to UART (blocking)
void uart_puts(Uart uart, const char *s);

/// Get a single character from UART (blocking)
char uart_getc(Uart uart);

/// Print a 32-bit hex value to UART
void uart_puthex32(Uart uart, uint32_t val);

/// Print a 64-bit hex value to UART
void uart_puthex64(Uart uart, uint64_t val);

/// Print a 64-bit bitvector hex value to UART
void uart_puthex64_bv(Uart uart, uint8_t val[8]);

/// Print a DNA value (96 bits) to UART
void uart_putdna(Uart uart, const dna_t val);

/// Print a decimal number to UART
void uart_putdec(Uart uart, uint64_t val);

/// Print a signed decimal number to UART
void uart_putdec_signed(Uart uart, int64_t val);

#endif // BITTIDE_UART_H
