// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#ifndef BITTIDE_UART_H
#define BITTIDE_UART_H

#include <stdint.h>

// ============================================================================
// UART Status Flags
// ============================================================================

#define UART_STATUS_TX_FULL 0x01
#define UART_STATUS_RX_EMPTY 0x02

// ============================================================================
// UART Device Structure
// ============================================================================

/// UART device structure (populated with memory-mapped register pointers)
typedef struct {
  volatile uint8_t *data;
  volatile uint8_t *status;
} Uart;

// ============================================================================
// UART Function Declarations
// ============================================================================

/// Initialize a UART structure with memory-mapped register addresses
Uart uart_init(volatile uint8_t *data, volatile uint8_t *status);

/// Check if UART transmit buffer is full
int uart_tx_full(const Uart *uart);

/// Check if UART receive buffer is empty
int uart_rx_empty(const Uart *uart);

/// Put a single character to UART (blocking)
void uart_putc(const Uart *uart, char c);

/// Put a string to UART (blocking)
void uart_puts(const Uart *uart, const char *s);

/// Get a single character from UART (blocking)
char uart_getc(const Uart *uart);

/// Print a 32-bit hex value to UART
void uart_puthex32(const Uart *uart, uint32_t val);

/// Print a 64-bit hex value to UART
void uart_puthex64(const Uart *uart, uint64_t val);

/// Print a DNA value (96 bits) to UART
void uart_putdna(const Uart *uart, const dna_t val);

/// Print a decimal number to UART
void uart_putdec(const Uart *uart, uint64_t val);

#endif // BITTIDE_UART_H
