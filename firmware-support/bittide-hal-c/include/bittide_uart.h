// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#ifndef BITTIDE_UART_H
#define BITTIDE_UART_H

#include <stdint.h>

// ============================================================================
// UART Status Flags
// ============================================================================

#define UART_STATUS_TX_FULL  0x01
#define UART_STATUS_RX_EMPTY 0x02

// ============================================================================
// UART Device Structure
// ============================================================================

/// UART device structure (populated with memory-mapped register pointers)
typedef struct {
    volatile uint8_t* data;
    volatile uint8_t* status;
} Uart;

/// Initialize a UART structure with memory-mapped register addresses
/// This should be called with the base addresses from the generated memmap headers
static inline Uart uart_init(
    volatile uint8_t* data,
    volatile uint8_t* status
) {
    Uart uart = {
        .data = data,
        .status = status
    };
    return uart;
}

// ============================================================================
// UART Basic I/O Functions
// ============================================================================

/// Check if UART transmit buffer is full
static inline int uart_tx_full(const Uart* uart) {
    return *uart->status & UART_STATUS_TX_FULL;
}

/// Check if UART receive buffer is empty
static inline int uart_rx_empty(const Uart* uart) {
    return *uart->status & UART_STATUS_RX_EMPTY;
}

/// Put a single character to UART (blocking)
static inline void uart_putc(const Uart* uart, char c) {
    while (uart_tx_full(uart)) {
        // Wait for TX buffer to be ready
    }
    *uart->data = c;
}

/// Put a string to UART (blocking)
static inline void uart_puts(const Uart* uart, const char* s) {
    while (*s) {
        uart_putc(uart, *s++);
    }
}

/// Get a single character from UART (blocking)
static inline char uart_getc(const Uart* uart) {
    while (uart_rx_empty(uart)) {
        // Wait for RX buffer to have data
    }
    return *uart->data;
}

// ============================================================================
// UART Formatted Output Functions
// ============================================================================

/// Print a 32-bit hex value to UART
static inline void uart_puthex32(const Uart* uart, uint32_t val) {
    const char hex[] = "0123456789abcdef";
    uart_puts(uart, "0x");
    for (int i = 28; i >= 0; i -= 4) {
        uart_putc(uart, hex[(val >> i) & 0xf]);
    }
}

/// Print a 64-bit hex value to UART
static inline void uart_puthex64(const Uart* uart, uint64_t val) {
    const char hex[] = "0123456789abcdef";
    uart_puts(uart, "0x");
    for (int i = 60; i >= 0; i -= 4) {
        uart_putc(uart, hex[(val >> i) & 0xf]);
    }
}

/// Print a decimal number to UART
static inline void uart_putdec(const Uart* uart, uint64_t val) {
    char buf[21]; // max 20 digits for uint64_t + null
    int i = 20;
    buf[i] = '\0';

    if (val == 0) {
        uart_putc(uart, '0');
        return;
    }

    while (val > 0 && i > 0) {
        buf[--i] = '0' + (val % 10);
        val /= 10;
    }

    uart_puts(uart, &buf[i]);
}

#endif // BITTIDE_UART_H
