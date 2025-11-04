// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#ifndef BITTIDE_UART_H
#define BITTIDE_UART_H

#include <stdint.h>

// ============================================================================
// UART Helper Functions
// ============================================================================

#define UART_STATUS_TX_FULL  0x01
#define UART_STATUS_RX_EMPTY 0x02

/// Simple UART put character
static inline void uart_putc(volatile uint8_t* uart_data, volatile uint8_t* uart_status, char c) {
    while (*uart_status & UART_STATUS_TX_FULL) {
        // Wait
    }
    *uart_data = c;
}

/// Simple UART put string
static inline void uart_puts(volatile uint8_t* uart_data, volatile uint8_t* uart_status, const char* s) {
    while (*s) {
        uart_putc(uart_data, uart_status, *s++);
    }
}

/// Simple UART get character
static inline char uart_getc(volatile uint8_t* uart_data, volatile uint8_t* uart_status) {
    while (*uart_status & UART_STATUS_RX_EMPTY) {
        // Wait
    }
    return *uart_data;
}

/// Print a 32-bit hex value to UART
static inline void uart_puthex32(volatile uint8_t* uart_data, volatile uint8_t* uart_status, uint32_t val) {
    const char hex[] = "0123456789abcdef";
    uart_puts(uart_data, uart_status, "0x");
    for (int i = 28; i >= 0; i -= 4) {
        uart_putc(uart_data, uart_status, hex[(val >> i) & 0xf]);
    }
}

/// Print a 64-bit hex value to UART
static inline void uart_puthex64(volatile uint8_t* uart_data, volatile uint8_t* uart_status, uint64_t val) {
    const char hex[] = "0123456789abcdef";
    uart_puts(uart_data, uart_status, "0x");
    for (int i = 60; i >= 0; i -= 4) {
        uart_putc(uart_data, uart_status, hex[(val >> i) & 0xf]);
    }
}

/// Print a decimal number to UART (simple implementation)
static inline void uart_putdec(volatile uint8_t* uart_data, volatile uint8_t* uart_status, uint64_t val) {
    char buf[21]; // max 20 digits for uint64_t + null
    int i = 20;
    buf[i] = '\0';

    if (val == 0) {
        uart_putc(uart_data, uart_status, '0');
        return;
    }

    while (val > 0 && i > 0) {
        buf[--i] = '0' + (val % 10);
        val /= 10;
    }

    uart_puts(uart_data, uart_status, &buf[i]);
}

#endif // BITTIDE_UART_H
