// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#include "stdint.h"
#include "memmap.h"

// UART status bits
#define UART_STATUS_TX_FULL  0x01  // Transmit buffer full
#define UART_STATUS_RX_EMPTY 0x02  // Receive buffer empty

// Simple UART functions
static void uart_putc(char c) {
    // Wait until transmit buffer is not full
    while (*UART_STATUS & UART_STATUS_TX_FULL) {
        // Busy wait
    }
    *UART_DATA = c;
}

static void uart_puts(const char* s) {
    while (*s) {
        uart_putc(*s++);
    }
}

static char uart_getc(void) {
    // Wait until receive buffer is not empty
    while (*UART_STATUS & UART_STATUS_RX_EMPTY) {
        // Busy wait
    }
    return *UART_DATA;
}

static void uart_puthex(uint32_t val) {
    const char hex[] = "0123456789abcdef";
    uart_puts("0x");
    for (int i = 28; i >= 0; i -= 4) {
        uart_putc(hex[(val >> i) & 0xf]);
    }
}

// Main C function
void c_main(void) {
    uart_puts("Hello from C!\r\n");
    uart_puts("Running on RISC-V\r\n");

    // Example: print a value
    uint32_t test_val = 0xDEADBEEF;
    uart_puts("Test value: ");
    uart_puthex(test_val);
    uart_puts("\r\n");

    // Set test status to success
    *STATUS_REG = TEST_STATUS_SUCCESS;

    // Echo loop
    uart_puts("Entering echo mode...\r\n");
    while (1) {
        // Properly wait for data and echo it back
        char c = uart_getc();
        uart_putc(c);
    }
}
