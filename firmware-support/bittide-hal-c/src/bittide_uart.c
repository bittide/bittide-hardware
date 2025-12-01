// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#include "bittide_uart.h"

Uart uart_init(volatile uint8_t *data, volatile uint8_t *status) {
  Uart uart = {.data = data, .status = status};
  return uart;
}

int uart_tx_full(const Uart *uart) {
  return *uart->status & UART_STATUS_TX_FULL;
}

int uart_rx_empty(const Uart *uart) {
  return *uart->status & UART_STATUS_RX_EMPTY;
}

void uart_putc(const Uart *uart, char c) {
  while (uart_tx_full(uart)) {
    // Wait for TX buffer to be ready
  }
  *uart->data = c;
}

void uart_puts(const Uart *uart, const char *s) {
  while (*s) {
    uart_putc(uart, *s++);
  }
}

char uart_getc(const Uart *uart) {
  while (uart_rx_empty(uart)) {
    // Wait for RX buffer to have data
  }
  return *uart->data;
}

void uart_puthex32(const Uart *uart, uint32_t val) {
  const char hex[] = "0123456789abcdef";
  uart_puts(uart, "0x");
  for (int i = 28; i >= 0; i -= 4) {
    uart_putc(uart, hex[(val >> i) & 0xf]);
  }
}

void uart_puthex64(const Uart *uart, uint64_t val) {
  const char hex[] = "0123456789abcdef";
  uart_puts(uart, "0x");
  for (int i = 60; i >= 0; i -= 4) {
    uart_putc(uart, hex[(val >> i) & 0xf]);
  }
}

void uart_putdna(const Uart *uart, const dna_t val) {
  const char hex[] = "0123456789abcdef";
  // DNA is 96 bits stored as 3x32-bit words
  // Print from most significant to least significant word
  for (int word = 2; word >= 0; word--) {
    for (int i = 28; i >= 0; i -= 4) {
      uart_putc(uart, hex[(val[word] >> i) & 0xf]);
    }
  }
}

void uart_putdec(const Uart *uart, uint64_t val) {
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
