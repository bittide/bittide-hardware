// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#include "bittide_uart.h"
#include "shared_devices/uart.h"

int uart_tx_full(Uart uart) {
  return uart_get_status(uart) & UART_STATUS_TX_FULL;
}

int uart_rx_empty(Uart uart) {
  return uart_get_status(uart) & UART_STATUS_RX_EMPTY;
}

void uart_putc(Uart uart, char c) {
  while (uart_tx_full(uart)) {
    // Wait for TX buffer to be ready
  }
  uart_set_data(uart, c);
}

void uart_puts(Uart uart, const char *s) {
  while (*s) {
    uart_putc(uart, *s++);
  }
}

char uart_getc(Uart uart) {
  while (uart_rx_empty(uart)) {
    // Wait for RX buffer to have data
  }
  return uart_get_data(uart);
}
void uart_puthex32(Uart uart, uint32_t val) {
  const char hex[] = "0123456789abcdef";
  uart_puts(uart, "0x");
  for (int i = 28; i >= 0; i -= 4) {
    uart_putc(uart, hex[(val >> i) & 0xf]);
  }
}

void uart_puthex64(Uart uart, uint64_t val) {
  const char hex[] = "0123456789abcdef";
  uart_puts(uart, "0x");
  for (int i = 60; i >= 0; i -= 4) {
    uart_putc(uart, hex[(val >> i) & 0xf]);
  }
}

void uart_puthex64_bv(Uart uart, uint8_t val[8]) {
  const char hex[] = "0123456789abcdef";
  uart_puts(uart, "0x");
  for (int i = 0; i < 8; i++) {
    uint8_t byte = val[i];
    uint8_t nibbles[2] = {(byte & 0xF0) >> 4, (byte & 0xF)};
    uart_putc(uart, hex[nibbles[0]]);
    uart_putc(uart, hex[nibbles[1]]);
  }
}

void uart_putdna(Uart uart, const dna_t val) {
  const char hex[] = "0123456789abcdef";
  // DNA is 96 bits stored as 3x32-bit words
  // Print from most significant to least significant word
  for (int word = 2; word >= 0; word--) {
    for (int i = 28; i >= 0; i -= 4) {
      uart_putc(uart, hex[(val[word] >> i) & 0xf]);
    }
  }
}
void uart_putdec(Uart uart, uint64_t val) {
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

void uart_putdec_signed(Uart uart, int64_t val) {
  if (val < 0) {
    uart_putc(uart, '-');
    // Handle most negative value specially to avoid overflow
    // INT64_MIN is -9223372036854775808, which can't be negated in int64_t
    if (val == INT64_MIN) {
      uart_putdec(uart, 9223372036854775808ULL);
    } else {
      uart_putdec(uart, (uint64_t)(-val));
    }
  } else {
    uart_putdec(uart, (uint64_t)val);
  }
}
