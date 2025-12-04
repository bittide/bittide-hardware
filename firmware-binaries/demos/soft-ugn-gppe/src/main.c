// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#include "bittide_dna.h"
#include "bittide_uart.h"
#include "hals/soft_ugn_demo_gppe/device_instances.h"

int c_main(void) {
  Uart uart = hal.uart;

  dna_t dna_value;
  dna_read(hal.dna, dna_value);

  uart_puts(uart, "DNA: ");
  uart_putdna(uart, dna_value);
  uart_puts(uart, "\n");

  uart_puts(uart, "Running on RISC-V\n");
  uart_puts(uart, "Hello from C!\n");

  while (1) {
  }
}
