// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#include "bittide_timer.h"
#include "bittide_uart.h"
#include "hals/vex_riscv/device_instances.h"

// Main C function demonstrating the Bittide HAL
int c_main(void) {
  Uart uart = hal.uart;
  Timer timer = hal.timer;

  uart_puts(uart, "Hello from C with Bittide HAL!\n");
  uart_puts(uart, "Running on RISC-V\n\n");

  // Display timer frequency
  uint64_t freq = timer_get_frequency(timer);
  uart_puts(uart, "Timer frequency: ");
  uart_putdec(uart, freq);
  uart_puts(uart, " Hz\n");

  // Get current time
  Instant now = timer_now(timer);
  uart_puts(uart, "Current time: ");
  uart_putdec(uart, instant_micros(&now));
  uart_puts(uart, " us\n\n");

  // Simple timer wait demonstration
  uart_puts(uart, "Waiting 1ms...\n");
  Duration wait_time = duration_from_millis(1);
  timer_wait(timer, wait_time);
  uart_puts(uart, "Done!\n\n");

  uart_puts(uart, "Program finished successfully!\n");

  // Infinite loop to keep program running
  while (1) {
    // Could add echo mode here if desired
  }
}
