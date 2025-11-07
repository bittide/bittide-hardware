// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#include "vexriscv_memmap.h"
#include "bittide_timer.h"
#include "bittide_uart.h"

// Main C function demonstrating the Bittide HAL
int c_main(void) {
    // Initialize timer with memory-mapped registers
    Timer timer = timer_init(
        TIMER_COMMAND,
        TIMER_SCRATCHPAD,
        TIMER_FREQUENCY,
        TIMER_CMP_RESULT
    );

    uart_puts(UART_DATA, UART_STATUS, "Hello from C with Bittide HAL!\r\n");
    uart_puts(UART_DATA, UART_STATUS, "Running on RISC-V\r\n\r\n");

    // Display timer frequency
    uint64_t freq = timer_frequency(&timer);
    uart_puts(UART_DATA, UART_STATUS, "Timer frequency: ");
    uart_putdec(UART_DATA, UART_STATUS, freq);
    uart_puts(UART_DATA, UART_STATUS, " Hz\r\n");

    // Get current time
    Instant now = timer_now(&timer);
    uart_puts(UART_DATA, UART_STATUS, "Current time: ");
    uart_putdec(UART_DATA, UART_STATUS, instant_micros(&now));
    uart_puts(UART_DATA, UART_STATUS, " us\r\n\r\n");

    // Simple timer wait demonstration
    uart_puts(UART_DATA, UART_STATUS, "Waiting 1ms...\r\n");
    Duration wait_time = duration_from_millis(1);
    timer_wait(&timer, wait_time);
    uart_puts(UART_DATA, UART_STATUS, "Done!\r\n\r\n");

    uart_puts(UART_DATA, UART_STATUS, "Program finished successfully!\r\n");

    // Infinite loop to keep program running
    while (1) {
        // Could add echo mode here if desired
    }
}
