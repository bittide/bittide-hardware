// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#include "vexriscv_memmap.h"
#include "bittide_hal.h"

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

    // Get current time - check if timer is advancing
    Instant now = timer_now(&timer);
    uart_puts(UART_DATA, UART_STATUS, "Current time (1st read): ");
    uart_putdec(UART_DATA, UART_STATUS, instant_micros(&now));
    uart_puts(UART_DATA, UART_STATUS, " us\r\n");

    // Read time again to see if it's advancing
    now = timer_now(&timer);
    uart_puts(UART_DATA, UART_STATUS, "Current time (2nd read): ");
    uart_putdec(UART_DATA, UART_STATUS, instant_micros(&now));
    uart_puts(UART_DATA, UART_STATUS, " us\r\n");

    // Read raw counter value
    uint64_t counter = timer_get_counter(&timer);
    uart_puts(UART_DATA, UART_STATUS, "Raw counter value: ");
    uart_putdec(UART_DATA, UART_STATUS, counter);
    uart_puts(UART_DATA, UART_STATUS, " cycles\r\n\r\n");

    // Test Duration creation and formatting
    uart_puts(UART_DATA, UART_STATUS, "=== Testing Duration ===\r\n");

    Duration d1 = duration_from_secs(5);
    uart_puts(UART_DATA, UART_STATUS, "5 seconds = ");
    uart_putdec(UART_DATA, UART_STATUS, duration_micros(&d1));
    uart_puts(UART_DATA, UART_STATUS, " microseconds\r\n");

    Duration d2 = duration_from_millis(250);
    uart_puts(UART_DATA, UART_STATUS, "250 milliseconds = ");
    uart_putdec(UART_DATA, UART_STATUS, duration_micros(&d2));
    uart_puts(UART_DATA, UART_STATUS, " microseconds\r\n");

    // Test duration arithmetic
    Duration d3 = duration_add(d1, d2);
    uart_puts(UART_DATA, UART_STATUS, "5s + 250ms = ");
    uart_putdec(UART_DATA, UART_STATUS, duration_millis(&d3));
    uart_puts(UART_DATA, UART_STATUS, " milliseconds\r\n\r\n");

    // Test timer wait functionality (using shorter delays for simulation)
    uart_puts(UART_DATA, UART_STATUS, "=== Testing Timer Waits ===\r\n");

    Instant start_time = timer_now(&timer);
    uart_puts(UART_DATA, UART_STATUS, "Start time: ");
    uart_putdec(UART_DATA, UART_STATUS, instant_micros(&start_time));
    uart_puts(UART_DATA, UART_STATUS, " us\r\n");

    for (int i = 0; i < 3; i++) {
        Instant before = timer_now(&timer);
        uart_puts(UART_DATA, UART_STATUS, "Before wait ");
        uart_putdec(UART_DATA, UART_STATUS, i + 1);
        uart_puts(UART_DATA, UART_STATUS, ": ");
        uart_putdec(UART_DATA, UART_STATUS, instant_micros(&before));
        uart_puts(UART_DATA, UART_STATUS, " us -> Waiting 1ms... ");

        Duration wait_time = duration_from_millis(1);
        timer_wait(&timer, wait_time);

        Instant after = timer_now(&timer);
        uart_puts(UART_DATA, UART_STATUS, "After: ");
        uart_putdec(UART_DATA, UART_STATUS, instant_micros(&after));
        uart_puts(UART_DATA, UART_STATUS, " us (");

        Duration elapsed = instant_sub_instant(after, before);
        uart_puts(UART_DATA, UART_STATUS, "elapsed: ");
        uart_putdec(UART_DATA, UART_STATUS, duration_micros(&elapsed));
        uart_puts(UART_DATA, UART_STATUS, " us)\r\n");
    }

    uart_puts(UART_DATA, UART_STATUS, "\r\n");

    // Test wait_until (very short delay for simulation)
    Instant before_wait_until = timer_now(&timer);
    uart_puts(UART_DATA, UART_STATUS, "Testing wait_until (100us from now)...\r\n");
    uart_puts(UART_DATA, UART_STATUS, "  Current time: ");
    uart_putdec(UART_DATA, UART_STATUS, instant_micros(&before_wait_until));
    uart_puts(UART_DATA, UART_STATUS, " us\r\n");

    Instant target = instant_add(before_wait_until, duration_from_micros(100));
    uart_puts(UART_DATA, UART_STATUS, "  Target time:  ");
    uart_putdec(UART_DATA, UART_STATUS, instant_micros(&target));
    uart_puts(UART_DATA, UART_STATUS, " us\r\n");

    WaitResult result = timer_wait_until(&timer, target);

    Instant after_wait_until = timer_now(&timer);
    uart_puts(UART_DATA, UART_STATUS, "  After wait:   ");
    uart_putdec(UART_DATA, UART_STATUS, instant_micros(&after_wait_until));
    uart_puts(UART_DATA, UART_STATUS, " us -> ");

    if (result == WAIT_SUCCESS) {
        uart_puts(UART_DATA, UART_STATUS, "Success!\r\n");
    } else {
        uart_puts(UART_DATA, UART_STATUS, "Already passed!\r\n");
    }

    uart_puts(UART_DATA, UART_STATUS, "\r\n");

    // Test periodic timer (very short intervals for simulation)
    uart_puts(UART_DATA, UART_STATUS, "=== Periodic Timer Test (3 ticks at 100us) ===\r\n");
    Instant next_tick = timer_now(&timer);
    Duration tick_interval = duration_from_micros(100);

    for (int i = 1; i <= 3; i++) {
        next_tick = instant_add(next_tick, tick_interval);
        timer_wait_until(&timer, next_tick);

        uart_puts(UART_DATA, UART_STATUS, "Tick ");
        uart_putdec(UART_DATA, UART_STATUS, i);
        uart_puts(UART_DATA, UART_STATUS, " at ");
        uart_putdec(UART_DATA, UART_STATUS, instant_micros(&next_tick));
        uart_puts(UART_DATA, UART_STATUS, " us\r\n");
    }

    uart_puts(UART_DATA, UART_STATUS, "\r\n=== All tests complete! ===\r\n\r\n");

    // Set test status to success
    *STATUS_REGISTER_STATUS = 1;

    uart_puts(UART_DATA, UART_STATUS, "Program finished successfully!\r\n");

    // Infinite loop to keep program running
    while (1) {
        // Could add echo mode here if desired
    }
}
