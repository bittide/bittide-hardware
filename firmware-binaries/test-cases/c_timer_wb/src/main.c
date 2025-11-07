// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#include "timewb_memmap.h"
#include "bittide_timer.h"
#include "bittide_uart.h"

// Test status values (must match Haskell TestStatus enum)
#define TEST_RUNNING 0
#define TEST_SUCCESS 1
#define TEST_FAIL 2

// Helper to report test failure and halt
static void test_fail(const char* message) {
    uart_puts(UART_DATA, UART_STATUS, "\r\n*** TEST FAILED: ");
    uart_puts(UART_DATA, UART_STATUS, message);
    uart_puts(UART_DATA, UART_STATUS, " ***\r\n");
    while(1) {}
}

// Main C function testing the Timer HAL
int c_main(void) {
    // Initialize timer with memory-mapped registers
    Timer timer = timer_init(
        TIMER_COMMAND,
        TIMER_SCRATCHPAD,
        TIMER_FREQUENCY,
        TIMER_CMP_RESULT
    );

    uart_puts(UART_DATA, UART_STATUS, "=== C Timer HAL Self-Test ===\r\n\r\n");

    // Test 1: Timer frequency should be non-zero
    uint64_t freq = timer_frequency(&timer);
    uart_puts(UART_DATA, UART_STATUS, "Test 1: Timer frequency = ");
    uart_putdec(UART_DATA, UART_STATUS, freq);
    uart_puts(UART_DATA, UART_STATUS, " Hz\r\n");
    if (freq == 0) {
        test_fail("Timer frequency is zero");
    }

    // Test 2: Timer should be advancing (counter > 0)
    Instant start = timer_now(&timer);
    uint64_t start_micros = instant_micros(&start);
    uart_puts(UART_DATA, UART_STATUS, "Test 2: Initial time = ");
    uart_putdec(UART_DATA, UART_STATUS, start_micros);
    uart_puts(UART_DATA, UART_STATUS, " us\r\n");

    uint64_t counter = timer_get_counter(&timer);
    uart_puts(UART_DATA, UART_STATUS, "        Raw counter = ");
    uart_putdec(UART_DATA, UART_STATUS, counter);
    uart_puts(UART_DATA, UART_STATUS, " cycles\r\n");
    if (counter == 0) {
        test_fail("Timer counter is stuck at zero");
    }

    // Test 3: Timer should advance over time
    Instant second_read = timer_now(&timer);
    uint64_t second_micros = instant_micros(&second_read);
    uart_puts(UART_DATA, UART_STATUS, "Test 3: Second read = ");
    uart_putdec(UART_DATA, UART_STATUS, second_micros);
    uart_puts(UART_DATA, UART_STATUS, " us\r\n");
    if (second_micros <= start_micros) {
        test_fail("Timer did not advance between reads");
    }

    // Test 4: Duration creation and arithmetic
    uart_puts(UART_DATA, UART_STATUS, "Test 4: Duration arithmetic\r\n");
    Duration d1 = duration_from_secs(5);
    uint64_t d1_micros = duration_micros(&d1);
    uart_puts(UART_DATA, UART_STATUS, "        5 seconds = ");
    uart_putdec(UART_DATA, UART_STATUS, d1_micros);
    uart_puts(UART_DATA, UART_STATUS, " us\r\n");
    if (d1_micros != 5000000) {
        test_fail("duration_from_secs incorrect");
    }

    Duration d2 = duration_from_millis(250);
    uint64_t d2_micros = duration_micros(&d2);
    uart_puts(UART_DATA, UART_STATUS, "        250 milliseconds = ");
    uart_putdec(UART_DATA, UART_STATUS, d2_micros);
    uart_puts(UART_DATA, UART_STATUS, " us\r\n");
    if (d2_micros != 250000) {
        test_fail("duration_from_millis incorrect");
    }

    Duration d3 = duration_add(d1, d2);
    uint64_t d3_millis = duration_millis(&d3);
    uart_puts(UART_DATA, UART_STATUS, "        5s + 250ms = ");
    uart_putdec(UART_DATA, UART_STATUS, d3_millis);
    uart_puts(UART_DATA, UART_STATUS, " ms\r\n");
    if (d3_millis != 5250) {
        test_fail("duration_add incorrect");
    }

    // Test 5: Timer wait functionality
    uart_puts(UART_DATA, UART_STATUS, "Test 5: Timer wait (1ms x 3)\r\n");
    for (int i = 0; i < 3; i++) {
        Instant before = timer_now(&timer);

        Duration wait_time = duration_from_millis(1);
        timer_wait(&timer, wait_time);

        Instant after = timer_now(&timer);

        Duration elapsed = instant_sub_instant(after, before);
        uint64_t elapsed_us = duration_micros(&elapsed);

        uart_puts(UART_DATA, UART_STATUS, "        Wait ");
        uart_putdec(UART_DATA, UART_STATUS, i + 1);
        uart_puts(UART_DATA, UART_STATUS, ": elapsed ");
        uart_putdec(UART_DATA, UART_STATUS, elapsed_us);
        uart_puts(UART_DATA, UART_STATUS, " us\r\n");

        // Verify elapsed time is at least 1ms (allow some tolerance)
        if (elapsed_us < 900) {
            test_fail("timer_wait returned too early");
        }
        if (elapsed_us > 2000) {
            test_fail("timer_wait took too long");
        }
    }

    // Test 6: wait_until functionality
    uart_puts(UART_DATA, UART_STATUS, "Test 6: wait_until (100us)\r\n");
    Instant before_wait_until = timer_now(&timer);
    uint64_t before_us = instant_micros(&before_wait_until);

    Instant target = instant_add(before_wait_until, duration_from_micros(100));
    uint64_t target_us = instant_micros(&target);

    uart_puts(UART_DATA, UART_STATUS, "        Before: ");
    uart_putdec(UART_DATA, UART_STATUS, before_us);
    uart_puts(UART_DATA, UART_STATUS, " us, Target: ");
    uart_putdec(UART_DATA, UART_STATUS, target_us);
    uart_puts(UART_DATA, UART_STATUS, " us\r\n");

    WaitResult result = timer_wait_until(&timer, target);
    if (result != WAIT_SUCCESS) {
        test_fail("wait_until failed (target already passed)");
    }

    Instant after_wait_until = timer_now(&timer);
    uint64_t after_us = instant_micros(&after_wait_until);
    uart_puts(UART_DATA, UART_STATUS, "        After: ");
    uart_putdec(UART_DATA, UART_STATUS, after_us);
    uart_puts(UART_DATA, UART_STATUS, " us\r\n");

    // Verify we waited until at least the target time
    if (after_us < target_us) {
        test_fail("wait_until returned before target");
    }

    // Test 7: Periodic timer (verify timing consistency)
    uart_puts(UART_DATA, UART_STATUS, "Test 7: Periodic timer (3 ticks at 100us)\r\n");
    Instant next_tick = timer_now(&timer);
    Duration tick_interval = duration_from_micros(100);

    for (int i = 1; i <= 3; i++) {
        next_tick = instant_add(next_tick, tick_interval);
        WaitResult res = timer_wait_until(&timer, next_tick);
        if (res != WAIT_SUCCESS) {
            test_fail("Periodic timer wait_until failed");
        }

        Instant actual = timer_now(&timer);
        uint64_t actual_us = instant_micros(&actual);
        uint64_t expected_us = instant_micros(&next_tick);

        uart_puts(UART_DATA, UART_STATUS, "        Tick ");
        uart_putdec(UART_DATA, UART_STATUS, i);
        uart_puts(UART_DATA, UART_STATUS, " at ");
        uart_putdec(UART_DATA, UART_STATUS, actual_us);
        uart_puts(UART_DATA, UART_STATUS, " us\r\n");

        // Verify actual time is close to expected
        if (actual_us < expected_us) {
            test_fail("Periodic timer tick arrived early");
        }
    }

    uart_puts(UART_DATA, UART_STATUS, "\r\n=== All tests PASSED! ===\r\n\r\n");
    uart_puts(UART_DATA, UART_STATUS, "C Timer HAL test completed successfully!\r\n");

    // Infinite loop to keep program running
    while (1) {
        // Could add echo mode here if desired
    }
}
