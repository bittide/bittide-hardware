// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#include "bittide_timer.h"
#include "bittide_uart.h"
#include "hals/time_wb/device_instances.h"

// Helper to report test failure and halt
static void test_fail(Uart uart, const char *message) {
  uart_puts(uart, "\r\n*** TEST FAILED: ");
  uart_puts(uart, message);
  uart_puts(uart, " ***\r\n");
  while (1) {
  }
}

// Main C function testing the Timer HAL
int c_main(void) {
  Uart uart = hal.uart;
  Timer timer = hal.timer;
  uart_puts(uart, "=== C Timer HAL Self-Test ===\r\n\r\n");

  // Test 1: Timer frequency should be non-zero
  uint64_t freq = timer_get_frequency(timer);
  uart_puts(uart, "Test 1: Timer frequency = ");
  uart_putdec(uart, freq);
  uart_puts(uart, " Hz\r\n");
  if (freq == 0) {
    test_fail(uart, "Timer frequency is zero");
  }

  // Test 2: Timer should be advancing (counter > 0)
  Instant start = timer_now(timer);
  uint64_t start_micros = instant_micros(&start);
  uart_puts(uart, "Test 2: Initial time = ");
  uart_putdec(uart, start_micros);
  uart_puts(uart, " us\r\n");

  uint64_t counter = timer_now_cycles(timer);
  uart_puts(uart, "        Raw counter = ");
  uart_putdec(uart, counter);
  uart_puts(uart, " cycles\r\n");
  if (counter == 0) {
    test_fail(uart, "Timer counter is stuck at zero");
  }

  // Test 3: Timer should advance over time
  Instant second_read = timer_now(timer);
  uint64_t second_micros = instant_micros(&second_read);
  uart_puts(uart, "Test 3: Second read = ");
  uart_putdec(uart, second_micros);
  uart_puts(uart, " us\r\n");
  if (second_micros <= start_micros) {
    test_fail(uart, "Timer did not advance between reads");
  }

  // Test 4: Duration creation and arithmetic
  uart_puts(uart, "Test 4: Duration arithmetic\r\n");
  Duration d1 = duration_from_secs(5);
  uint64_t d1_micros = duration_micros(&d1);
  uart_puts(uart, "        5 seconds = ");
  uart_putdec(uart, d1_micros);
  uart_puts(uart, " us\r\n");
  if (d1_micros != 5000000) {
    test_fail(uart, "duration_from_secs incorrect");
  }

  Duration d2 = duration_from_millis(250);
  uint64_t d2_micros = duration_micros(&d2);
  uart_puts(uart, "        250 milliseconds = ");
  uart_putdec(uart, d2_micros);
  uart_puts(uart, " us\r\n");
  if (d2_micros != 250000) {
    test_fail(uart, "duration_from_millis incorrect");
  }

  Duration d3 = duration_add(d1, d2);
  uint64_t d3_millis = duration_millis(&d3);
  uart_puts(uart, "        5s + 250ms = ");
  uart_putdec(uart, d3_millis);
  uart_puts(uart, " ms\r\n");
  if (d3_millis != 5250) {
    test_fail(uart, "duration_add incorrect");
  }

  // Test 5: Timer wait functionality
  uart_puts(uart, "Test 5: Timer wait (1ms x 3)\r\n");
  for (int i = 0; i < 3; i++) {
    Instant before = timer_now(timer);

    Duration wait_time = duration_from_millis(1);
    timer_wait(timer, wait_time);

    Instant after = timer_now(timer);

    Duration elapsed = instant_sub_instant(after, before);
    uint64_t elapsed_us = duration_micros(&elapsed);

    uart_puts(uart, "        Wait ");
    uart_putdec(uart, i + 1);
    uart_puts(uart, ": elapsed ");
    uart_putdec(uart, elapsed_us);
    uart_puts(uart, " us\r\n");

    // Verify elapsed time is at least 1ms (allow some tolerance)
    if (elapsed_us < 900) {
      test_fail(uart, "timer_wait returned too early");
    }
    if (elapsed_us > 2000) {
      test_fail(uart, "timer_wait took too long");
    }
  }

  // Test 6: wait_until functionality
  uart_puts(uart, "Test 6: wait_until (500us)\r\n");

  // Get time and compute target BEFORE printing to avoid timing issues
  Instant before_wait_until = timer_now(timer);
  Instant target = instant_add(before_wait_until, duration_from_micros(500));

  // Now we can print
  uint64_t before_us = instant_micros(&before_wait_until);
  uint64_t target_us = instant_micros(&target);
  uart_puts(uart, "        Before: ");
  uart_putdec(uart, before_us);
  uart_puts(uart, " us, Target: ");
  uart_putdec(uart, target_us);
  uart_puts(uart, " us\r\n");

  WaitResult result = timer_wait_until(timer, target);
  if (result != WAIT_SUCCESS) {
    test_fail(uart, "wait_until failed (target already passed)");
  }

  Instant after_wait_until = timer_now(timer);
  uint64_t after_us = instant_micros(&after_wait_until);
  uart_puts(uart, "        After: ");
  uart_putdec(uart, after_us);
  uart_puts(uart, " us\r\n");

  // Verify we waited until at least the target time
  if (after_us < target_us) {
    test_fail(uart, "wait_until returned before target");
  }

  // Test 7: Periodic timer (verify timing consistency)
  uart_puts(uart, "Test 7: Periodic timer (3 ticks at 100us)\r\n");
  Instant next_tick = timer_now(timer);
  Duration tick_interval = duration_from_micros(100);

  for (int i = 1; i <= 3; i++) {
    next_tick = instant_add(next_tick, tick_interval);
    WaitResult res = timer_wait_until(timer, next_tick);
    if (res != WAIT_SUCCESS) {
      test_fail(uart, "Periodic timer wait_until failed");
    }

    Instant actual = timer_now(timer);
    uint64_t actual_us = instant_micros(&actual);
    uint64_t expected_us = instant_micros(&next_tick);

    uart_puts(uart, "        Tick ");
    uart_putdec(uart, i);
    uart_puts(uart, " at ");
    uart_putdec(uart, actual_us);
    uart_puts(uart, " us\r\n");

    // Verify actual time is close to expected
    if (actual_us < expected_us) {
      test_fail(uart, "Periodic timer tick arrived early");
    }
  }

  // Test 8: WaitResult returns WAIT_ALREADY_PASSED for past times
  uart_puts(uart, "Test 8: WaitResult behavior for past times\r\n");

  // Create a time that's definitely in the past (time zero)
  Instant past_time = instant_from_micros(0);

  WaitResult past_result = timer_wait_until(timer, past_time);
  if (past_result != WAIT_ALREADY_PASSED) {
    test_fail(uart,
              "wait_until should return WAIT_ALREADY_PASSED for past times");
  }
  uart_puts(uart, "        Result: WAIT_ALREADY_PASSED (correct)\r\n");

  // Test 9: WaitResult returns SUCCESS for future times
  uart_puts(uart, "Test 9: WaitResult behavior for future times\r\n");

  Instant current = timer_now(timer);
  Instant future_time = instant_add(current, duration_from_micros(100));

  WaitResult future_result = timer_wait_until(timer, future_time);
  if (future_result != WAIT_SUCCESS) {
    test_fail(uart, "wait_until should return WAIT_SUCCESS for future times");
  }
  uart_puts(uart, "        Result: WAIT_SUCCESS (correct)\r\n");

  Instant after_future = timer_now(timer);
  // Verify we actually waited
  if (after_future.micros < future_time.micros) {
    test_fail(uart, "wait_until returned before target time");
  }

  // Test 10: WaitResult behavior for wait_until_cycles (low-level API)
  uart_puts(uart, "Test 10: WaitResult for cycle-based API\r\n");

  // Try to wait for cycles in the past
  uint64_t past_cycles = 0;

  WaitResult cycles_past_result = timer_wait_until_cycles(timer, past_cycles);
  if (cycles_past_result != WAIT_ALREADY_PASSED) {
    test_fail(uart,
              "wait_until_cycles should return WAIT_ALREADY_PASSED for past");
  }
  uart_puts(uart, "        Result: WAIT_ALREADY_PASSED (correct)\r\n");

  // Get fresh reading, compute target, and wait immediately (minimal delay)
  uint64_t current_cycles = timer_now_cycles(timer);
  uint64_t future_cycles = current_cycles + micros_to_cycles(100, freq);
  WaitResult cycles_future_result =
      timer_wait_until_cycles(timer, future_cycles);

  // Now print the results

  if (cycles_future_result != WAIT_SUCCESS) {
    test_fail(uart, "wait_until_cycles should return WAIT_SUCCESS for future");
  }
  uart_puts(uart, "        Result: WAIT_SUCCESS (correct)\r\n");

  uint64_t after_cycles = timer_now_cycles(timer);

  if (after_cycles < future_cycles) {
    uart_puts(uart, "        After wait: ");
    uart_putdec(uart, after_cycles);
    uart_puts(uart, " cycles\r\n");
    test_fail(uart, "wait_until_cycles returned before target");
  }

  // Test 11: WaitResult behavior for stalling variants
  uart_puts(uart, "Test 11: WaitResult for stalling wait variants\r\n");

  // Use time zero as the past time to avoid underflow
  Instant stall_past = instant_from_micros(0);

  uart_puts(uart, "        Testing wait_until_stall with past time\r\n");
  WaitResult stall_past_result = timer_wait_until_stall(timer, stall_past);
  if (stall_past_result != WAIT_ALREADY_PASSED) {
    test_fail(uart,
              "wait_until_stall should return WAIT_ALREADY_PASSED for past");
  }
  uart_puts(uart, "        Result: WAIT_ALREADY_PASSED (correct)\r\n");

  Instant stall_current = timer_now(timer);
  Instant stall_future = instant_add(stall_current, duration_from_micros(100));

  uart_puts(uart, "        Testing wait_until_stall with future time\r\n");
  WaitResult stall_future_result = timer_wait_until_stall(timer, stall_future);
  if (stall_future_result != WAIT_SUCCESS) {
    test_fail(uart, "wait_until_stall should return WAIT_SUCCESS for future");
  }
  uart_puts(uart, "        Result: WAIT_SUCCESS (correct)\r\n");

  uart_puts(uart, "\r\n=== All tests PASSED! ===\r\n\r\n");
  uart_puts(uart, "C Timer HAL test completed successfully!\r\n");

  // Infinite loop to keep program running
  while (1) {
    // Could add echo mode here if desired
  }
}
