// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#include "bittide_timer.h"

// Time commands for the timer peripheral (internal use)
#define TIME_CMD_CAPTURE 0
#define TIME_CMD_WAIT_FOR_CMP 1

// ============================================================================
// Duration Functions
// ============================================================================

Duration duration_from_micros(uint64_t micros) {
  Duration d = {micros};
  return d;
}

Duration duration_from_millis(uint64_t millis) {
  Duration d = {millis * 1000};
  return d;
}

Duration duration_from_secs(uint64_t secs) {
  Duration d = {secs * 1000000};
  return d;
}

Duration duration_from_mins(uint64_t mins) {
  Duration d = {mins * 60 * 1000000};
  return d;
}

Duration duration_from_hours(uint64_t hours) {
  Duration d = {hours * 60 * 60 * 1000000};
  return d;
}

uint64_t duration_micros(const Duration *d) { return d->micros; }

uint64_t duration_millis(const Duration *d) { return d->micros / 1000; }

uint64_t duration_secs(const Duration *d) { return d->micros / 1000000; }

uint64_t duration_cycles(const Duration *d, uint64_t frequency) {
  return d->micros * (frequency / 1000000);
}

Duration duration_add(Duration a, Duration b) {
  Duration result = {a.micros + b.micros};
  return result;
}

Duration duration_sub(Duration a, Duration b) {
  Duration result = {a.micros - b.micros};
  return result;
}

Duration duration_mul(Duration d, uint64_t scalar) {
  Duration result = {d.micros * scalar};
  return result;
}

Duration duration_div(Duration d, uint64_t scalar) {
  Duration result = {d.micros / scalar};
  return result;
}

// ============================================================================
// Instant Functions
// ============================================================================

Instant instant_from_cycles(uint64_t cycles, uint64_t frequency) {
  Instant i = {cycles / (frequency / 1000000)};
  return i;
}

Instant instant_from_micros(uint64_t micros) {
  Instant i = {micros};
  return i;
}

Instant instant_from_millis(uint64_t millis) {
  Instant i = {millis * 1000};
  return i;
}

Instant instant_from_secs(uint64_t secs) {
  Instant i = {secs * 1000000};
  return i;
}

Instant instant_from_mins(uint64_t mins) {
  Instant i = {mins * 60 * 1000000};
  return i;
}

Instant instant_from_hours(uint64_t hours) {
  Instant i = {hours * 60 * 60 * 1000000};
  return i;
}

uint64_t instant_micros(const Instant *i) { return i->micros; }

uint64_t instant_millis(const Instant *i) { return i->micros / 1000; }

uint64_t instant_secs(const Instant *i) { return i->micros / 1000000; }

uint64_t instant_get_cycles(const Instant *i, uint64_t frequency) {
  return i->micros * (frequency / 1000000);
}

Instant instant_add(Instant i, Duration d) {
  Instant result = {i.micros + d.micros};
  return result;
}

Instant instant_sub_duration(Instant i, Duration d) {
  Instant result = {i.micros - d.micros};
  return result;
}

Duration instant_sub_instant(Instant a, Instant b) {
  Duration result = {a.micros - b.micros};
  return result;
}

Instant instant_end_of_time(void) {
  Instant i = {UINT64_MAX};
  return i;
}

// ============================================================================
// Conversion Functions (Cycles <-> Microseconds)
// ============================================================================

uint64_t cycles_to_micros(uint64_t cycles, uint64_t frequency) {
  return cycles / (frequency / 1000000);
}

uint64_t micros_to_cycles(uint64_t micros, uint64_t frequency) {
  return micros * (frequency / 1000000);
}

uint64_t duration_to_cycles(Duration d, uint64_t frequency) {
  return micros_to_cycles(d.micros, frequency);
}

uint64_t instant_to_cycles(Instant i, uint64_t frequency) {
  return micros_to_cycles(i.micros, frequency);
}

// ============================================================================
// Timer API - Cycle-based (Low-level)
// ============================================================================

uint64_t timer_now_cycles(Timer timer) {
  timer_set_command(timer, TIME_CMD_CAPTURE);
  return timer_get_scratchpad(timer);
}

void timer_wait_cycles(Timer timer, uint64_t cycles) {
  timer_set_command(timer, TIME_CMD_CAPTURE);
  uint64_t now = timer_get_scratchpad(timer);
  uint64_t target = now + cycles;

  timer_set_scratchpad(timer, target);

  while (!timer_get_cmp_result(timer)) {
    // Busy wait
  }
}

WaitResult timer_wait_until_cycles(Timer timer, uint64_t target_cycles) {
  timer_set_command(timer, TIME_CMD_CAPTURE);
  uint64_t now = timer_get_scratchpad(timer);

  if (now > target_cycles) {
    return WAIT_ALREADY_PASSED;
  }

  timer_set_scratchpad(timer, target_cycles);
  while (!timer_get_cmp_result(timer)) {
    // Busy wait
  }

  return WAIT_SUCCESS;
}

void timer_wait_cycles_stall(Timer timer, uint64_t cycles) {
  timer_set_command(timer, TIME_CMD_CAPTURE);
  uint64_t now = timer_get_scratchpad(timer);
  uint64_t target = now + cycles;

  timer_set_scratchpad(timer, target);
  timer_set_command(timer, TIME_CMD_WAIT_FOR_CMP);
}

WaitResult timer_wait_until_cycles_stall(Timer timer, uint64_t target_cycles) {
  timer_set_command(timer, TIME_CMD_CAPTURE);
  uint64_t now = timer_get_scratchpad(timer);

  if (now > target_cycles) {
    return WAIT_ALREADY_PASSED;
  }

  timer_set_scratchpad(timer, target_cycles);
  timer_set_command(timer, TIME_CMD_WAIT_FOR_CMP);

  return WAIT_SUCCESS;
}

// ============================================================================
// Timer API - Microsecond-based (High-level)
// ============================================================================

Instant timer_now(Timer timer) {
  uint64_t cycles = timer_now_cycles(timer);
  uint64_t freq = timer_get_frequency(timer);
  Instant result = {cycles_to_micros(cycles, freq)};
  return result;
}

void timer_wait(Timer timer, Duration duration) {
  uint64_t freq = timer_get_frequency(timer);
  uint64_t cycles = duration_to_cycles(duration, freq);
  timer_wait_cycles(timer, cycles);
}

WaitResult timer_wait_until(Timer timer, Instant target) {
  uint64_t freq = timer_get_frequency(timer);
  uint64_t target_cycles = instant_to_cycles(target, freq);
  return timer_wait_until_cycles(timer, target_cycles);
}

void timer_wait_stall(Timer timer, Duration duration) {
  uint64_t freq = timer_get_frequency(timer);
  uint64_t cycles = duration_to_cycles(duration, freq);
  timer_wait_cycles_stall(timer, cycles);
}

WaitResult timer_wait_until_stall(Timer timer, Instant target) {
  uint64_t freq = timer_get_frequency(timer);
  uint64_t target_cycles = instant_to_cycles(target, freq);
  return timer_wait_until_cycles_stall(timer, target_cycles);
}

// ============================================================================
// Timer Comparison Functions
// ============================================================================

CompareResult get_compare_result(Timer timer) {
  return timer_get_cmp_result(timer) ? COMPARE_GREATER_EQUAL : COMPARE_LESS;
}

CompareResult timer_compare_cycles(Timer timer, uint64_t target_cycles) {
  timer_set_scratchpad(timer, target_cycles);
  return get_compare_result(timer);
}

CompareResult timer_compare(Timer timer, Instant target) {
  uint64_t freq = timer_get_frequency(timer);
  uint64_t target_cycles = instant_to_cycles(target, freq);
  return timer_compare_cycles(timer, target_cycles);
}
