// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#ifndef BITTIDE_TIMER_H
#define BITTIDE_TIMER_H

#include "shared_devices/timer.h"
#include <stdint.h>

// ============================================================================
// Time Structures
// ============================================================================

/// A representation of an absolute time value in microseconds
typedef struct {
  uint64_t micros;
} Instant;

/// A representation of a relative time in microseconds
typedef struct {
  uint64_t micros;
} Duration;

/// Result from wait operations
typedef enum {
  WAIT_SUCCESS = 0,       // Wait was successful
  WAIT_ALREADY_PASSED = 1 // Requested instant already passed
} WaitResult;

/// Result from comparison operations (same as WaitResult)
typedef enum { COMPARE_LESS = 0, COMPARE_GREATER_EQUAL = 1 } CompareResult;

// ============================================================================
// Duration Function Declarations
// ============================================================================

Duration duration_from_micros(uint64_t micros);
Duration duration_from_millis(uint64_t millis);
Duration duration_from_secs(uint64_t secs);
Duration duration_from_mins(uint64_t mins);
Duration duration_from_hours(uint64_t hours);

uint64_t duration_micros(const Duration *d);
uint64_t duration_millis(const Duration *d);
uint64_t duration_secs(const Duration *d);
uint64_t duration_cycles(const Duration *d, uint64_t frequency);

Duration duration_add(Duration a, Duration b);
Duration duration_sub(Duration a, Duration b);
Duration duration_mul(Duration d, uint64_t scalar);
Duration duration_div(Duration d, uint64_t scalar);

// ============================================================================
// Instant Function Declarations
// ============================================================================

Instant instant_from_cycles(uint64_t cycles, uint64_t frequency);
Instant instant_from_micros(uint64_t micros);
Instant instant_from_millis(uint64_t millis);
Instant instant_from_secs(uint64_t secs);
Instant instant_from_mins(uint64_t mins);
Instant instant_from_hours(uint64_t hours);

uint64_t instant_micros(const Instant *i);
uint64_t instant_millis(const Instant *i);
uint64_t instant_secs(const Instant *i);
uint64_t instant_get_cycles(const Instant *i, uint64_t frequency);

Instant instant_add(Instant i, Duration d);
Instant instant_sub_duration(Instant i, Duration d);
Duration instant_sub_instant(Instant a, Instant b);
Instant instant_end_of_time(void);

// ============================================================================
// Conversion Function Declarations (Cycles <-> Microseconds)
// ============================================================================

uint64_t cycles_to_micros(uint64_t cycles, uint64_t frequency);
uint64_t micros_to_cycles(uint64_t micros, uint64_t frequency);
uint64_t duration_to_cycles(Duration d, uint64_t frequency);
uint64_t instant_to_cycles(Instant i, uint64_t frequency);

// ============================================================================
// Timer Function Declarations
// ============================================================================

// Cycle-based API (Low-level)
uint64_t timer_now_cycles(Timer timer);
void timer_wait_cycles(Timer timer, uint64_t cycles);
WaitResult timer_wait_until_cycles(Timer timer, uint64_t target_cycles);
void timer_wait_cycles_stall(Timer timer, uint64_t cycles);
WaitResult timer_wait_until_cycles_stall(Timer timer, uint64_t target_cycles);

// Microsecond-based API (High-level)
Instant timer_now(Timer timer);
void timer_wait(Timer timer, Duration duration);
WaitResult timer_wait_until(Timer timer, Instant target);
void timer_wait_stall(Timer timer, Duration duration);
WaitResult timer_wait_until_stall(Timer timer, Instant target);

// Timer Comparison Functions
CompareResult get_compare_result(Timer timer);
CompareResult timer_compare_cycles(Timer timer, uint64_t target_cycles);
CompareResult timer_compare(Timer timer, Instant target);

#endif // BITTIDE_TIMER_H
