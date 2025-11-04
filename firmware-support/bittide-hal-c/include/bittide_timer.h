// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#ifndef BITTIDE_TIMER_H
#define BITTIDE_TIMER_H

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

/// Time commands for the timer peripheral
typedef enum {
    TIME_CMD_CAPTURE = 0,
    TIME_CMD_WAIT_FOR_CMP = 1
} TimeCmd;

// ============================================================================
// Duration Functions
// ============================================================================

static inline Duration duration_from_micros(uint64_t micros) {
    Duration d = {micros};
    return d;
}

static inline Duration duration_from_millis(uint64_t millis) {
    Duration d = {millis * 1000};
    return d;
}

static inline Duration duration_from_secs(uint64_t secs) {
    Duration d = {secs * 1000000};
    return d;
}

static inline Duration duration_from_mins(uint64_t mins) {
    Duration d = {mins * 60 * 1000000};
    return d;
}

static inline Duration duration_from_hours(uint64_t hours) {
    Duration d = {hours * 60 * 60 * 1000000};
    return d;
}

static inline uint64_t duration_micros(const Duration* d) {
    return d->micros;
}

static inline uint64_t duration_millis(const Duration* d) {
    return d->micros / 1000;
}

static inline uint64_t duration_secs(const Duration* d) {
    return d->micros / 1000000;
}

static inline uint64_t duration_cycles(const Duration* d, uint64_t frequency) {
    return d->micros * (frequency / 1000000);
}

static inline Duration duration_add(Duration a, Duration b) {
    Duration result = {a.micros + b.micros};
    return result;
}

static inline Duration duration_sub(Duration a, Duration b) {
    Duration result = {a.micros - b.micros};
    return result;
}

static inline Duration duration_mul(Duration d, uint64_t scalar) {
    Duration result = {d.micros * scalar};
    return result;
}

static inline Duration duration_div(Duration d, uint64_t scalar) {
    Duration result = {d.micros / scalar};
    return result;
}

// ============================================================================
// Instant Functions
// ============================================================================

static inline Instant instant_from_cycles(uint64_t cycles, uint64_t frequency) {
    Instant i = {cycles / (frequency / 1000000)};
    return i;
}

static inline Instant instant_from_micros(uint64_t micros) {
    Instant i = {micros};
    return i;
}

static inline Instant instant_from_millis(uint64_t millis) {
    Instant i = {millis * 1000};
    return i;
}

static inline Instant instant_from_secs(uint64_t secs) {
    Instant i = {secs * 1000000};
    return i;
}

static inline Instant instant_from_mins(uint64_t mins) {
    Instant i = {mins * 60 * 1000000};
    return i;
}

static inline Instant instant_from_hours(uint64_t hours) {
    Instant i = {hours * 60 * 60 * 1000000};
    return i;
}

static inline uint64_t instant_micros(const Instant* i) {
    return i->micros;
}

static inline uint64_t instant_millis(const Instant* i) {
    return i->micros / 1000;
}

static inline uint64_t instant_secs(const Instant* i) {
    return i->micros / 1000000;
}

static inline uint64_t instant_get_cycles(const Instant* i, uint64_t frequency) {
    return i->micros * (frequency / 1000000);
}

static inline Instant instant_add(Instant i, Duration d) {
    Instant result = {i.micros + d.micros};
    return result;
}

static inline Instant instant_sub_duration(Instant i, Duration d) {
    Instant result = {i.micros - d.micros};
    return result;
}

static inline Duration instant_sub_instant(Instant a, Instant b) {
    Duration result = {a.micros - b.micros};
    return result;
}

static inline Instant instant_end_of_time(void) {
    Instant i = {UINT64_MAX};
    return i;
}

// ============================================================================
// Timer Device Functions
// ============================================================================

/// Timer device structure (populated with memory-mapped register pointers)
typedef struct {
    volatile uint8_t*  command;
    volatile uint64_t* scratchpad;
    volatile uint64_t* frequency;
    volatile uint8_t*  cmp_result;
} Timer;

/// Initialize a Timer structure with memory-mapped register addresses
/// This should be called with the base addresses from the generated memmap headers
static inline Timer timer_init(
    volatile uint8_t*  command,
    volatile uint64_t* scratchpad,
    volatile uint64_t* frequency,
    volatile uint8_t*  cmp_result
) {
    Timer timer = {
        .command = command,
        .scratchpad = scratchpad,
        .frequency = frequency,
        .cmp_result = cmp_result
    };
    return timer;
}

/// Get timer frequency in Hz
static inline uint64_t timer_frequency(const Timer* timer) {
    return *timer->frequency;
}

/// Get the scratchpad value
static inline uint64_t timer_scratchpad(const Timer* timer) {
    return *timer->scratchpad;
}

/// Set the scratchpad value
static inline void timer_set_scratchpad(const Timer* timer, uint64_t value) {
    *timer->scratchpad = value;
}

/// Set the timer command
static inline void timer_set_command(const Timer* timer, TimeCmd cmd) {
    *timer->command = (uint8_t)cmd;
}

/// Get the comparison result
static inline uint8_t timer_cmp_result(const Timer* timer) {
    return *timer->cmp_result;
}

/// Freeze the time counter (capture current value to scratchpad)
static inline void timer_freeze(const Timer* timer) {
    timer_set_command(timer, TIME_CMD_CAPTURE);
}

/// Wait for comparison to become true (stalling wait)
static inline void timer_wait_for_cmp(const Timer* timer) {
    timer_set_command(timer, TIME_CMD_WAIT_FOR_CMP);
}

/// Get the current counter value
static inline uint64_t timer_get_counter(const Timer* timer) {
    timer_freeze(timer);
    return timer_scratchpad(timer);
}

/// Get current time as an Instant
static inline Instant timer_now(const Timer* timer) {
    uint64_t cycles = timer_get_counter(timer);
    uint64_t frequency = timer_frequency(timer);
    return instant_from_cycles(cycles, frequency);
}

/// Wait for a duration (non-stalling)
static inline void timer_wait(const Timer* timer, Duration duration) {
    uint64_t now = timer_get_counter(timer);
    uint64_t frequency = timer_frequency(timer);
    uint64_t cycles = duration_cycles(&duration, frequency);
    uint64_t target = now + cycles;

    timer_set_scratchpad(timer, target);
    while (!timer_cmp_result(timer)) {
        // Busy wait
    }
}

/// Wait until a specific instant (non-stalling)
static inline WaitResult timer_wait_until(const Timer* timer, Instant target) {
    uint64_t now = timer_get_counter(timer);
    uint64_t frequency = timer_frequency(timer);
    uint64_t target_cycles = instant_get_cycles(&target, frequency);

    if (now > target_cycles) {
        return WAIT_ALREADY_PASSED;
    }

    timer_set_scratchpad(timer, target_cycles);
    while (!timer_cmp_result(timer)) {
        // Busy wait
    }

    return WAIT_SUCCESS;
}

/// Wait for a duration (stalling CPU)
static inline void timer_wait_stall(const Timer* timer, Duration duration) {
    uint64_t now = timer_get_counter(timer);
    uint64_t frequency = timer_frequency(timer);
    uint64_t cycles = duration_cycles(&duration, frequency);
    uint64_t target = now + cycles;

    timer_set_scratchpad(timer, target);
    timer_wait_for_cmp(timer);
}

/// Wait until a specific instant (stalling CPU)
static inline WaitResult timer_wait_until_stall(const Timer* timer, Instant target) {
    uint64_t now = timer_get_counter(timer);
    uint64_t frequency = timer_frequency(timer);
    uint64_t target_cycles = instant_get_cycles(&target, frequency);

    if (now > target_cycles) {
        return WAIT_ALREADY_PASSED;
    }

    timer_set_scratchpad(timer, target_cycles);
    timer_wait_for_cmp(timer);

    return WAIT_SUCCESS;
}

#endif // BITTIDE_TIMER_H
