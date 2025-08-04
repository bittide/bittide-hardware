// SPDX-FileCopyrightText: 2023 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
/*! Time structures.

The `time` module contains structures used to get some sense of time.
While clocks are dynamic in a Bittide System, it is expected that the deviation from
wall clock time is no worse than any on-board static clock source and the effect is
insignificant for timing purposes.

 - [`Instant`] is used to represent absolute time.
 - [`Duration`] is used to represent relative time.
 - [`Clock`] Manages time and provides utility methods for waiting and updating time.
*/

use crate::shared_devices::Timer;
use crate::types::TimeCmd;

use core::cmp;
use core::fmt;
use core::ops;
use ufmt::derive::uDebug;
use ufmt::uDisplay;
use ufmt::uWrite;
use ufmt::uwrite;

pub mod self_test;

#[derive(uDebug, Debug, PartialEq, Eq)]
pub enum WaitResult {
    /// Wait was successful and requested on time
    Success,

    /// Requested waiting on an instant that has already passed
    AlreadyPassed,
}

/// A representation of an absolute time value.
///
/// The `Instant` type is a wrapper around a `u64` value that represents the number
/// of clock cycles since system startup.
///
#[derive(uDebug, Copy, Clone)]
pub struct Instant {
    micros: u64,
}

impl core::cmp::PartialEq for Instant {
    fn eq(&self, other: &Self) -> bool {
        self.micros == other.micros
    }
}

impl core::cmp::PartialOrd for Instant {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.micros().cmp(&other.micros()))
    }
}

impl Instant {
    /// Create a new `Instant` from a number of microseconds and the frequency of the timing peripheral.
    pub fn from_cycles(cycles: u64, frequency: u64) -> Instant {
        if frequency == 0 {
            panic!("Tried to create an Instant with a frequency of 0 hertz.")
        }
        Instant {
            micros: cycles / (frequency / 1e6 as u64),
        }
    }
    /// Create a new `Instant` from a number of microseconds and the frequency of the timing peripheral.
    pub fn from_micros(micros: u64) -> Instant {
        Instant { micros }
    }

    /// Create a new `Instant` from a number of milliseconds and the frequency of the timing peripheral.
    pub fn from_millis(millis: u64) -> Instant {
        Instant {
            micros: millis * 1e3 as u64,
        }
    }

    /// Create a new `Instant` from a number of seconds and the frequency of the timing peripheral.
    pub fn from_secs(secs: u64) -> Instant {
        Instant {
            micros: secs * 1e6 as u64,
        }
    }

    /// Create a new `Instant` from a number of minutes and the frequency of the timing peripheral.
    pub fn from_mins(mins: u64) -> Instant {
        Instant {
            micros: mins * (60 * 1e6 as u64),
        }
    }

    /// Create a new `Instant` from a number of hours and the frequency of the timing peripheral.
    pub fn from_hours(hours: u64) -> Instant {
        Instant {
            micros: hours * (60 * 60 * 1e6 as u64),
        }
    }

    /// The number of whole hours represented by this `Instant`.
    pub fn hours(&self) -> u64 {
        self.micros / ((1e6 as u64) * 60 * 60)
    }

    /// The number of whole minutes represented by this `Instant`.
    pub fn mins(&self) -> u64 {
        self.micros / ((1e6 as u64) * 60)
    }

    /// The number of whole seconds represented by this `Instant`.
    pub fn secs(&self) -> u64 {
        self.micros / (1e6 as u64)
    }

    /// The number of whole milliseconds represented by this `Instant`.
    pub fn millis(&self) -> u64 {
        self.micros / (1e3 as u64)
    }

    /// The number of whole microseconds represented by this `Instant`.
    pub fn micros(&self) -> u64 {
        self.micros
    }

    /// The number of cycles stored by this `Instant`.
    pub fn get_cycles(&self, frequency: u64) -> u64 {
        self.micros * (frequency / 1e6 as u64)
    }

    // Maximum time that we can represent
    pub fn end_of_time() -> Instant {
        Instant { micros: u64::MAX }
    }
}

impl ops::Add<Duration> for Instant {
    type Output = Instant;

    fn add(self, rhs: Duration) -> Instant {
        Instant {
            micros: self.micros + rhs.micros(),
        }
    }
}

impl ops::AddAssign<Duration> for Instant {
    fn add_assign(&mut self, rhs: Duration) {
        self.micros += rhs.micros();
    }
}

impl ops::Sub<Duration> for Instant {
    type Output = Instant;

    fn sub(self, rhs: Duration) -> Instant {
        Instant {
            micros: self.micros - rhs.micros(),
        }
    }
}

impl ops::SubAssign<Duration> for Instant {
    fn sub_assign(&mut self, rhs: Duration) {
        self.micros -= rhs.micros();
    }
}

impl ops::Sub<Instant> for Instant {
    type Output = Duration;

    fn sub(self, rhs: Instant) -> Duration {
        Duration::from_micros(self.micros() - rhs.micros())
    }
}

/// A representation of a relative time, stored in microseconds.
#[derive(uDebug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Duration {
    micros: u64,
}

impl Duration {
    /// Create a new `Duration` from a number of microseconds.
    pub fn from_micros(micros: u64) -> Duration {
        Duration { micros }
    }

    /// Create a new `Duration` from a number of milliseconds.
    pub fn from_millis(millis: u64) -> Duration {
        Duration {
            micros: millis * (1e3 as u64),
        }
    }

    /// Create a new `Duration` from a number of seconds.
    pub fn from_secs(secs: u64) -> Duration {
        Duration {
            micros: secs * (1e6 as u64),
        }
    }

    /// Create a new `Duration` from a number of minutes.
    pub fn from_mins(mins: u64) -> Duration {
        Duration {
            micros: mins * (60 * 1e6 as u64),
        }
    }

    /// Create a new `Duration` from a number of hours.
    pub fn from_hours(hours: u64) -> Duration {
        Duration {
            micros: hours * (60 * 60 * 1e6 as u64),
        }
    }

    /// The number of whole hours represented by this Duration.
    pub const fn hours(&self) -> u64 {
        self.micros / ((1e6 as u64) * 60 * 60)
    }

    /// The number of whole minutes represented by this Duration.
    pub const fn mins(&self) -> u64 {
        self.micros / ((1e6 as u64) * 60)
    }

    /// The number of whole seconds represented by this Duration.
    pub const fn secs(&self) -> u64 {
        self.micros / (1e6 as u64)
    }

    /// The number of whole milliseconds represented by this Duration.
    pub const fn millis(&self) -> u64 {
        self.micros / (1e3 as u64)
    }

    /// The number of whole microseconds represented by this Duration.
    pub const fn micros(&self) -> u64 {
        self.micros
    }

    /// The number of clock cycles represented by this Duration given the frequency.
    pub fn cycles(&self, frequency: u64) -> u64 {
        (self.micros * frequency) / (1e6 as u64)
    }
}

impl ops::Add<Duration> for Duration {
    type Output = Duration;

    fn add(self, rhs: Duration) -> Duration {
        Duration {
            micros: self.micros + rhs.micros,
        }
    }
}

impl ops::AddAssign<Duration> for Duration {
    fn add_assign(&mut self, rhs: Duration) {
        self.micros += rhs.micros;
    }
}

impl ops::Sub<Duration> for Duration {
    type Output = Duration;

    fn sub(self, rhs: Duration) -> Duration {
        Duration {
            micros: self
                .micros
                .checked_sub(rhs.micros)
                .expect("overflow when subtracting durations"),
        }
    }
}

impl Timer {
    /// Freezes the time counter.
    pub fn freeze(&self) {
        self.set_command(TimeCmd::Capture);
    }

    pub fn wait_for_cmp(&self) {
        self.set_command(TimeCmd::WaitForCmp);
    }

    /// Retrieves the current value of the time counter by requesting the time component
    /// write the current counter to its internal scratchpad and then reading that out.
    /// This discards whatever value may already be present there.
    ///
    /// Returns:
    /// - The current value of the time counter as a `u64`.
    fn get_counter(&self) -> u64 {
        self.freeze();
        self.scratchpad()
    }

    /// Update the clock and return the current Instant.
    pub fn now(&self) -> Instant {
        let cycles = self.get_counter();
        let frequency = self.frequency();
        Instant::from_cycles(cycles, frequency)
    }

    /// Wait for a `Duration` without stalling.
    pub fn wait(&self, duration: Duration) {
        let now = self.get_counter();
        let cycles = duration.cycles(self.frequency());
        let _ = self.wait_until_raw(now + cycles);
    }

    /// Wait until we have passed an `Instant`.
    #[must_use]
    pub fn wait_until(&self, target: Instant) -> WaitResult {
        let target_cycles = target.get_cycles(self.frequency());
        self.wait_until_raw(target_cycles)
    }

    #[must_use]
    pub fn wait_until_raw(&self, target: u64) -> WaitResult {
        let now = self.get_counter();
        if now > target {
            return WaitResult::AlreadyPassed;
        }
        self.set_scratchpad(target);
        while !self.cmp_result() {
            continue;
        }
        WaitResult::Success
    }

    /// Stall the CPU until the comparison set by `timer_cmp` is `True`.
    pub fn wait_stall(&self, duration: Duration) {
        let now = self.get_counter();
        let duration = duration.cycles(self.frequency());
        let _ = self.wait_until_stall_raw(now + duration);
    }

    #[must_use]
    pub fn wait_until_stall(&self, target: Instant) -> WaitResult {
        let target_cycles = target.get_cycles(self.frequency());
        self.wait_until_stall_raw(target_cycles)
    }

    #[must_use]
    pub fn wait_until_stall_raw(&self, target: u64) -> WaitResult {
        let now = self.get_counter();
        if now > target {
            return WaitResult::AlreadyPassed;
        }
        self.set_scratchpad(target);
        self.wait_for_cmp();
        WaitResult::Success
    }
}

impl From<Instant> for smoltcp::time::Instant {
    fn from(val: Instant) -> Self {
        smoltcp::time::Instant::from_micros(val.micros() as i64)
    }
}

impl From<smoltcp::time::Duration> for Duration {
    fn from(duration: smoltcp::time::Duration) -> Self {
        Duration::from_micros(duration.micros())
    }
}

impl uDisplay for Instant {
    fn fmt<W>(&self, f: &mut ufmt::Formatter<'_, W>) -> Result<(), W::Error>
    where
        W: uWrite + ?Sized,
    {
        let hours = self.hours();
        let mins = self.mins() % 60;
        let secs = self.secs() % 60;
        let millis = self.millis() % 1000;

        uwrite!(f, "{}:{}:{}.{}", hours, mins, secs, millis)
    }
}

impl uDisplay for Duration {
    fn fmt<W>(&self, f: &mut ufmt::Formatter<'_, W>) -> Result<(), W::Error>
    where
        W: uWrite + ?Sized,
    {
        let hours = self.hours();
        let mins = self.mins() % 60;
        let secs = self.secs() % 60;
        let millis = self.millis() % 1000;
        let micros = self.micros() % 1000;

        uwrite!(f, "{}:{}:{}.{}.{}", hours, mins, secs, millis, micros)
    }
}

impl fmt::Display for Duration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let hours = self.hours();
        let mins = self.mins() % 60;
        let secs = self.secs() % 60;
        let millis = self.millis() % 1000;
        let micros = self.micros() % 1000;

        write!(f, "{hours}:{mins}:{secs}.{millis}.{micros}")
    }
}

impl fmt::Display for Instant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let hours = self.hours();
        let mins = self.mins() % 60;
        let secs = self.secs() % 60;
        let millis = self.millis() % 1000;

        write!(f, "{hours}:{mins}:{secs}.{millis}")
    }
}
