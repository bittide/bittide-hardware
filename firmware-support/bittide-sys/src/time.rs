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

use core::cmp;
use core::ops;
use ufmt::derive::uDebug;
use ufmt::uDisplay;
use ufmt::uWrite;
use ufmt::uwrite;
pub mod self_test;

/// A representation of an absolute time value.
///
/// The `Instant` type is a wrapper around a `u64` value that represents the number
/// of clock cycles since system startup.
///
#[derive(uDebug, Copy, Clone)]
pub struct Instant {
    clock_cycles: u64,
    frequency: u64,
}

impl core::cmp::PartialEq for Instant {
    fn eq(&self, other: &Self) -> bool {
        self.clock_cycles == other.clock_cycles && self.frequency == other.frequency
    }
}

impl core::cmp::PartialOrd for Instant {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.to_micros().cmp(&other.to_micros()))
    }
}

impl Instant {
    /// Create a new `Instant` from a number of microseconds and the frequency of the timing peripheral.
    pub fn from_cycles(cycles: u64, frequency: u64) -> Instant {
        if frequency == 0 {
            panic!("Tried to create an Instant with a frequency of 0 hertz.")
        }
        Instant {
            clock_cycles: cycles,
            frequency,
        }
    }
    /// Create a new `Instant` from a number of microseconds and the frequency of the timing peripheral.
    pub fn from_micros(micros: u64, frequency: u64) -> Instant {
        if frequency == 0 {
            panic!("Tried to create an Instant with a frequency of 0 hertz.")
        }
        Instant {
            clock_cycles: (micros * frequency / 1e6 as u64),
            frequency,
        }
    }

    /// Create a new `Instant` from a number of milliseconds and the frequency of the timing peripheral.
    pub fn from_millis(millis: u64, frequency: u64) -> Instant {
        if frequency == 0 {
            panic!("Tried to create an Instant with a frequency of 0 hertz.")
        }
        Instant {
            clock_cycles: (millis * frequency / 1e3 as u64),
            frequency,
        }
    }

    /// Create a new `Instant` from a number of seconds and the frequency of the timing peripheral.
    pub fn from_secs(secs: u64, frequency: u64) -> Instant {
        if frequency == 0 {
            panic!("Tried to create an Instant with a frequency of 0 hertz.")
        }
        Instant {
            clock_cycles: secs * frequency,
            frequency,
        }
    }

    /// Create a new `Instant` from a number of minutes and the frequency of the timing peripheral.
    pub fn from_mins(mins: u64, frequency: u64) -> Instant {
        if frequency == 0 {
            panic!("Tried to create an Instant with a frequency of 0 hertz.")
        }
        Instant {
            clock_cycles: mins * (60 * frequency),
            frequency,
        }
    }

    /// Create a new `Instant` from a number of hours and the frequency of the timing peripheral.
    pub fn from_hours(hours: u64, frequency: u64) -> Instant {
        if frequency == 0 {
            panic!("Tried to create an Instant with a frequency of 0 hertz.")
        }
        Instant {
            clock_cycles: hours * (60 * 60 * frequency),
            frequency,
        }
    }

    /// The number of whole hours represented by this `Instant`.
    pub fn to_hours(&self) -> u64 {
        self.clock_cycles / (60 * 60 * self.frequency)
    }

    /// The number of whole minutes represented by this `Instant`.
    pub fn to_mins(&self) -> u64 {
        self.clock_cycles / (60 * self.frequency)
    }

    /// The number of whole seconds represented by this `Instant`.
    pub fn to_secs(&self) -> u64 {
        self.clock_cycles / self.frequency
    }

    /// The number of whole milliseconds represented by this `Instant`.
    pub fn to_millis(&self) -> u64 {
        (self.clock_cycles * 1e3 as u64) / self.frequency
    }

    /// The number of whole microseconds represented by this `Instant`.
    pub fn to_micros(&self) -> u64 {
        (self.clock_cycles * 1e6 as u64) / self.frequency
    }

    /// The number of cycles stored by this `Instant`.
    pub fn get_cycles(&self) -> u64 {
        self.clock_cycles
    }

    /// The frequency corresponding to this `Instant`s clock cycles.
    pub fn get_frequency(&self) -> u64 {
        self.frequency
    }
}

impl ops::Add<Duration> for Instant {
    type Output = Instant;

    fn add(self, rhs: Duration) -> Instant {
        Instant {
            clock_cycles: self.clock_cycles + rhs.to_cycles(self.frequency),
            frequency: self.frequency,
        }
    }
}

impl ops::AddAssign<Duration> for Instant {
    fn add_assign(&mut self, rhs: Duration) {
        self.clock_cycles += rhs.to_cycles(self.frequency)
    }
}

impl ops::Sub<Duration> for Instant {
    type Output = Instant;

    fn sub(self, rhs: Duration) -> Instant {
        Instant {
            clock_cycles: self.clock_cycles - rhs.to_cycles(self.frequency),
            frequency: self.frequency,
        }
    }
}

impl ops::SubAssign<Duration> for Instant {
    fn sub_assign(&mut self, rhs: Duration) {
        self.clock_cycles -= rhs.to_cycles(self.frequency)
    }
}

impl ops::Sub<Instant> for Instant {
    type Output = Duration;

    fn sub(self, rhs: Instant) -> Duration {
        Duration::from_micros(self.to_micros() - rhs.to_micros())
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
    pub const fn to_hours(&self) -> u64 {
        self.micros / ((1e6 as u64) * 60 * 60)
    }

    /// The number of whole minutes represented by this Duration.
    pub const fn to_mins(&self) -> u64 {
        self.micros / ((1e6 as u64) * 60)
    }

    /// The number of whole seconds represented by this Duration.
    pub const fn to_secs(&self) -> u64 {
        self.micros / (1e6 as u64)
    }

    /// The number of whole milliseconds represented by this Duration.
    pub const fn to_millis(&self) -> u64 {
        self.micros / (1e3 as u64)
    }

    /// The number of whole microseconds represented by this Duration.
    pub const fn to_micros(&self) -> u64 {
        self.micros
    }

    /// The number of clock cycles represented by this Duration given the frequency.
    pub fn to_cycles(&self, frequency: u64) -> u64 {
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

/// The Clock struct is a hardware abstraction for the timekeeping peripheral.
/// It provides methods for waiting and updating time.
/// Typical usage:
/// use bittide_sys::time::{Clock, Duration};
///
/// let addr = 0x1000_0000 as *const u32;
/// let clock = unsafe{ Clock::new(addr) };
/// let now = clock.elapsed();
/// clock.wait(Duration::from_millis(1));
/// let later = clock.elapsed();
/// let duration = later - now;
/// assert!(duration > Duration::from_millis(1));

#[derive(uDebug, Clone)]
pub struct Clock {
    freeze_count: *mut u32,
    counter: *const u64,
    frequency: *const u64,
}

impl Clock {
    /// Create a new Clock instance.
    ///
    /// # Safety
    ///
    /// `addr` needs to point to a mapped memory address for a timer component.
    pub unsafe fn new(addr: *const u32) -> Clock {
        unsafe {
            Clock {
                freeze_count: addr.cast_mut(),
                counter: addr.add(1).cast::<u64>(),
                frequency: addr.add(3).cast::<u64>(),
            }
        }
    }

    /// Wait for a `Duration`.
    pub fn wait(&self, duration: Duration) {
        let mut now = self.elapsed();
        let target = now + duration;
        while target > now {
            now = self.elapsed();
        }
    }

    /// Wait until we have passed an `Instant`.
    pub fn wait_until(&self, target: Instant) {
        let mut now = self.elapsed();
        while target > now {
            now = self.elapsed();
        }
    }

    /// Update the clock and return the current Instant.
    pub fn elapsed(&self) -> Instant {
        self.freeze();
        Instant {
            clock_cycles: self.get_counter(),
            frequency: self.get_frequency(),
        }
    }

    /// Freezes the time counter.
    fn freeze(&self) {
        unsafe {
            self.freeze_count.write_volatile(0);
        }
    }

    /// Retrieves the current value of the time counter.
    ///
    /// Returns:
    /// - The current value of the time counter as a `u64`.
    fn get_counter(&self) -> u64 {
        unsafe { self.counter.read_volatile() }
    }

    /// Retrieves the frequency of the time counter.
    ///
    /// Returns:
    /// - The frequency of the time counter as a `u64`.
    pub fn get_frequency(&self) -> u64 {
        unsafe { self.frequency.read_volatile() }
    }
}

impl From<Instant> for smoltcp::time::Instant {
    fn from(val: Instant) -> Self {
        smoltcp::time::Instant::from_micros(val.to_micros() as i64)
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
        let hours = self.to_hours();
        let mins = self.to_mins() % 60;
        let secs = self.to_secs() % 60;
        let millis = self.to_millis() % 1000;

        uwrite!(f, "{}:{}:{}.{}", hours, mins, secs, millis)
    }
}
