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
use core::fmt;
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
    micros: u64,
}

impl core::cmp::PartialEq for Instant {
    fn eq(&self, other: &Self) -> bool {
        self.micros == other.micros
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
    pub fn to_hours(&self) -> u64 {
        self.micros / ((1e6 as u64) * 60 * 60)
    }

    /// The number of whole minutes represented by this `Instant`.
    pub fn to_mins(&self) -> u64 {
        self.micros / ((1e6 as u64) * 60)
    }

    /// The number of whole seconds represented by this `Instant`.
    pub fn to_secs(&self) -> u64 {
        self.micros / (1e6 as u64)
    }

    /// The number of whole milliseconds represented by this `Instant`.
    pub fn to_millis(&self) -> u64 {
        self.micros / (1e3 as u64)
    }

    /// The number of whole microseconds represented by this `Instant`.
    pub fn to_micros(&self) -> u64 {
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
            micros: self.micros + rhs.to_micros(),
        }
    }
}

impl ops::AddAssign<Duration> for Instant {
    fn add_assign(&mut self, rhs: Duration) {
        self.micros += rhs.to_micros();
    }
}

impl ops::Sub<Duration> for Instant {
    type Output = Instant;

    fn sub(self, rhs: Duration) -> Instant {
        Instant {
            micros: self.micros - rhs.to_micros(),
        }
    }
}

impl ops::SubAssign<Duration> for Instant {
    fn sub_assign(&mut self, rhs: Duration) {
        self.micros -= rhs.to_micros();
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

#[repr(u8)]
/// Command sent to the time component. A `Freeze` tells the timer to save the current
/// counter value into its scratchpad, and a `WaitForCmp` tells the timer to stall the CPU
/// until the comparison set by `TimeCmp` is `true`.
pub enum TimerCommand {
    Freeze = 0,
    WaitForCmp = 1,
}

#[repr(u8)]
/// Comparison type for the time component to use against its internal scratchpad.
pub enum TimeCmp {
    TimeEq = 0,
    TimeNeq = 1,
    TimeLt = 2,
    TimeGt = 3,
    TimeLte = 4,
    TimeGte = 5,
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
    base_addr: *mut u32,
}

impl Clock {
    const CMD_OFFSET: usize = 0;
    const CMP_OFFSET: usize = 1;
    const SCP_OFFSET: usize = 2;
    const FRQ_OFFSET: usize = 4;
    const CRS_OFFSET: usize = 6;

    /// Create a new Clock instance.
    ///
    /// # Safety
    ///
    /// `addr` needs to point to a mapped memory address for a timer component.
    pub unsafe fn new(base_addr: *const ()) -> Clock {
        let addr = base_addr as *mut u32;
        Clock { base_addr: addr }
    }

    /// Freezes the time counter.
    pub fn freeze(&mut self) {
        unsafe {
            self.base_addr
                .add(Self::CMD_OFFSET)
                .write_volatile(TimerCommand::Freeze as u32);
        }
    }

    pub fn wait_for_cmp(&mut self) {
        unsafe {
            self.base_addr
                .add(Self::CMD_OFFSET)
                .write_volatile(TimerCommand::WaitForCmp as u32);
        }
    }

    /// Sets the comparison type the time component uses.
    pub fn timer_cmp(&mut self, cmp: TimeCmp) {
        unsafe {
            self.base_addr
                .add(Self::CMP_OFFSET)
                .write_volatile(cmp as u32);
        }
    }

    pub fn read_scratchpad(&self) -> u64 {
        unsafe {
            self.base_addr
                .add(Self::SCP_OFFSET)
                .cast::<u64>()
                .read_volatile()
        }
    }

    pub fn write_scratchpad(&mut self, val: u64) {
        unsafe {
            self.base_addr
                .add(Self::SCP_OFFSET)
                .cast::<u64>()
                .write_volatile(val)
        }
    }

    /// Retrieves the frequency of the time counter.
    ///
    /// Returns:
    /// - The frequency of the time counter as a `u64`.
    pub fn frequency(&self) -> u64 {
        unsafe {
            self.base_addr
                .add(Self::FRQ_OFFSET)
                .cast::<u64>()
                .read_volatile()
        }
    }

    /// Retrieves the result of the currently configured clock comparison without blocking.
    pub fn get_cmp_result(&self) -> bool {
        unsafe { self.base_addr.add(Self::CRS_OFFSET).read_volatile() != 0 }
    }

    /// Retrieves the current value of the time counter by requesting the time component
    /// write the current counter to its internal scratchpad and then reading that out.
    /// This discards whatever value may already be present there.
    ///
    /// Returns:
    /// - The current value of the time counter as a `u64`.
    fn get_counter(&mut self) -> u64 {
        self.freeze();
        self.read_scratchpad()
    }

    /// Update the clock and return the current Instant.
    pub fn now(&mut self) -> Instant {
        let cycles = self.get_counter();
        let frequency = self.frequency();
        Instant::from_cycles(cycles, frequency)
    }

    /// Wait for a `Duration` without stalling.
    pub fn wait(&mut self, duration: Duration) {
        let now = self.get_counter();
        let cycles = duration.to_cycles(self.frequency());
        self.wait_until_raw(now + cycles);
    }

    /// Wait until we have passed an `Instant`.
    pub fn wait_until(&mut self, target: Instant) {
        let target = target.get_cycles(self.frequency());
        self.wait_until_raw(target);
    }

    pub fn wait_until_raw(&mut self, target: u64) {
        self.write_scratchpad(target);
        self.timer_cmp(TimeCmp::TimeGte);
        while !self.get_cmp_result() {}
    }

    /// Stall the CPU until the comparison set by `timer_cmp` is `True`.
    pub fn wait_stall(&mut self, duration: Duration) {
        let now = self.get_counter();
        let duration = duration.to_cycles(self.frequency());
        self.wait_until_stall_raw(now + duration);
    }

    pub fn wait_until_stall(&mut self, target: Instant) {
        let target = target.get_cycles(self.frequency());
        self.wait_until_stall_raw(target);
    }

    pub fn wait_until_stall_raw(&mut self, target: u64) {
        self.write_scratchpad(target);
        self.timer_cmp(TimeCmp::TimeGte);
        self.wait_for_cmp();
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

impl uDisplay for Duration {
    fn fmt<W>(&self, f: &mut ufmt::Formatter<'_, W>) -> Result<(), W::Error>
    where
        W: uWrite + ?Sized,
    {
        let hours = self.to_hours();
        let mins = self.to_mins() % 60;
        let secs = self.to_secs() % 60;
        let millis = self.to_millis() % 1000;
        let micros = self.to_micros() % 1000;

        uwrite!(f, "{}:{}:{}.{}.{}", hours, mins, secs, millis, micros)
    }
}

impl fmt::Display for Duration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let hours = self.to_hours();
        let mins = self.to_mins() % 60;
        let secs = self.to_secs() % 60;
        let millis = self.to_millis() % 1000;
        let micros = self.to_micros() % 1000;

        write!(f, "{hours}:{mins}:{secs}.{millis}.{micros}")
    }
}

impl fmt::Display for Instant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let hours = self.to_hours();
        let mins = self.to_mins() % 60;
        let secs = self.to_secs() % 60;
        let millis = self.to_millis() % 1000;

        write!(f, "{hours}:{mins}:{secs}.{millis}")
    }
}
