// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
use core::fmt::{self, Debug};
use ufmt::{uDebug, uwrite};

use crate::manual_additions::timer::{Duration, Instant, Timer, WaitResult};

type TestReturn = Option<(&'static str, Option<FailValue>)>;
type TestFn = fn(Timer) -> TestReturn;

pub enum FailValue {
    U64(u64),
    Duration(Duration),
    Instant(Instant),
}

impl Debug for FailValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FailValue::U64(val) => write!(f, "{val:?}"),
            FailValue::Duration(dur) => write!(f, "{:?}", dur.micros),
            FailValue::Instant(inst) => write!(f, "{:?}", inst.micros),
        }
    }
}

impl uDebug for FailValue {
    fn fmt<W>(&self, f: &mut ufmt::Formatter<'_, W>) -> Result<(), W::Error>
    where
        W: ufmt::uWrite + ?Sized,
    {
        match self {
            FailValue::U64(val) => uwrite!(f, "{:?}", val),
            FailValue::Duration(dur) => uwrite!(f, "{:?}", dur.micros as i64),
            FailValue::Instant(inst) => uwrite!(f, "{:?}", inst.micros),
        }
    }
}

macro_rules! tests {
    ($($testname:ident),+$(,)?) => {
        [
            $(($testname as TestFn, stringify!($testname)),)+
        ]
    };
}

/// Tests for the time module.
/// It receives a pointer to the timing peripheral and returns a list of tuples containing the
/// name of the test and an Option<&'static str> indicating if the test passed or an error message.
#[allow(dead_code)]
pub fn self_test(timer: Timer) -> impl Iterator<Item = (&'static str, TestReturn)> {
    let tests = tests!(
        now_not_null,
        freq_not_null,
        skip_next_ms,
        wait_1ms,
        wait_until_1ms,
        wait_until_stall_1ms,
        duration_hour_minute,
        duration_minute_second,
        duration_second_millisecond,
        duration_millisecond_microsecond,
        instant_hour_minute,
        instant_minute_second,
        instant_second_millisecond,
        instant_millisecond_microsecond,
    );
    // Run the tests and collect the results.
    tests
        .into_iter()
        .map(move |(f, name)| (name, f(unsafe { Timer::new(timer.0) })))
}

/// Obtain the value of the counter, check if it's not 0.
pub fn now_not_null(timer: Timer) -> TestReturn {
    let frequency = timer.frequency();
    let now = timer.now();
    if now == Instant::from_cycles(0, frequency) {
        Some((
            "now_not_null test failed: now is null",
            Some(FailValue::Instant(now)),
        ))
    } else {
        None
    }
}

/// Read the frequency value, check if it's not 0.
pub fn freq_not_null(timer: Timer) -> TestReturn {
    let frequency: u64 = timer.frequency();
    if frequency == 0 {
        Some(("freq_not_null test failed: frequency is null", None))
    } else {
        None
    }
}

/// Read the current time in milliseconds, wait a ms and read again.
/// The new time should differ less than 100 us from the expected target.
pub fn wait_1ms(timer: Timer) -> TestReturn {
    let wait_time = Duration::from_millis(1);
    let time0 = timer.now();
    timer.wait(wait_time);
    let time1 = timer.now();
    let expected = time0 + wait_time;
    let diff = time1 - expected;
    if diff >= Duration::from_micros(100) {
        Some((
            "wait_1ms test failed: time difference is too large",
            Some(FailValue::Duration(diff)),
        ))
    } else {
        None
    }
}

/// Read the current time in milliseconds, wait until that time + 1ms
/// Read out the time again and
pub fn wait_until_1ms(timer: Timer) -> TestReturn {
    let start_time = timer.now();
    let target = start_time + Duration::from_millis(1);
    let expect_passed = timer.wait_until(start_time);
    if expect_passed != WaitResult::AlreadyPassed {
        return Some((
            "wait_until_1ms test failed: start_time not already passed",
            None,
        ));
    }

    let expect_success = timer.wait_until(target);
    if expect_success != WaitResult::Success {
        return Some(("wait_until_1ms test failed: target already passed", None));
    }
    let end_time = timer.now();
    let diff = end_time - target;
    if diff >= Duration::from_micros(100) {
        Some((
            "wait_until_2ms test failed: time difference is too large",
            Some(FailValue::Duration(diff)),
        ))
    } else {
        None
    }
}

/// Read the current time in milliseconds, wait until that time + 1ms
/// Read out the time again and
pub fn wait_until_stall_1ms(timer: Timer) -> TestReturn {
    let start_time = timer.now();
    let target = start_time + Duration::from_millis(1);
    let expect_passed = timer.wait_until_stall(start_time);
    if expect_passed != WaitResult::AlreadyPassed {
        return Some((
            "wait_until_stall_1ms test failed: start_time not already passed",
            None,
        ));
    }

    let expect_success = timer.wait_until_stall(target);
    if expect_success != WaitResult::Success {
        return Some((
            "wait_until_stall_1ms test failed: target already passed",
            None,
        ));
    }
    let end_time = timer.now();
    let diff = end_time - target;
    if diff >= Duration::from_micros(100) {
        Some((
            "wait_until_stall_1ms test failed: time difference is too large",
            Some(FailValue::Duration(diff)),
        ))
    } else {
        None
    }
}

/// Read the current time in milliseconds and create a target of the current time in ms + 2.
/// Wait until we reach the target and obtain the new time. The time should differ no more than
/// 100 ms from the target.
pub fn skip_next_ms(timer: Timer) -> TestReturn {
    let time0 = timer.now();
    let target = Instant::from_millis(time0.millis() + 2);
    let _ = timer.wait_until(target);
    let time1 = timer.now();
    let diff = time1 - target;
    if diff >= Duration::from_micros(100) {
        Some((
            "skip_next_ms test failed: time difference is too large",
            Some(FailValue::Duration(diff)),
        ))
    } else {
        None
    }
}

/// Create a Duration of 1 hour and 60 minutes, convert between them and check if they are equal.
pub fn duration_hour_minute(_: Timer) -> TestReturn {
    let duration_hour = Duration::from_hours(1);
    let duration_minute = Duration::from_mins(60);
    let hour_to_minute = duration_hour.mins();
    let minute_to_hour = duration_minute.hours();
    if duration_hour != duration_minute {
        Some((
            "duration_hour_minute test failed: durations are not equal",
            None,
        ))
    } else if hour_to_minute != 60 {
        Some((
            "duration_hour_minute test failed: hour to minute conversion failed",
            None,
        ))
    } else if minute_to_hour != 1 {
        Some((
            "duration_hour_minute test failed: minute to hour conversion failed",
            None,
        ))
    } else {
        None
    }
}

/// Create a Duration of 1 minute and 60 seconds, convert between them and check if they are equal.
pub fn duration_minute_second(_: Timer) -> TestReturn {
    let duration_minute = Duration::from_mins(1);
    let duration_second = Duration::from_secs(60);
    let minute_to_second = duration_minute.secs();
    let second_to_minute = duration_second.mins();
    if duration_minute != duration_second {
        Some((
            "duration_minute_second test failed: durations are not equal",
            None,
        ))
    } else if minute_to_second != 60 {
        Some((
            "duration_minute_second test failed: minute to second conversion failed",
            None,
        ))
    } else if second_to_minute != 1 {
        Some((
            "duration_minute_second test failed: second to minute conversion failed",
            None,
        ))
    } else {
        None
    }
}

/// Create a Duration of 1 second and 1000 milliseconds, convert between them and check if they are equal.
pub fn duration_second_millisecond(_: Timer) -> TestReturn {
    let duration_second = Duration::from_secs(1);
    let duration_millisecond = Duration::from_millis(1000);
    let second_to_millisecond = duration_second.millis();
    let millisecond_to_second = duration_millisecond.secs();
    if duration_second != duration_millisecond {
        Some((
            "duration_second_millisecond test failed: durations are not equal",
            None,
        ))
    } else if second_to_millisecond != 1000 {
        Some((
            "duration_second_millisecond test failed: second to millisecond conversion failed",
            None,
        ))
    } else if millisecond_to_second != 1 {
        Some((
            "duration_second_millisecond test failed: millisecond to second conversion failed",
            None,
        ))
    } else {
        None
    }
}

/// Create a Duration of 1 millisecond and 1000 microseconds, convert between them and check if they are equal.
pub fn duration_millisecond_microsecond(_: Timer) -> TestReturn {
    let duration_millisecond = Duration::from_millis(1);
    let duration_microsecond = Duration::from_micros(1000);
    let millisecond_to_microsecond = duration_millisecond.micros();
    let microsecond_to_millisecond = duration_microsecond.millis();
    if duration_millisecond != duration_microsecond {
        Some((
            "duration_millisecond_microsecond test failed: durations are not equal",
            None,
        ))
    } else if millisecond_to_microsecond != 1000 {
        Some(("duration_millisecond_microsecond test failed: millisecond to microsecond conversion failed", None))
    } else if microsecond_to_millisecond != 1 {
        Some(("duration_millisecond_microsecond test failed: microsecond to millisecond conversion failed", None))
    } else {
        None
    }
}

/// Create an Instant of 1 hour and 60 minutes, convert between them and check if they are equal.
pub fn instant_hour_minute(_clock: Timer) -> TestReturn {
    let instant_hour = Instant::from_hours(1);
    let instant_minute = Instant::from_mins(60);
    let hour_to_minute = instant_hour.mins();
    let minute_to_hour = instant_minute.hours();
    if instant_hour != instant_minute {
        Some((
            "instant_hour_minute test failed: instants are not equal",
            None,
        ))
    } else if hour_to_minute != 60 {
        Some((
            "instant_hour_minute test failed: hour to minute conversion failed",
            None,
        ))
    } else if minute_to_hour != 1 {
        Some((
            "instant_hour_minute test failed: minute to hour conversion failed",
            None,
        ))
    } else {
        None
    }
}

/// Create an Instant of 1 minute and 60 seconds, convert between them and check if they are equal.
pub fn instant_minute_second(_clock: Timer) -> TestReturn {
    let instant_minute = Instant::from_mins(1);
    let instant_second = Instant::from_secs(60);
    let minute_to_second = instant_minute.secs();
    let second_to_minute = instant_second.mins();
    if instant_minute != instant_second {
        Some((
            "instant_minute_second test failed: instants are not equal",
            None,
        ))
    } else if minute_to_second != 60 {
        Some((
            "instant_minute_second test failed: minute to second conversion failed",
            None,
        ))
    } else if second_to_minute != 1 {
        Some((
            "instant_minute_second test failed: second to minute conversion failed",
            None,
        ))
    } else {
        None
    }
}

/// Create an Instant of 1 second and 1000 milliseconds, convert between them and check if they are equal.
pub fn instant_second_millisecond(_clock: Timer) -> TestReturn {
    let instant_second = Instant::from_secs(1);
    let instant_millisecond = Instant::from_millis(1000);
    let second_to_millisecond = instant_second.millis();
    let millisecond_to_second = instant_millisecond.secs();
    if instant_second != instant_millisecond {
        Some((
            "instant_second_millisecond test failed: instants are not equal",
            None,
        ))
    } else if second_to_millisecond != 1000 {
        Some((
            "instant_second_millisecond test failed: second to millisecond conversion failed",
            None,
        ))
    } else if millisecond_to_second != 1 {
        Some((
            "instant_second_millisecond test failed: millisecond to second conversion failed",
            None,
        ))
    } else {
        None
    }
}

/// Create an Instant of 1 millisecond and 1000 microseconds, convert between them and check if they are equal.
pub fn instant_millisecond_microsecond(_clock: Timer) -> TestReturn {
    let instant_millisecond = Instant::from_millis(1);
    let instant_microsecond = Instant::from_micros(1000);
    let millisecond_to_microsecond = instant_millisecond.micros();
    let microsecond_to_millisecond = instant_microsecond.millis();
    if instant_millisecond != instant_microsecond {
        Some((
            "instant_millisecond_microsecond test failed: instants are not equal",
            None,
        ))
    } else if millisecond_to_microsecond != 1000 {
        Some(("instant_millisecond_microsecond test failed: millisecond to microsecond conversion failed", None))
    } else if microsecond_to_millisecond != 1 {
        Some(("instant_millisecond_microsecond test failed: microsecond to millisecond conversion failed", None))
    } else {
        None
    }
}
