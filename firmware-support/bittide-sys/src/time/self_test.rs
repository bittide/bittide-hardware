// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
use crate::time::{Clock, Duration, Instant};
use core::fmt::{self, Debug};
use ufmt::{uDebug, uwrite};

type TestReturn = Option<(&'static str, Option<FailValue>)>;
type TestFn = fn(Clock) -> TestReturn;

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
pub fn self_test(clock: Clock) -> impl Iterator<Item = (&'static str, TestReturn)> {
    let tests = tests!(
        now_not_null,
        freq_not_null,
        skip_next_ms,
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
        .map(move |(f, name)| (name, f(clock.clone())))
}

/// Obtain the value of the counter, check if it's not 0.
pub fn now_not_null(mut clock: Clock) -> TestReturn {
    let frequency = clock.frequency();
    let now = clock.now();
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
pub fn freq_not_null(clock: Clock) -> TestReturn {
    let frequency: u64 = clock.frequency();
    if frequency == 0 {
        Some(("freq_not_null test failed: frequency is null", None))
    } else {
        None
    }
}

/// Read the current time in milliseconds, wait a ms and read again.
/// The new time should differ less than 100 us from the expected target.
pub fn wait_1ms(mut clock: Clock) -> TestReturn {
    let wait_time = Duration::from_millis(1);
    let time0 = clock.now();
    clock.wait(wait_time);
    let time1 = clock.now();
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

/// Read the current time in milliseconds and create a target of the current time in ms + 2.
/// Wait until we reach the target and obtain the new time. The time should differ no more than
/// 100 ms from the target.
pub fn skip_next_ms(mut clock: Clock) -> TestReturn {
    let time0 = clock.now();
    let target = Instant::from_millis(time0.to_millis() + 2);
    clock.wait_until(target);
    let time1 = clock.now();
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
pub fn duration_hour_minute(_: Clock) -> TestReturn {
    let duration_hour = Duration::from_hours(1);
    let duration_minute = Duration::from_mins(60);
    let hour_to_minute = duration_hour.to_mins();
    let minute_to_hour = duration_minute.to_hours();
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
pub fn duration_minute_second(_: Clock) -> TestReturn {
    let duration_minute = Duration::from_mins(1);
    let duration_second = Duration::from_secs(60);
    let minute_to_second = duration_minute.to_secs();
    let second_to_minute = duration_second.to_mins();
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
pub fn duration_second_millisecond(_: Clock) -> TestReturn {
    let duration_second = Duration::from_secs(1);
    let duration_millisecond = Duration::from_millis(1000);
    let second_to_millisecond = duration_second.to_millis();
    let millisecond_to_second = duration_millisecond.to_secs();
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
pub fn duration_millisecond_microsecond(_: Clock) -> TestReturn {
    let duration_millisecond = Duration::from_millis(1);
    let duration_microsecond = Duration::from_micros(1000);
    let millisecond_to_microsecond = duration_millisecond.to_micros();
    let microsecond_to_millisecond = duration_microsecond.to_millis();
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
pub fn instant_hour_minute(_clock: Clock) -> TestReturn {
    let instant_hour = Instant::from_hours(1);
    let instant_minute = Instant::from_mins(60);
    let hour_to_minute = instant_hour.to_mins();
    let minute_to_hour = instant_minute.to_hours();
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
pub fn instant_minute_second(_clock: Clock) -> TestReturn {
    let instant_minute = Instant::from_mins(1);
    let instant_second = Instant::from_secs(60);
    let minute_to_second = instant_minute.to_secs();
    let second_to_minute = instant_second.to_mins();
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
pub fn instant_second_millisecond(_clock: Clock) -> TestReturn {
    let instant_second = Instant::from_secs(1);
    let instant_millisecond = Instant::from_millis(1000);
    let second_to_millisecond = instant_second.to_millis();
    let millisecond_to_second = instant_millisecond.to_secs();
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
pub fn instant_millisecond_microsecond(_clock: Clock) -> TestReturn {
    let instant_millisecond = Instant::from_millis(1);
    let instant_microsecond = Instant::from_micros(1000);
    let millisecond_to_microsecond = instant_millisecond.to_micros();
    let microsecond_to_millisecond = instant_microsecond.to_millis();
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
