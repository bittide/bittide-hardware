// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

//! Benchmark of the SPI transactions clock control uses to adjust the frequency of
//! the clock chip. It determines the maximum rate at which clock control can make
//! frequency adjustments.
//!
//! FINC and FDEC write to the same register, so the SPI driver does not have to set
//! the page and address in between. Each adjustment therefore costs a single transfer,
//! regardless of whether FINC and FDEC alternate.
//!
//! Durations are measured from the start of an adjustment until the SPI is no longer
//! busy. This includes accessing the SPI over the clock domain crossing and reading the
//! timer, which is reported separately as "timer overhead".

use bittide_hal::manual_additions::timer::Duration;
use bittide_hal::shared_devices::{Si539xSpi, Timer, Uart};
use bittide_hal::types::SpeedChange;
use ufmt::uwriteln;

/// Number of operations to time per benchmark. Must be even, so the alternating FINC
/// and FDEC benchmark leaves the frequency unchanged.
const N_OPERATIONS: u32 = 1000;

struct Stats {
    min: Duration,
    max: Duration,
    total: Duration,
}

impl Stats {
    /// Time 'N_OPERATIONS' calls of 'op'. The operation receives its index, so it can
    /// alternate between operations.
    fn measure(timer: &Timer, mut op: impl FnMut(u32)) -> Stats {
        let mut stats = Stats {
            min: Duration::from_micros(u64::MAX),
            max: Duration::from_micros(0),
            total: Duration::from_micros(0),
        };
        for i in 0..N_OPERATIONS {
            let start = timer.now();
            op(i);
            let duration = timer.now() - start;
            stats.min = stats.min.min(duration);
            stats.max = stats.max.max(duration);
            stats.total += duration;
        }
        stats
    }

    fn report(&self, uart: &mut Uart, name: &str) {
        let mean = self.total.micros() / N_OPERATIONS as u64;
        uwriteln!(
            uart,
            "SPI benchmark: {}: min {} us, mean {} us, max {} us, total {} us, n {}",
            name,
            self.min.micros(),
            mean,
            self.max.micros(),
            self.total.micros(),
            N_OPERATIONS,
        )
        .unwrap();
    }
}

/// Run the benchmark and report the results over UART.
///
/// This performs real frequency adjustments, but alternates between FINC and FDEC. The
/// frequency therefore deviates at most a single step, and ends where it started.
pub fn run(si539x_spi: &Si539xSpi, timer: &Timer, uart: &mut Uart) {
    uwriteln!(uart, "Running SPI benchmark..").unwrap();

    Stats::measure(timer, |_| ()).report(uart, "timer overhead");

    Stats::measure(timer, |i| {
        let speed_change = if i % 2 == 0 {
            SpeedChange::SpeedUp
        } else {
            SpeedChange::SlowDown
        };
        si539x_spi.try_speed_change(speed_change);
        while si539x_spi.is_busy() {
            continue;
        }
    })
    .report(uart, "alternating FINC/FDEC");
}
