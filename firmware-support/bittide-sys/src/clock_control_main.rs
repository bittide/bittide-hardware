// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use bittide_hal::manual_additions::clock_control::ClockControlInterface;
use bittide_hal::manual_additions::domain_diff_counters::DomainDiffCountersInterface;
use bittide_hal::manual_additions::freeze::FreezeInterface;
use bittide_hal::manual_additions::timer::{Duration, Instant, WaitResult};
use bittide_hal::shared_devices::sample_memory::SampleMemory;
use bittide_hal::shared_devices::sync_out_generator::SyncOutGenerator;
use bittide_hal::shared_devices::timer::Timer;
use bittide_hal::shared_devices::uart::Uart;
use itertools::izip;
use ufmt::uwriteln;

use crate::callisto::Callisto;
use crate::sample_store::SampleStore;
use crate::stability_detector::StabilityDetector;

fn test_bit(bv: u8, i: usize) -> bool {
    (bv & (1 << i)) != 0
}

fn panic_on_missed_deadline(
    uart: &mut Uart,
    timer: &Timer,
    next_update: Instant,
    timer_result: WaitResult,
) {
    if let WaitResult::AlreadyPassed = timer_result {
        uwriteln!(
            uart,
            "Deadline missed! Timer did not update in time. Now: {}. Next update: {}",
            timer.now().micros(),
            next_update.micros(),
        )
        .unwrap();
        panic!("Deadline missed! See UART log.");
    };
}

pub fn clock_control_main<
    const EB_COUNT: usize,
    const N_LINKS: usize,
    CC: ClockControlInterface,
    F: FreezeInterface,
    DDC: DomainDiffCountersInterface,
>(
    cc: CC,
    timer: Timer,
    mut uart: Uart,
    freeze: F,
    sample_memory: SampleMemory,
    sync_out_generator: SyncOutGenerator,
    domain_diff_counters: DDC,
) -> ! {
    uwriteln!(uart, "Starting sync out generator..").unwrap();
    sync_out_generator.set_active(true);

    uwriteln!(uart, "Starting clock control..").unwrap();
    let mut callisto = Callisto::new(cc.config().callisto);

    let mut stability_detector = StabilityDetector::<N_LINKS>::new(4, Duration::from_secs(2));

    let mut sample_store = SampleStore::<EB_COUNT>::new(sample_memory, 100);

    let interval = Duration::from_micros(200);
    let mut next_update = timer.now() + interval;
    let mut prev_all_stable = false;

    loop {
        freeze.set_freeze(());

        cc.set_change_speed(callisto.update(
            &cc,
            izip!(0..DDC::ENABLE_LEN, freeze.eb_counters_volatile_iter()).map(|(i, counter)| {
                if domain_diff_counters.enable(i).unwrap_or(false) {
                    Some(counter)
                } else {
                    None
                }
            }),
        ));

        let stability = stability_detector.update(&cc, timer.now());

        let has_sense_of_global_time = freeze.number_of_sync_pulses_seen() != 0;
        if !prev_all_stable && has_sense_of_global_time {
            sample_store.store(&freeze, stability, callisto.accumulated_speed_requests);
        }

        let all_stable = stability.all_stable(cc.n_links() as usize);
        if !prev_all_stable && all_stable {
            uwriteln!(uart, "All links stable").unwrap();
        } else if prev_all_stable && !all_stable {
            uwriteln!(uart, "Links no longer stable").unwrap();
            panic!("Links no longer stable");
        }
        prev_all_stable = all_stable;

        let link_mask_rev = cc.link_mask_rev();
        for i in 0..DDC::ENABLE_LEN {
            domain_diff_counters.set_enable(i, test_bit(link_mask_rev[0], i));
        }

        let timer_result = timer.wait_until_stall(next_update);
        panic_on_missed_deadline(&mut uart, &timer, next_update, timer_result);
        next_update += interval;
    }
}
