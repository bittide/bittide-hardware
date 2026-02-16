#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use bittide_hal::hals::switch_demo_mu::DeviceInstances;
use bittide_hal::shared_devices::{ElasticBuffer, Transceivers};
use bittide_sys::stability_detector::Stability;
use core::panic::PanicInfo;
use ufmt::uwriteln;

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum UgnCaptureState {
    WaitForChannelNegotiation,
    WaitForNeighborRxReady,
    SwitchUserMode,
    WaitForUgnCapture,
    Done,
}

#[derive(Debug, Copy, Clone)]
pub struct LinkStartup {
    pub state: UgnCaptureState,
}

impl LinkStartup {
    /// Create a new LinkStartup
    pub fn new() -> Self {
        Self {
            state: UgnCaptureState::WaitForChannelNegotiation,
        }
    }

    /// Transition to the next state based on current conditions
    pub fn next(
        &mut self,
        transceivers: &Transceivers,
        channel: usize,
        elastic_buffer: &ElasticBuffer,
        captured_ugn: bool,
    ) {
        self.state = match self.state {
            UgnCaptureState::WaitForChannelNegotiation => {
                if transceivers
                    .handshakes_done(channel)
                    .expect("Channel out of range")
                {
                    transceivers.set_transmit_starts(channel, true);
                    UgnCaptureState::WaitForNeighborRxReady
                } else {
                    self.state
                }
            }
            UgnCaptureState::WaitForNeighborRxReady => {
                if transceivers
                    .neighbor_transmit_readys(channel)
                    .expect("Channel out of range")
                {
                    UgnCaptureState::SwitchUserMode
                } else {
                    self.state
                }
            }
            UgnCaptureState::SwitchUserMode => {
                // Center the elastic buffer once before switching to user mode
                elastic_buffer.set_occupancy(0);
                elastic_buffer.set_clear_status_registers(true);
                transceivers.set_receive_readys(channel, true);
                UgnCaptureState::WaitForUgnCapture
            }
            UgnCaptureState::WaitForUgnCapture => {
                if captured_ugn {
                    elastic_buffer.set_auto_center_enable(true);
                    UgnCaptureState::Done
                } else {
                    self.state
                }
            }
            UgnCaptureState::Done => self.state,
        };
    }

    pub fn is_done(&self) -> bool {
        self.state == UgnCaptureState::Done
    }
}

impl Default for LinkStartup {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(not(test))]
use riscv_rt::entry;

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let mut uart = INSTANCES.uart;
    let transceivers = &INSTANCES.transceivers;
    let cc = INSTANCES.clock_control;
    let elastic_buffers = [
        &INSTANCES.elastic_buffer_0,
        &INSTANCES.elastic_buffer_1,
        &INSTANCES.elastic_buffer_2,
        &INSTANCES.elastic_buffer_3,
        &INSTANCES.elastic_buffer_4,
        &INSTANCES.elastic_buffer_5,
        &INSTANCES.elastic_buffer_6,
    ];
    let capture_ugns = [
        INSTANCES.capture_ugn_0,
        INSTANCES.capture_ugn_1,
        INSTANCES.capture_ugn_2,
        INSTANCES.capture_ugn_3,
        INSTANCES.capture_ugn_4,
        INSTANCES.capture_ugn_5,
        INSTANCES.capture_ugn_6,
    ];

    let mut link_startups = [LinkStartup::new(); 7];
    while !link_startups.iter().all(|ls| ls.is_done()) {
        for (i, link_startup) in link_startups.iter_mut().enumerate() {
            link_startup.next(
                transceivers,
                i,
                elastic_buffers[i],
                capture_ugns[i].has_captured(),
            );
        }
    }

    uwriteln!(uart, "Waiting for stability...").unwrap();
    loop {
        // We don't update the stability here, but leave that to callisto. Although
        // we also have access to the 'links_settled' register, we don't want to
        // flood the CC bus.
        let stability = Stability {
            stable: cc.links_stable()[0],
            settled: 0,
        };
        let all_stable = stability.all_stable();
        if all_stable {
            break;
        }
    }

    uwriteln!(uart, "Stopping auto-centering...").unwrap();
    elastic_buffers
        .iter()
        .for_each(|eb| eb.set_auto_center_enable(false));
    elastic_buffers
        .iter()
        .for_each(|eb| eb.wait_auto_center_idle());
    let eb_deltas = elastic_buffers
        .iter()
        .map(|eb| eb.auto_center_total_adjustments());

    for (capture_ugn, eb_delta) in capture_ugns.iter().zip(eb_deltas) {
        capture_ugn.set_elastic_buffer_delta(eb_delta);
    }

    uwriteln!(uart, "All UGNs captured").unwrap();

    loop {
        for (i, eb) in elastic_buffers.iter().enumerate() {
            if eb.underflow() {
                uwriteln!(
                    uart,
                    "[ERROR] Channel {} elastic buffer underflowed at cycle {}, min occupancy: {}",
                    i,
                    eb.underflow_timestamp(),
                    eb.min_data_count_seen(),
                )
                .unwrap();
                panic!();
            }
            if eb.overflow() {
                uwriteln!(
                    uart,
                    "[ERROR] Channel {} elastic buffer overflowed at cycle {}, max occupancy: {}",
                    i,
                    eb.overflow_timestamp(),
                    eb.max_data_count_seen(),
                )
                .unwrap();
                panic!();
            }
        }
    }
}

#[panic_handler]
fn panic_handler(_: &PanicInfo) -> ! {
    #[allow(clippy::empty_loop)]
    loop {}
}
