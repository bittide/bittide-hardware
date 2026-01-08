#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use bittide_hal::hals::switch_demo_mu::DeviceInstances;
use bittide_hal::shared_devices::{ElasticBuffer, Transceivers, Uart};
use bittide_sys::stability_detector::Stability;
use core::panic::PanicInfo;
use ufmt::uwriteln;

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum UgnCaptureState {
    WaitForChannelNegotiation,
    SwitchUserMode,
    WaitForUgnCapture,
    WaitForStability,
    Done,
}

impl UgnCaptureState {
    pub fn as_index(&self) -> usize {
        *self as usize
    }
}

#[derive(Copy, Clone)]
pub struct LinkStartup {
    pub state: UgnCaptureState,
    pub eb_delta: i8,

    pub statistics: [u8; 4],
}

impl LinkStartup {
    /// Create a new LinkStartup.
    pub fn new() -> Self {
        Self {
            state: UgnCaptureState::WaitForChannelNegotiation,
            eb_delta: 0,
            statistics: [0; 4],
        }
    }

    /// Center the elastic buffer and accumulate the number of frames that were
    /// added or removed.
    pub fn center_eb(&mut self, eb: &ElasticBuffer) {
        self.eb_delta += eb.set_occupancy(0);
    }

    /// Transition to the next state based on current conditions.
    pub fn next(
        &mut self,
        uart: &mut Uart,
        transceivers: &Transceivers,
        channel: usize,
        elastic_buffer: &ElasticBuffer,
        captured_ugn: bool,
        all_stable: bool,
    ) {
        self.statistics[self.state.as_index()] += 1;
        self.state = match self.state {
            UgnCaptureState::WaitForChannelNegotiation => {
                if transceivers.handshakes_done(channel).unwrap_or(false) {
                    uwriteln!(uart, "Channel {} negotiation done.", channel).unwrap();
                    uwriteln!(uart, "{:?}", transceivers.statistics(channel)).unwrap();
                    UgnCaptureState::SwitchUserMode
                } else {
                    self.state
                }
            }
            UgnCaptureState::SwitchUserMode => {
                // Center the elastic buffer once before switching to user mode.
                elastic_buffer.set_occupancy(0);
                uwriteln!(
                    uart,
                    "Switch transceiver channel {} to user mode..",
                    channel
                )
                .unwrap();
                transceivers.set_receive_readys(channel, true);
                transceivers.set_transmit_starts(channel, true);
                UgnCaptureState::WaitForUgnCapture
            }
            UgnCaptureState::WaitForUgnCapture => {
                if captured_ugn {
                    uwriteln!(uart, "Captured UGN for channel {}", channel).unwrap();
                    UgnCaptureState::WaitForStability
                } else {
                    self.state
                }
            }
            UgnCaptureState::WaitForStability => {
                // Continously center the elastic buffer to avoid over/under-flows.
                self.center_eb(elastic_buffer);
                if all_stable {
                    elastic_buffer.set_stable(true);
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

    pub fn print_statistics(&self, uart: &mut Uart, channel: usize) {
        uwriteln!(
            uart,
            "Channel {} startup statistics: {:?}",
            channel,
            self.statistics,
        )
        .unwrap();
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
    let timer = INSTANCES.timer;
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

    let start = timer.now();

    let mut link_startups = [LinkStartup::new(); 7];
    while !link_startups.iter().all(|ls| ls.is_done()) {
        // We don't update the stability here, but leave that to callisto. Although
        // we also have access to the 'links_settled' register, we don't want to
        // flood the CC bus.
        let stability = Stability {
            stable: cc.links_stable()[0],
            settled: 0,
        };
        let all_stable = stability.all_stable();

        for (i, link_startup) in link_startups.iter_mut().enumerate() {
            link_startup.next(
                &mut uart,
                transceivers,
                i,
                elastic_buffers[i],
                capture_ugns[i].has_captured(),
                all_stable,
            );
        }
    }
    let duration = timer.now() - start;
    uwriteln!(uart, "Link startups completed in {}", duration).unwrap();
    for (i, link_startup) in link_startups.iter().enumerate() {
        link_startup.print_statistics(&mut uart, i);
    }

    let eb_deltas = link_startups.iter().map(|ls| ls.eb_delta);

    for (i, (capture_ugn, eb_delta)) in capture_ugns.iter().zip(eb_deltas).enumerate() {
        capture_ugn.set_elastic_buffer_delta(eb_delta);
        uwriteln!(
            uart,
            "Capture UGN {}: local = {}, remote = {}, EB delta = {}",
            i,
            capture_ugn.local_counter(),
            u64::from_ne_bytes(capture_ugn.remote_counter()),
            eb_delta
        )
        .unwrap();
    }
    uwriteln!(uart, "All UGNs captured").unwrap();

    #[allow(clippy::empty_loop)]
    loop {}
}

#[panic_handler]
fn panic_handler(_: &PanicInfo) -> ! {
    #[allow(clippy::empty_loop)]
    loop {}
}
