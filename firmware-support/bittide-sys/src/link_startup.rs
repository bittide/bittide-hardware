// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use bittide_hal::manual_additions::transceivers::TransceiversInterface;
use bittide_hal::shared_devices::ElasticBuffer;
use log::debug;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum UgnCaptureState {
    WaitForChannelNegotiation,
    WaitForNeighborTransmitReady,
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
        transceivers: &impl TransceiversInterface,
        channel: usize,
        elastic_buffer: &ElasticBuffer,
        captured_ugn: bool,
    ) {
        let old_state = self.state;
        self.state = match self.state {
            UgnCaptureState::WaitForChannelNegotiation => {
                if transceivers
                    .handshakes_done(channel)
                    .expect("Channel out of range")
                {
                    transceivers.set_transmit_starts(channel, true);
                    UgnCaptureState::WaitForNeighborTransmitReady
                } else {
                    self.state
                }
            }
            UgnCaptureState::WaitForNeighborTransmitReady => {
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
        if self.state != old_state {
            debug!(
                "link_startup[{}]: {:?} -> {:?}",
                channel, old_state, self.state
            );
        }
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
