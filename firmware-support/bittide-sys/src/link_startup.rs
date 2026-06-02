// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use bittide_hal::manual_additions::handshakes::HandshakesInterface;
use bittide_hal::manual_additions::transceivers::TransceiversInterface;
use bittide_hal::shared_devices::ElasticBuffer;
use bittide_hal::types::mode::Mode;
use log::debug;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum LinkStartupState {
    WaitForRxInitDone,
    EnableHandshake,
    WaitForTxInitDone,
    SignalSoftwareReady,
    WaitForNeighborSoftwareReady,
    CenterElasticBuffer,
    SignalReceiveReady,
    WaitForReceiveDone,
    EnableAutoCenter,
    WaitForHandshakeDone,
    Done,
}

#[derive(Debug, Copy, Clone)]
pub struct LinkStartup {
    pub state: LinkStartupState,
}

impl LinkStartup {
    pub fn new() -> Self {
        Self {
            state: LinkStartupState::WaitForRxInitDone,
        }
    }

    pub fn next(
        &mut self,
        transceivers: &impl TransceiversInterface,
        handshakes: &impl HandshakesInterface,
        channel: usize,
        elastic_buffer: &ElasticBuffer,
    ) {
        let old_state = self.state;
        self.state = match self.state {
            LinkStartupState::WaitForRxInitDone => {
                if transceivers
                    .rx_data_init_dones(channel)
                    .expect("Channel out of range")
                {
                    LinkStartupState::EnableHandshake
                } else {
                    self.state
                }
            }
            LinkStartupState::EnableHandshake => {
                handshakes
                    .set_modes(channel, Mode::Enabled)
                    .expect("Channel out of range");
                LinkStartupState::WaitForTxInitDone
            }
            LinkStartupState::WaitForTxInitDone => {
                if transceivers
                    .tx_data_init_dones(channel)
                    .expect("Channel out of range")
                {
                    LinkStartupState::SignalSoftwareReady
                } else {
                    self.state
                }
            }
            LinkStartupState::SignalSoftwareReady => {
                handshakes
                    .set_software_ready(channel, true)
                    .expect("Channel out of range");
                LinkStartupState::WaitForNeighborSoftwareReady
            }
            LinkStartupState::WaitForNeighborSoftwareReady => {
                if handshakes
                    .neighbor_software_ready(channel)
                    .expect("Channel out of range")
                {
                    LinkStartupState::CenterElasticBuffer
                } else {
                    self.state
                }
            }
            LinkStartupState::CenterElasticBuffer => {
                elastic_buffer.set_occupancy(0);
                elastic_buffer.set_clear_status_registers(true);
                LinkStartupState::SignalReceiveReady
            }
            LinkStartupState::SignalReceiveReady => {
                handshakes
                    .set_receive_ready(channel, true)
                    .expect("Channel out of range");
                LinkStartupState::WaitForReceiveDone
            }
            LinkStartupState::WaitForReceiveDone => {
                if handshakes
                    .receive_done(channel)
                    .expect("Channel out of range")
                {
                    LinkStartupState::EnableAutoCenter
                } else {
                    self.state
                }
            }
            LinkStartupState::EnableAutoCenter => {
                elastic_buffer.set_auto_center_enable(true);
                LinkStartupState::WaitForHandshakeDone
            }
            LinkStartupState::WaitForHandshakeDone => {
                if handshakes
                    .handshake_done(channel)
                    .expect("Channel out of range")
                {
                    LinkStartupState::Done
                } else {
                    self.state
                }
            }
            LinkStartupState::Done => self.state,
        };
        if self.state != old_state {
            debug!(
                "link_startup[{}]: {:?} -> {:?}",
                channel, old_state, self.state
            );
        }
    }

    pub fn is_done(&self) -> bool {
        self.state == LinkStartupState::Done
    }
}

impl Default for LinkStartup {
    fn default() -> Self {
        Self::new()
    }
}
