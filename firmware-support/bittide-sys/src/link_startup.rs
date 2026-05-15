// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use bittide_hal::shared_devices::{ElasticBuffer, Handshakes, Transceivers};
use bittide_hal::types::mode::Mode;

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
        transceivers: &Transceivers,
        handshakes: &Handshakes,
        channel: usize,
        elastic_buffer: &ElasticBuffer,
    ) {
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
                let mut srs = handshakes.software_readys();
                srs.set(channel, true).expect("Channel out of range");
                handshakes.set_software_readys(srs);
                LinkStartupState::WaitForNeighborSoftwareReady
            }
            LinkStartupState::WaitForNeighborSoftwareReady => {
                if handshakes
                    .neighbor_software_readys()
                    .get(channel)
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
                let mut rrs = handshakes.receive_readys();
                rrs.set(channel, true).expect("Channel out of range");
                handshakes.set_receive_readys(rrs);
                LinkStartupState::WaitForReceiveDone
            }
            LinkStartupState::WaitForReceiveDone => {
                if handshakes
                    .receive_dones()
                    .get(channel)
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
                    .handshake_dones()
                    .get(channel)
                    .expect("Channel out of range")
                {
                    LinkStartupState::Done
                } else {
                    self.state
                }
            }
            LinkStartupState::Done => self.state,
        };
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
