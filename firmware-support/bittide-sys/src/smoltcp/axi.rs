// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
use bittide_hal::manual_additions::axi::{AxiRx, AxiTx};
use log::{debug, trace};
use smoltcp::phy::{self, Device, DeviceCapabilities, Medium};
use smoltcp::time::Instant;

/// Abstraction over `Axirx` and `AxiTx` to provide a `Device` implementation for smoltcp.
///
pub struct AxiEthernet<Rx: AxiRx, Tx: AxiTx, const MTU: usize> {
    axi_rx: Rx,
    rx_token_exists: bool,
    axi_tx: Tx,
    max_burst: Option<usize>,
    medium: Medium,
}

#[allow(clippy::new_without_default)]
impl<Rx: AxiRx, Tx: AxiTx, const MTU: usize> AxiEthernet<Rx, Tx, MTU> {
    pub fn new(
        medium: Medium,
        axi_rx: Rx,
        axi_tx: Tx,
        max_burst: Option<usize>,
    ) -> AxiEthernet<Rx, Tx, MTU> {
        AxiEthernet {
            axi_rx,
            rx_token_exists: false,
            axi_tx,
            max_burst,
            medium,
        }
    }
}

impl<Rx: AxiRx, Tx: AxiTx, const MTU: usize> Device for AxiEthernet<Rx, Tx, MTU> {
    type RxToken<'a>
        = RxToken<'a, Rx>
    where
        Rx: 'a,
        Tx: 'a;
    type TxToken<'a>
        = TxToken<'a, Tx, MTU>
    where
        Rx: 'a,
        Tx: 'a;
    fn capabilities(&self) -> DeviceCapabilities {
        let mut cap = DeviceCapabilities::default();
        cap.max_transmission_unit = MTU;
        cap.medium = self.medium;
        cap.max_burst_size = self.max_burst;
        cap
    }

    fn receive(&mut self, _timestamp: Instant) -> Option<(Self::RxToken<'_>, Self::TxToken<'_>)> {
        // If there is data available,
        let status = self.axi_rx.read_status();
        if self.rx_token_exists {
            debug!("Rx token already exists");
            return None;
        }
        if status.packet_complete {
            trace!("Data available");
            self.rx_token_exists = true;

            // Produce a receive toking with the data and tx token that can
            // be used to respond to the received data.
            let rx = RxToken {
                axi_rx: &mut self.axi_rx,
                rx_token_exists: &mut self.rx_token_exists,
            };
            let tx = TxToken {
                axi_tx: &mut self.axi_tx,
            };
            Some((rx, tx))
        } else {
            if status.buffer_full {
                debug!("Clearing full buffer");
                self.axi_rx.clear_packet();
                self.axi_rx.clear_status();
            }
            None
        }
    }

    fn transmit(&mut self, _timestamp: Instant) -> Option<Self::TxToken<'_>> {
        Some(TxToken {
            axi_tx: &mut self.axi_tx,
        })
    }
}

pub struct RxToken<'a, Rx: AxiRx> {
    axi_rx: &'a mut Rx,
    rx_token_exists: &'a mut bool,
}

impl<Rx: AxiRx> phy::RxToken for RxToken<'_, Rx> {
    fn consume<R, F>(self, f: F) -> R
    where
        F: FnOnce(&[u8]) -> R,
    {
        // Get a slice containing the received data
        let buf = self.axi_rx.get_slice();

        // Process the received data
        let r = f(buf);

        // Clear the packet and status registers
        self.axi_rx.clear_packet();
        self.axi_rx.clear_status();
        *self.rx_token_exists = false;
        trace!("Consumed {} bytes", buf.len());

        r
    }
}

pub struct TxToken<'a, Tx: AxiTx, const MTU: usize> {
    axi_tx: &'a mut Tx,
}

impl<'a, Tx: AxiTx, const MTU: usize> TxToken<'a, Tx, MTU> {
    pub fn new(axi_tx: &'a mut Tx) -> TxToken<'a, Tx, MTU> {
        TxToken { axi_tx }
    }
}

impl<'a, Tx: AxiTx, const MTU: usize> phy::TxToken for TxToken<'a, Tx, MTU> {
    fn consume<R, F>(self, len: usize, f: F) -> R
    where
        F: FnOnce(&mut [u8]) -> R,
    {
        // The HAL of our peripheral manually sends the packet word by word and byte by byte.
        // For this reason we need to ensure that all bytes
        let mut buffer = [0; MTU];
        let packet = &mut buffer[0..len];
        let result: R = f(packet);
        self.axi_tx.send(packet);
        result
    }
}
