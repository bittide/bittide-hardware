// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
use crate::axi::{AxiRx, AxiTx};
use log::debug;
use smoltcp::phy::{self, Device, DeviceCapabilities, Medium};
use smoltcp::time::Instant;
pub struct AxiEthernet<const MTU: usize> {
    axi_rx: AxiRx<MTU>,
    axi_tx: AxiTx,
    medium: Medium,
}

#[allow(clippy::new_without_default)]
impl<const MTU: usize> AxiEthernet<MTU> {
    pub fn new(medium: Medium, axi_rx: AxiRx<MTU>, axi_tx: AxiTx) -> AxiEthernet<MTU> {
        AxiEthernet {
            axi_rx,
            axi_tx,
            medium,
        }
    }
}

impl<const MTU: usize> Device for AxiEthernet<MTU> {
    type RxToken<'a> = RxToken<'a, MTU>;
    type TxToken<'a> = TxToken<'a, MTU>;
    fn capabilities(&self) -> DeviceCapabilities {
        let mut cap = DeviceCapabilities::default();
        cap.max_transmission_unit = MTU;
        cap.medium = self.medium;
        cap
    }

    fn receive(&mut self, _timestamp: Instant) -> Option<(Self::RxToken<'_>, Self::TxToken<'_>)> {
        // If there is data available,
        let status = self.axi_rx.read_status();
        if status.packet_complete {
            debug!("Data available");

            // Produce a receive toking with the data and tx token that can
            // be used to respond to the received data.
            let rx = RxToken {
                axi_rx: &mut self.axi_rx,
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

pub struct RxToken<'a, const BUFFER_SIZE: usize> {
    axi_rx: &'a mut AxiRx<{ BUFFER_SIZE }>,
}

impl<const BUFFER_SIZE: usize> phy::RxToken for RxToken<'_, BUFFER_SIZE> {
    fn consume<R, F>(self, f: F) -> R
    where
        F: FnOnce(&mut [u8]) -> R,
    {
        // Get a slice containing the received data
        let buf = self.axi_rx.get_slice();

        // TODO: This is a hack to get around the fact that the buffer is not mutable,
        // but the smoltcp API requires it to be. This Should be fixed by
        // https://github.com/smoltcp-rs/smoltcp/pull/924
        #[allow(clippy::cast_ref_to_mut)]
        let mutable_buf =
            unsafe { core::slice::from_raw_parts_mut(buf.as_ptr().cast_mut(), buf.len()) };

        // Process the received data
        let r = f(mutable_buf);

        // Clear the packet and status registers
        self.axi_rx.clear_packet();
        self.axi_rx.clear_status();
        debug!("Consumed {} bytes", buf.len());

        r
    }
}

pub struct TxToken<'a, const BUFFER_SIZE: usize> {
    axi_tx: &'a mut AxiTx,
}

impl<'a, const BUFFER_SIZE: usize> TxToken<'a, BUFFER_SIZE> {
    pub fn new(axi_tx: &'a mut AxiTx) -> TxToken<'a, BUFFER_SIZE> {
        TxToken { axi_tx }
    }
}

impl<'a, const BUFFER_SIZE: usize> phy::TxToken for TxToken<'a, BUFFER_SIZE> {
    fn consume<R, F>(self, len: usize, f: F) -> R
    where
        F: FnOnce(&mut [u8]) -> R,
    {
        // The HAL of our peripheral manually sends the packet word by word and byte by byte.
        // For this reason we need to ensure that all bytes
        let mut buffer = [0; BUFFER_SIZE];
        let packet = &mut buffer[0..len];
        let result: R = f(packet);
        self.axi_tx.send(packet);
        result
    }
}
