// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
use crate::axi::{AxiRx, AxiTx};
use log::info;
use smoltcp::phy::{self, Device, DeviceCapabilities, Medium};
use smoltcp::time::Instant;
pub struct AxiEthernet<const MTU: usize> {
    axi_rx: AxiRx,
    axi_tx: AxiTx,
    medium: Medium,
}

#[allow(clippy::new_without_default)]
impl<const MTU: usize> AxiEthernet<{ MTU }> {
    pub fn new(medium: Medium, axi_rx: AxiRx, axi_tx: AxiTx) -> AxiEthernet<{ MTU }> {
        AxiEthernet {
            axi_rx,
            axi_tx,
            medium,
        }
    }
}

impl<const MTU: usize> Device for AxiEthernet<{ MTU }> {
    type RxToken<'a> = RxToken<'a>;
    type TxToken<'a> = TxToken<'a, MTU>;
    fn capabilities(&self) -> DeviceCapabilities {
        let mut cap = DeviceCapabilities::default();
        cap.max_transmission_unit = MTU;
        cap.medium = self.medium;
        cap
    }

    fn receive(&mut self, _timestamp: Instant) -> Option<(Self::RxToken<'_>, Self::TxToken<'_>)> {
        if self.axi_rx.has_data() {
            info!("Data available");
            let rx = RxToken {
                axi_rx: &mut self.axi_rx,
            };
            let tx = TxToken {
                axi_tx: &mut self.axi_tx,
            };
            Some((rx, tx))
        } else {
            None
        }
    }

    fn transmit(&mut self, _timestamp: Instant) -> Option<Self::TxToken<'_>> {
        Some(TxToken {
            axi_tx: &mut self.axi_tx,
        })
    }
}

#[doc(hidden)]
#[derive()]
pub struct RxToken<'a> {
    axi_rx: &'a mut AxiRx,
}

impl phy::RxToken for RxToken<'_> {
    fn consume<R, F>(self, f: F) -> R
    where
        F: FnOnce(&mut [u8]) -> R,
    {
        let buf = self.axi_rx.get_slice();

        // TODO: This is a hack to get around the fact that the buffer is not mutable
        // in the smoltcp API. We should probably fix this in the smoltcp API.
        #[allow(clippy::cast_ref_to_mut)]
        let mutable_buf = unsafe { &mut *(buf as *const [u8] as *mut [u8]) };
        let r = f(mutable_buf);
        self.axi_rx.clear_packet();
        self.axi_rx.clear_status();
        info!("Consumed {} bytes", buf.len());
        r
    }
}

#[doc(hidden)]
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
        let mut buffer = [0; BUFFER_SIZE];
        let packet = &mut buffer[0..len];
        let result: R = f(packet);
        self.axi_tx.send(packet);
        result
    }
}
