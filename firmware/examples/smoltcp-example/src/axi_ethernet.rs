use heapless::{Vec};

use smoltcp::phy::{self, Device, DeviceCapabilities, Medium};
use smoltcp::time::Instant;

use crate::axi_buffers::{AxiRxBuffer, AxiTxBuffer};

/// A loopback device.
#[derive(Debug)]
pub struct AxiEthernet {
    pub(crate) rx_buffer: AxiRxBuffer,
    pub(crate) tx_buffer: AxiTxBuffer,
    medium: Medium,
}

#[allow(clippy::new_without_default)]
impl AxiEthernet {

    pub fn new(medium: Medium, base_addr_rx: *mut u8, base_addr_tx: *mut u8, buffer_size: usize) -> AxiEthernet {
        AxiEthernet {
            rx_buffer: AxiRxBuffer::new(base_addr_rx, buffer_size),
            tx_buffer: AxiTxBuffer::new(base_addr_tx),
            medium,
        }
    }
}

impl Device for AxiEthernet {
    type RxToken<'a> = RxToken;
    type TxToken<'a> = TxToken<'a>;

    fn capabilities(&self) -> DeviceCapabilities {
        let mut cap = DeviceCapabilities::default();
        cap.max_transmission_unit = 65535;
        cap.medium = self.medium;
        cap
    }

    fn receive(&mut self, _timestamp: Instant) -> Option<(Self::RxToken<'_>, Self::TxToken<'_>)> {
        if !self.rx_buffer.is_packet_available(){ return None};
        let mut buffer: Vec<u8, 1024> = Vec::new();
        let len = self.rx_buffer.packet_length();
        buffer.resize(len, 0).unwrap();
        self.rx_buffer.read_packet(&mut buffer).unwrap();
        let rx = RxToken {buffer};
        let tx = TxToken {tx_buffer: &mut self.tx_buffer};
        Some((rx, tx))
        }
    fn transmit(&mut self, _timestamp: Instant) -> Option<Self::TxToken<'_>> {
        Some(TxToken {tx_buffer: &mut self.tx_buffer})
    }
}

#[doc(hidden)]
pub struct RxToken {
    buffer: Vec<u8,1024>,
}

impl phy::RxToken for RxToken {
    fn consume<R, F>(mut self, f: F) -> R
    where
        F: FnOnce(&mut [u8]) -> R,
    {
        f(&mut self.buffer)
    }
}

#[doc(hidden)]
#[derive(Debug)]
pub struct TxToken<'a> {
    tx_buffer: &'a mut AxiTxBuffer,
}

impl<'a> phy::TxToken for TxToken<'a> {
    fn consume<R, F>(mut self, len: usize, f: F) -> R
    where
        F: FnOnce(&mut [u8]) -> R,
    {
        let mut buffer: Vec<u8, 1024> = Vec::new();
        buffer.resize(len, 0).unwrap();
        let result: R = f(&mut buffer);
        self.tx_buffer.write_packet(&buffer);
        result
    }
}
