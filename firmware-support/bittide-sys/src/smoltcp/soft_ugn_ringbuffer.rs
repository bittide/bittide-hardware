// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

//! smoltcp Device implementation for aligned soft-UGN ringbuffers.

use bittide_hal::hals::soft_ugn_demo_mu::devices::{ReceiveRingbuffer, TransmitRingbuffer};
use crc::{Crc, CRC_32_ISCSI};
use log::trace;
use smoltcp::phy::{self, Device, DeviceCapabilities, Medium};
use smoltcp::time::Instant;

const ALIGNMENT_EMPTY: u64 = 0;
const ALIGNMENT_ANNOUNCE: u64 = 0xBADC0FFEE;
const ALIGNMENT_ACKNOWLEDGE: u64 = 0xDEADABBA;
const PACKET_HEADER_SIZE: usize = 8;
const MIN_IP_PACKET_SIZE: usize = 20;
const CRC: Crc<u32> = Crc::<u32>::new(&CRC_32_ISCSI);

pub struct AlignedReceiveBuffer {
    rx: ReceiveRingbuffer,
    rx_alignment_offset: Option<usize>,
    tx_reference: usize,
}

impl AlignedReceiveBuffer {
    pub fn new(rx: ReceiveRingbuffer) -> Self {
        Self {
            rx,
            rx_alignment_offset: None,
            tx_reference: 0,
        }
    }

    pub fn align(&mut self, tx: &TransmitRingbuffer) {
        trace!("ringbuffer align start");
        let announce_pattern = [ALIGNMENT_ANNOUNCE.to_le_bytes()];
        tx.write_slice(&announce_pattern, 0);

        let empty_pattern: [[u8; 8]; 1] = [ALIGNMENT_EMPTY.to_le_bytes()];
        for i in 1..TransmitRingbuffer::DATA_LEN {
            tx.write_slice(&empty_pattern, i);
        }

        let rx_offset = 'outer: loop {
            for rx_idx in 0..ReceiveRingbuffer::DATA_LEN {
                let mut data_buf = [[0u8; 8]; 1];
                self.rx.read_slice(&mut data_buf, rx_idx);
                let value = u64::from_le_bytes(data_buf[0]);

                if value == ALIGNMENT_ANNOUNCE || value == ALIGNMENT_ACKNOWLEDGE {
                    trace!("ringbuffer align saw marker at rx_idx {}", rx_idx);
                    break 'outer rx_idx;
                }
            }
        };

        let ack_pattern = [ALIGNMENT_ACKNOWLEDGE.to_le_bytes()];
        tx.write_slice(&ack_pattern, 0);

        loop {
            let mut data_buf = [[0u8; 8]; 1];
            self.rx.read_slice(&mut data_buf, rx_offset);
            let value = u64::from_le_bytes(data_buf[0]);
            if value == ALIGNMENT_ACKNOWLEDGE {
                break;
            }
        }

        self.rx_alignment_offset = Some(rx_offset);
        self.tx_reference = tx.0 as *const _ as usize;
        trace!(
            "ringbuffer align done offset {} tx_ref 0x{:X}",
            rx_offset,
            self.tx_reference
        );
    }

    pub fn is_aligned(&self) -> bool {
        self.rx_alignment_offset.is_some()
    }

    pub fn verify_aligned_to(&self, tx: &TransmitRingbuffer) -> bool {
        self.is_aligned() && self.tx_reference == (tx.0 as *const _ as usize)
    }

    pub fn get_alignment_reference(&self) -> usize {
        self.tx_reference
    }

    pub fn read_slice(&self, dst: &mut [[u8; 8]], offset: usize) {
        assert!(dst.len() + offset <= ReceiveRingbuffer::DATA_LEN);
        let rx_offset = self
            .rx_alignment_offset
            .expect("Alignment offset not discovered yet. Call align() first.");
        let mut aligned_offset = offset + rx_offset;
        if aligned_offset >= ReceiveRingbuffer::DATA_LEN {
            aligned_offset -= ReceiveRingbuffer::DATA_LEN;
        }
        read_slice_with_wrap(&self.rx, dst, aligned_offset)
    }
}

fn read_slice_with_wrap(rx: &ReceiveRingbuffer, dst: &mut [[u8; 8]], offset: usize) {
    assert!(dst.len() <= ReceiveRingbuffer::DATA_LEN);
    if dst.len() + offset <= ReceiveRingbuffer::DATA_LEN {
        rx.read_slice(dst, offset);
    } else {
        let first_part_len = ReceiveRingbuffer::DATA_LEN - offset;
        let (first, second) = dst.split_at_mut(first_part_len);
        rx.read_slice(first, offset);
        rx.read_slice(second, 0);
    }
}

fn is_valid(buffer: &[u8]) -> bool {
    if buffer.len() < PACKET_HEADER_SIZE {
        return false;
    }
    let stored_crc = u32::from_le_bytes([buffer[0], buffer[1], buffer[2], buffer[3]]);
    let calculated_crc = CRC.checksum(&buffer[4..]);
    stored_crc == calculated_crc
}

pub struct RingbufferDevice {
    rx_buffer: AlignedReceiveBuffer,
    tx_buffer: TransmitRingbuffer,
    mtu: usize,
    last_rx_seq: u16,
    tx_seq_num: u16,
}

impl RingbufferDevice {
    pub fn new(rx_buffer: AlignedReceiveBuffer, tx_buffer: TransmitRingbuffer) -> Self {
        let rx_bytes = ReceiveRingbuffer::DATA_LEN * 8;
        let tx_bytes = TransmitRingbuffer::DATA_LEN * 8;
        let mtu = rx_bytes.min(tx_bytes) - PACKET_HEADER_SIZE;

        if rx_buffer.is_aligned() {
            assert!(
                rx_buffer.verify_aligned_to(&tx_buffer),
                "RX buffer is aligned but not to the provided TX buffer, expected reference {:p}, got 0x{:X}",
                &tx_buffer.0,
                rx_buffer.get_alignment_reference(),
            );
        }

        Self {
            rx_buffer,
            tx_buffer,
            mtu,
            last_rx_seq: u16::MAX,
            tx_seq_num: 0,
        }
    }

    pub fn mtu(&self) -> usize {
        self.mtu
    }
}

impl Device for RingbufferDevice {
    type RxToken<'a> = RxToken;
    type TxToken<'a> = TxToken<'a>;

    fn capabilities(&self) -> DeviceCapabilities {
        let mut cap = DeviceCapabilities::default();
        cap.max_transmission_unit = self.mtu;
        cap.medium = Medium::Ip;
        cap
    }

    fn receive(&mut self, _timestamp: Instant) -> Option<(Self::RxToken<'_>, Self::TxToken<'_>)> {
        let mut header_buf: [[u8; 8]; 1] = [[0u8; 8]; 1];
        self.rx_buffer.read_slice(&mut header_buf, 0);

        let header_ptr = header_buf[0].as_ptr();
        let seq_num = unsafe { (header_ptr.add(4) as *const u16).read_unaligned() };
        let packet_len = unsafe { (header_ptr.add(6) as *const u16).read_unaligned() } as usize;

        trace!(
            "ringbuffer rx header seq {} len {} last {}",
            seq_num,
            packet_len,
            self.last_rx_seq
        );

        if seq_num == self.last_rx_seq {
            trace!("ringbuffer rx repeated seq {}", seq_num);
            return None;
        }

        if packet_len < MIN_IP_PACKET_SIZE || packet_len > self.mtu {
            trace!(
                "ringbuffer rx invalid len {} (must be {}-{}) seq {}",
                packet_len,
                MIN_IP_PACKET_SIZE,
                self.mtu,
                seq_num
            );
            return None;
        }

        let total_len = PACKET_HEADER_SIZE + packet_len;
        let num_words = total_len.div_ceil(8);
        let mut packet_buffer = [0u8; ReceiveRingbuffer::DATA_LEN * 8];

        let word_slice = unsafe {
            core::slice::from_raw_parts_mut(packet_buffer.as_mut_ptr() as *mut [u8; 8], num_words)
        };

        self.rx_buffer.read_slice(word_slice, 0);

        if !is_valid(&packet_buffer[..total_len]) {
            trace!("ringbuffer rx crc fail seq {}", seq_num);
            return None;
        }

        trace!(
            "ringbuffer rx valid seq {}, payload {} bytes",
            seq_num,
            packet_len
        );
        self.last_rx_seq = seq_num;

        let mut payload = [0u8; ReceiveRingbuffer::DATA_LEN * 8];
        payload[..packet_len]
            .copy_from_slice(&packet_buffer[PACKET_HEADER_SIZE..PACKET_HEADER_SIZE + packet_len]);

        let rx = RxToken {
            buffer: payload,
            length: packet_len,
        };
        let tx = TxToken {
            tx_buffer: &mut self.tx_buffer,
            mtu: self.mtu,
            seq_num: &mut self.tx_seq_num,
        };
        Some((rx, tx))
    }

    fn transmit(&mut self, _timestamp: Instant) -> Option<Self::TxToken<'_>> {
        trace!("ringbuffer tx token seq {}", self.tx_seq_num);
        Some(TxToken {
            tx_buffer: &mut self.tx_buffer,
            mtu: self.mtu,
            seq_num: &mut self.tx_seq_num,
        })
    }
}

pub struct RxToken {
    buffer: [u8; ReceiveRingbuffer::DATA_LEN * 8],
    length: usize,
}

impl phy::RxToken for RxToken {
    fn consume<R, F>(self, f: F) -> R
    where
        F: FnOnce(&[u8]) -> R,
    {
        trace!("Consuming validated packet ({} bytes)", self.length);
        f(&self.buffer[..self.length])
    }
}

pub struct TxToken<'a> {
    tx_buffer: &'a mut TransmitRingbuffer,
    mtu: usize,
    seq_num: &'a mut u16,
}

impl phy::TxToken for TxToken<'_> {
    fn consume<R, F>(self, len: usize, f: F) -> R
    where
        F: FnOnce(&mut [u8]) -> R,
    {
        assert!(
            len <= self.mtu,
            "Packet length {} exceeds MTU {}",
            len,
            self.mtu
        );

        let mut buffer = [0u8; TransmitRingbuffer::DATA_LEN * 8];
        let header_ptr = buffer.as_mut_ptr();
        unsafe {
            (header_ptr.add(4) as *mut u16).write_unaligned(self.seq_num.to_le());
            (header_ptr.add(6) as *mut u16).write_unaligned((len as u16).to_le());
        }

        let result = f(&mut buffer[PACKET_HEADER_SIZE..PACKET_HEADER_SIZE + len]);

        let total_len = PACKET_HEADER_SIZE + len;
        let crc = CRC.checksum(&buffer[4..total_len]);
        unsafe {
            (header_ptr as *mut u32).write_unaligned(crc.to_le());
        }

        let num_words = total_len.div_ceil(8);
        let word_slice =
            unsafe { core::slice::from_raw_parts(buffer.as_ptr() as *const [u8; 8], num_words) };

        self.tx_buffer.write_slice(word_slice, 0);

        trace!(
            "Transmitted packet: checksum {}, seq {}, total length {}",
            crc,
            self.seq_num,
            total_len,
        );
        *self.seq_num = self.seq_num.wrapping_add(1);

        result
    }
}
