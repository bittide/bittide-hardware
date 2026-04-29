// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use crate::manual_additions::{
    index::{Index, IndexInterface, IndexSizeCheck},
    FromAs, IntoAs,
};
use log::{debug, trace, warn};

const ALIGNMENT_ANNOUNCE: u64 = 0xBADC0FFEE;
const ALIGNMENT_ACKNOWLEDGE: u64 = 0xDEADABBA;

pub trait TransmitRingBufferInterface {
    const DATA_LEN: usize;

    /// Get a pointer to the base address of the TransmitRingBuffer
    fn base_ptr(&self) -> *mut [u8; 8];

    /// Sets the enable register for the transmit ring_buffer. When enabled, the ring_buffer will
    /// transmit the contents of the buffer to the network using an internal incrementing
    /// counter as read address. When disabled, the ring_buffer will ignore the counter and not transmit anything,
    /// but the counter will continue to increment to maintain alignment.
    fn set_enable(&self, enabled: bool);

    /// Get the current value of the enable register for the transmit ring_buffer.
    fn get_enable(&self) -> bool;

    /// Write a slice to the transmit buffer at the given offset. The slice must not exceed the buffer length when combined with the offset.
    fn write_slice(&self, src: &[[u8; 8]], offset: usize) {
        assert!(src.len() + offset <= Self::DATA_LEN);
        trace!(
            "ring_buffer tx write_slice len {} offset {offset}",
            src.len(),
        );
        unsafe {
            self.write_slice_unchecked(src, offset);
        }
    }

    /// Write a slice to the transmit buffer at the given offset without checking bounds. The caller must ensure that `src.len() + offset` does not exceed the buffer length.
    ///
    /// # Safety
    /// This function is unsafe because it can cause out-of-bounds memory access if the caller
    /// does not ensure that `src.len() + offset` is within the bounds of the transmit buffer.
    unsafe fn write_slice_unchecked(&self, src: &[[u8; 8]], offset: usize) {
        let dst_ptr = self.base_ptr().add(offset);
        let src_ptr = src.as_ptr();
        if !(src_ptr as usize).is_multiple_of(4) || !(dst_ptr as usize).is_multiple_of(4) {
            warn!(
                "ring_buffer tx write_slice_unchecked unaligned: src {src_ptr:p} dst {dst_ptr:p}"
            );
        }

        core::ptr::copy_nonoverlapping(src_ptr, dst_ptr, src.len());
    }

    fn clear(&self) {
        debug!("ring_buffer tx clear len {}", Self::DATA_LEN);
        let zero = [[0u8; 8]; 1];
        for i in 0..Self::DATA_LEN {
            unsafe {
                self.write_slice_unchecked(&zero, i);
            }
        }
    }

    /// Read a slice from the transmit buffer at the given offset. The slice must not exceed the buffer length when combined with the offset.
    fn read_slice(&self, dst: &mut [[u8; 8]], offset: usize) {
        assert!(dst.len() + offset <= Self::DATA_LEN);
        trace!(
            "ring_buffer tx read_slice len {} offset {}",
            dst.len(),
            offset
        );
        unsafe {
            self.read_slice_unchecked(dst, offset);
        }
    }

    /// Read a slice from the transmit buffer at the given offset without checking bounds. The caller must ensure that `dst.len() + offset` does not exceed the buffer length.
    ///
    /// # Safety
    /// This function is unsafe because it can cause out-of-bounds memory access if the caller
    /// does not ensure that `dst.len() + offset` is within the bounds of the transmit buffer.
    unsafe fn read_slice_unchecked(&self, dst: &mut [[u8; 8]], offset: usize) {
        let src_ptr = (self.base_ptr() as *const [u8; 8]).add(offset);
        let dst_ptr = dst.as_mut_ptr();
        if !(src_ptr as usize).is_multiple_of(4) || !(dst_ptr as usize).is_multiple_of(4) {
            warn!("ring_buffer tx read_slice_unchecked unaligned: src {src_ptr:p} dst {dst_ptr:p}");
        }
        // Use volatile reads to read back from hardware buffer
        for i in 0..dst.len() {
            dst_ptr.add(i).write(src_ptr.add(i).read_volatile());
        }
    }
}

pub trait ReceiveRingBufferInterface {
    const DATA_LEN: usize;
    const CLEAR_AT_COUNT_SIZE: usize;

    type CountIdx: IndexInterface + IndexSizeCheck<Inner: FromAs<usize>>;

    fn base_ptr(&self) -> *const [u8; 8];

    fn set_clear_at_count(&self, count: Self::CountIdx);

    /// Sets the enable register for the receive ring_buffer. When enabled, incoming frames from
    /// the network will be written to the buffer at an address determined by an internal free
    /// running counter. When disabled, incoming frames will be ignored, but the counter will
    /// continue to increment to maintain alignment and the current contents of the buffer can still be read.
    fn set_enable(&self, enabled: bool);

    /// Get the current value of the enable register for the receive ring_buffer.
    fn get_enable(&self) -> bool;

    /// Read a slice from the receive buffer at the given offset. The slice must not exceed the buffer length when combined with the offset.
    fn read_slice(&self, dst: &mut [[u8; 8]], offset: usize) {
        assert!(dst.len() + offset <= Self::DATA_LEN);
        trace!(
            "ring_buffer rx read_slice len {} offset {}",
            dst.len(),
            offset
        );
        unsafe {
            self.read_slice_unchecked(dst, offset);
        }
    }

    /// Read a slice from the receive buffer at the given offset without checking bounds. The caller must ensure that `dst.len() + offset` does not exceed the buffer length.
    ///
    /// # Safety
    /// This function is unsafe because it can cause out-of-bounds memory access if the caller
    /// does not ensure that `dst.len() + offset` is within the bounds of the receive buffer.
    unsafe fn read_slice_unchecked(&self, dst: &mut [[u8; 8]], offset: usize) {
        let src_ptr = self.base_ptr().add(offset);
        let dst_ptr = dst.as_mut_ptr();
        if !(src_ptr as usize).is_multiple_of(4) || !(dst_ptr as usize).is_multiple_of(4) {
            warn!("ring_buffer rx read_slice_unchecked unaligned: src {src_ptr:p} dst {dst_ptr:p}");
        }
        // Use volatile reads to ensure we respect hardware buffer semantics
        for i in 0..dst.len() {
            dst_ptr.add(i).write(src_ptr.add(i).read_volatile());
        }
    }
}

macro_rules! impl_ring_buffer_interfaces {
    (rx: $rx:ty, tx: $tx:ty, cidx: $cidx:ty$(,)?) => {
        const _: () = {
            if <$rx>::DATA_LEN != <$tx>::DATA_LEN {
                const_panic::concat_panic!(
                    "RingBuffer sizes do not match for ",
                    stringify!($rx),
                    " and ",
                    stringify!($tx)
                );
            }
        };

        impl ReceiveRingBufferInterface for $rx {
            const DATA_LEN: usize = <$rx>::DATA_LEN;
            const CLEAR_AT_COUNT_SIZE: usize = <$rx>::CLEAR_AT_COUNT_SIZE;

            type CountIdx = $cidx;

            fn base_ptr(&self) -> *const [u8; 8] {
                self.0.cast::<[u8; 8]>()
            }
            fn set_clear_at_count(&self, count: $cidx) {
                assert!(usize::from_as(count.into_inner()) < Self::CLEAR_AT_COUNT_SIZE);
                <$rx>::set_clear_at_count(self, count.try_into().unwrap());
            }
            fn set_enable(&self, enabled: bool) {
                <$rx>::set_enable(self, enabled);
            }
            fn get_enable(&self) -> bool {
                <$rx>::enable(self)
            }
        }

        impl TransmitRingBufferInterface for $tx {
            const DATA_LEN: usize = <$tx>::DATA_LEN;

            fn base_ptr(&self) -> *mut [u8; 8] {
                self.0.cast::<[u8; 8]>()
            }

            fn set_enable(&self, enabled: bool) {
                <$tx>::set_enable(self, enabled);
            }
            fn get_enable(&self) -> bool {
                <$tx>::enable(self)
            }
        }
    };
}

impl_ring_buffer_interfaces! {
    rx: crate::shared_devices::ReceiveRingBuffer,
    tx: crate::shared_devices::TransmitRingBuffer,
    cidx: Index<16, u8>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum AlignPhase {
    Unaligned,
    FindingAlignment,
    AcknowledgingAlignment,
    WaitingForZeroes,
    Aligned,
}

pub struct AlignedReceiveBuffer<Rx, Tx> {
    pub buffer: Rx,
    phase: AlignPhase,
    _tx: core::marker::PhantomData<Tx>,
}

impl<Rx, Tx> AlignedReceiveBuffer<Rx, Tx>
where
    Rx: ReceiveRingBufferInterface,
    Tx: TransmitRingBufferInterface,
{
    pub fn new(buffer: Rx) -> Self {
        debug!("ring_buffer aligned receive buffer new");
        Self {
            buffer,
            phase: AlignPhase::Unaligned,
            _tx: core::marker::PhantomData,
        }
    }

    pub fn is_aligned(&self) -> bool {
        self.phase == AlignPhase::Aligned
    }

    /// Perform one step of the alignment procedure. Returns `true` when alignment
    /// is fully complete (including acknowledgement).
    pub fn align_step(&mut self, tx: &Tx) -> bool {
        match self.phase {
            AlignPhase::Unaligned => {
                debug!("ring_buffer align_start");
                assert_eq!(self.phase, AlignPhase::Unaligned);
                tx.clear();
                let announce_pattern = [ALIGNMENT_ANNOUNCE.to_le_bytes()];
                tx.set_enable(true);
                self.buffer.set_enable(true);
                tx.write_slice(&announce_pattern, 0);
                self.phase = AlignPhase::FindingAlignment;
                false
            }
            AlignPhase::Aligned => true,
            AlignPhase::FindingAlignment => {
                for rx_idx in 0..Rx::DATA_LEN {
                    let data_buf = unsafe { self.buffer.base_ptr().add(rx_idx).read_volatile() };
                    let value = u64::from_le_bytes(data_buf);

                    if value == ALIGNMENT_ANNOUNCE || value == ALIGNMENT_ACKNOWLEDGE {
                        debug!("ring_buffer align marker at rx_idx {rx_idx}");
                        if rx_idx == 0 {
                            let ack_pattern = [ALIGNMENT_ACKNOWLEDGE.to_le_bytes()];
                            tx.write_slice(&ack_pattern, 0);
                            self.phase = AlignPhase::AcknowledgingAlignment;
                            return false;
                        } else {
                            self.buffer.set_clear_at_count(unsafe {
                                Rx::CountIdx::idx_new_unchecked(rx_idx.into_as())
                            });
                        }
                    }
                }
                false
            }
            AlignPhase::AcknowledgingAlignment => {
                let data_buf = unsafe { self.buffer.base_ptr().read_volatile() };
                let value = u64::from_le_bytes(data_buf);

                // We consider ourselves aligned as soon as we see either the acknowledge pattern
                // as sent by our partner, or our partner has disabled transmission (which results)
                // in us seeing zeroes in the buffer.
                if value == ALIGNMENT_ACKNOWLEDGE || value == 0 {
                    debug!("Partner acknowledged, disabling TX");
                    tx.set_enable(false);
                    self.phase = AlignPhase::WaitingForZeroes;
                }
                false
            }
            AlignPhase::WaitingForZeroes => {
                let data_buf = unsafe { self.buffer.base_ptr().read_volatile() };
                let value = u64::from_le_bytes(data_buf);
                if value == 0 {
                    debug!("Alignment complete");
                    self.buffer.set_enable(false);
                    self.phase = AlignPhase::Aligned;
                    return true;
                }
                false
            }
        }
    }

    pub fn clear_alignment(&mut self) {
        debug!("ring_buffer clear alignment");
        self.phase = AlignPhase::Unaligned;
    }
}
