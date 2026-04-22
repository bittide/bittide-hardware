// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use log::{debug, trace, warn};

const ALIGNMENT_ANNOUNCE: u64 = 0xBADC0FFEE;
const ALIGNMENT_ACKNOWLEDGE: u64 = 0xDEADABBA;

pub trait TransmitRingbufferInterface {
    const DATA_LEN: usize;

    /// Get a pointer to the base address of the TransmitRingbuffer
    fn base_ptr(&self) -> *mut [u8; 8];

    /// Enable transmission of frames to the network. When disabled, will transmit zeroes.
    fn set_enable(&self, enabled: bool);

    fn get_enable(&self) -> bool;

    /// Write a slice to the transmit buffer at the given offset. The slice must not exceed the buffer length when combined with the offset.
    fn write_slice(&self, src: &[[u8; 8]], offset: usize) {
        assert!(src.len() + offset <= Self::DATA_LEN);
        trace!(
            "ringbuffer tx write_slice len {} offset {}",
            src.len(),
            offset
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
                "ringbuffer tx write_slice_unchecked unaligned: src {:p} dst {:p}",
                src_ptr, dst_ptr
            );
        }

        core::ptr::copy_nonoverlapping(src_ptr, dst_ptr, src.len());
    }

    /// Write a slice to the transmit buffer wrapping if `src.len() + offset`exceeds the length
    /// of the buffer.
    ///
    /// # Panic
    /// Panics if the length of the source exceeds the length of the buffer.
    fn write_slice_with_wrap(&self, src: &[[u8; 8]], offset: usize) {
        assert!(src.len() <= Self::DATA_LEN);
        unsafe {
            self.write_slice_with_wrap_unchecked(src, offset);
        }
    }

    /// Write a slice to the transmit buffer without bounds checking. It wraps if `src.len() + offset`exceeds the length
    /// of the buffer.
    ///
    /// # Safety
    /// Caller must ensure that the size of the source does not exceed the buffer length.
    unsafe fn write_slice_with_wrap_unchecked(&self, src: &[[u8; 8]], offset: usize) {
        if src.len() + offset <= Self::DATA_LEN {
            self.write_slice(src, offset);
        } else {
            let first_part_len = Self::DATA_LEN - offset;
            let (first, second) = src.split_at(first_part_len);
            debug!(
                "ringbuffer tx write_slice_with_wrap offset {} first {} second {}",
                offset,
                first.len(),
                second.len()
            );
            self.write_slice(first, offset);
            self.write_slice(second, 0);
        }
    }

    fn clear(&self) {
        debug!("ringbuffer tx clear len {}", Self::DATA_LEN);
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
            "ringbuffer tx read_slice len {} offset {}",
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
            warn!(
                "ringbuffer tx read_slice_unchecked unaligned: src {:p} dst {:p}",
                src_ptr, dst_ptr
            );
        }
        // Use volatile reads to read back from hardware buffer
        for i in 0..dst.len() {
            dst_ptr.add(i).write(src_ptr.add(i).read_volatile());
        }
    }
}

pub trait ReceiveRingbufferInterface {
    const DATA_LEN: usize;
    const CLEAR_AT_COUNT_SIZE: usize;

    fn base_ptr(&self) -> *const [u8; 8];

    fn set_clear_at_count(&self, count: usize);

    fn set_enable(&self, enabled: bool);
    fn get_enable(&self) -> bool;

    /// Read a slice from the receive buffer at the given offset. The slice must not exceed the buffer length when combined with the offset.
    fn read_slice(&self, dst: &mut [[u8; 8]], offset: usize) {
        assert!(dst.len() + offset <= Self::DATA_LEN);
        trace!(
            "ringbuffer rx read_slice len {} offset {}",
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
            warn!(
                "ringbuffer rx read_slice_unchecked unaligned: src {:p} dst {:p}",
                src_ptr, dst_ptr
            );
        }
        // Use volatile reads to ensure we respect hardware buffer semantics
        for i in 0..dst.len() {
            dst_ptr.add(i).write(src_ptr.add(i).read_volatile());
        }
    }
}

macro_rules! impl_ringbuffer_interfaces {
    (rx: $rx:ty, tx: $tx:ty) => {
        const _: () = {
            if <$rx>::DATA_LEN != <$tx>::DATA_LEN {
                const_panic::concat_panic!(
                    "Ringbuffer sizes do not match for ",
                    stringify!($rx),
                    " and ",
                    stringify!($tx)
                );
            }
        };

        impl ReceiveRingbufferInterface for $rx {
            const DATA_LEN: usize = <$rx>::DATA_LEN;
            const CLEAR_AT_COUNT_SIZE: usize = <$rx>::CLEAR_AT_COUNT_SIZE;

            fn base_ptr(&self) -> *const [u8; 8] {
                self.0.cast::<[u8; 8]>()
            }
            fn set_clear_at_count(&self, count: usize) {
                assert!(count < Self::CLEAR_AT_COUNT_SIZE);
                <$rx>::set_clear_at_count(self, count.try_into().unwrap());
            }
            fn set_enable(&self, enabled: bool) {
                <$rx>::set_enable(self, enabled);
            }
            fn get_enable(&self) -> bool {
                <$rx>::enable(self)
            }
        }

        impl TransmitRingbufferInterface for $tx {
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

impl_ringbuffer_interfaces! {
    rx: crate::hals::ringbuffer_test::devices::ReceiveRingbuffer,
    tx: crate::hals::ringbuffer_test::devices::TransmitRingbuffer
}

impl_ringbuffer_interfaces! {
    rx: crate::hals::soft_ugn_demo_mu::devices::ReceiveRingbuffer,
    tx: crate::hals::soft_ugn_demo_mu::devices::TransmitRingbuffer
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
    tx_reference: usize,
    phase: AlignPhase,
    _tx: core::marker::PhantomData<Tx>,
}

impl<Rx, Tx> AlignedReceiveBuffer<Rx, Tx>
where
    Rx: ReceiveRingbufferInterface,
    Tx: TransmitRingbufferInterface,
{
    pub fn new(buffer: Rx) -> Self {
        debug!("ringbuffer aligned receive buffer new");
        Self {
            buffer,
            tx_reference: 0,
            phase: AlignPhase::Unaligned,
            _tx: core::marker::PhantomData,
        }
    }

    pub fn is_aligned(&self) -> bool {
        self.phase == AlignPhase::Aligned
    }

    pub fn align(&mut self, tx: &Tx) {
        debug!("ringbuffer align start");
        tx.clear();
        let announce_pattern = [ALIGNMENT_ANNOUNCE.to_le_bytes()];
        tx.set_enable(true);
        self.buffer.set_enable(true);
        tx.write_slice(&announce_pattern, 0);

        let mut aligned = false;
        while !aligned {
            for rx_idx in 0..Rx::DATA_LEN {
                let data_buf = unsafe { self.buffer.base_ptr().add(rx_idx).read_volatile() };
                let value = u64::from_le_bytes(data_buf);

                if value == ALIGNMENT_ANNOUNCE || value == ALIGNMENT_ACKNOWLEDGE {
                    debug!("ringbuffer align marker at rx_idx {}", rx_idx);
                    if rx_idx == 0 {
                        aligned = true;
                    } else {
                        self.buffer.set_clear_at_count(rx_idx);
                    }
                }
            }
        }

        let ack_pattern = [ALIGNMENT_ACKNOWLEDGE.to_le_bytes()];
        tx.write_slice(&ack_pattern, 0);

        loop {
            let data_buf = unsafe { self.buffer.base_ptr().read_volatile() };
            let value = u64::from_le_bytes(data_buf);
            if value == ALIGNMENT_ACKNOWLEDGE {
                break;
            }
        }
        debug!("Partner acknowledged, disabling TX");
        tx.set_enable(false);

        loop {
            let data_buf = unsafe { self.buffer.base_ptr().read_volatile() };
            let value = u64::from_le_bytes(data_buf);
            if value == 0 {
                break;
            }
        }
        debug!("Alignment complete");
        self.buffer.set_enable(false);
        self.tx_reference = tx.base_ptr() as *const _ as usize;
        self.phase = AlignPhase::Aligned;
    }

    /// Perform one step of the alignment procedure. Returns `true` when alignment
    /// is fully complete (including acknowledgement).
    pub fn align_step(&mut self, tx: &Tx) -> bool {
        match self.phase {
            AlignPhase::Unaligned => {
                debug!("ringbuffer align_start");
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
                        debug!("ringbuffer align marker at rx_idx {}", rx_idx);
                        if rx_idx == 0 {
                            let ack_pattern = [ALIGNMENT_ACKNOWLEDGE.to_le_bytes()];
                            tx.write_slice(&ack_pattern, 0);
                            self.phase = AlignPhase::AcknowledgingAlignment;
                            return false;
                        } else {
                            self.buffer.set_clear_at_count(rx_idx);
                        }
                    }
                }
                false
            }
            AlignPhase::AcknowledgingAlignment => {
                let data_buf = unsafe { self.buffer.base_ptr().read_volatile() };
                let value = u64::from_le_bytes(data_buf);
                if value == ALIGNMENT_ACKNOWLEDGE {
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
                    self.tx_reference = tx.base_ptr() as *const _ as usize;
                    self.phase = AlignPhase::Aligned;
                    return true;
                }
                false
            }
        }
    }

    pub fn clear_alignment(&mut self) {
        debug!("ringbuffer clear alignment");
        self.tx_reference = 0;
        self.phase = AlignPhase::Unaligned;
    }
}
