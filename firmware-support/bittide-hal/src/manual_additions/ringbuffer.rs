// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use log::warn;

const ALIGNMENT_ANNOUNCE: u64 = 0xBADC0FFEE;
const ALIGNMENT_ACKNOWLEDGE: u64 = 0xDEADABBA;

pub trait TransmitRingbufferInterface {
    const DATA_LEN: usize;

    /// Get a pointer to the base address of the TransmitRingbuffer
    fn base_ptr(&self) -> *mut [u8; 8];

    /// Write a slice to the transmit buffer at the given offset. The slice must not exceed the buffer length when combined with the offset.
    fn write_slice(&self, src: &[[u8; 8]], offset: usize) {
        assert!(src.len() + offset <= Self::DATA_LEN);
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
        if (src_ptr as usize) % 4 != 0 || (dst_ptr as usize) % 4 != 0 {
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
            self.write_slice(first, offset);
            self.write_slice(second, 0);
        }
    }

    fn clear(&self) {
        let zero = [[0u8; 8]; 1];
        for i in 0..Self::DATA_LEN {
            self.write_slice(&zero, i);
        }
    }
}

pub trait ReceiveRingbufferInterface {
    const DATA_LEN: usize;

    fn base_ptr(&self) -> *const [u8; 8];

    fn read_slice(&self, dst: &mut [[u8; 8]], offset: usize) {
        assert!(dst.len() + offset <= Self::DATA_LEN);
        unsafe {
            self.read_slice_unchecked(dst, offset);
        }
    }

    /// Read a slice from the buffer into the destination without bounds checking.
    ///
    /// # Safety
    /// Will fail if the requested size exceeds the size of the buffer.
    unsafe fn read_slice_unchecked(&self, dst: &mut [[u8; 8]], offset: usize) {
        let dst_ptr = dst.as_mut_ptr();
        let src_ptr = self.base_ptr().add(offset);
        if (src_ptr as usize) % 4 != 0 || (dst_ptr as usize) % 4 != 0 {
            warn!(
                "ringbuffer rx read_slice_unchecked unaligned: src {:p} dst {:p}",
                src_ptr, dst_ptr
            );
        }
        core::ptr::copy_nonoverlapping(src_ptr, dst_ptr, dst.len());
    }

    /// Read a slice from the buffer, wrapping around if the `dst.len() + offset` exceeds the
    /// length of the buffer.
    ///
    /// # Panics
    /// Will panic if the destination requests more bytes than the size of the buffer.
    fn read_slice_with_wrap(&self, dst: &mut [[u8; 8]], offset: usize) {
        assert!(dst.len() <= Self::DATA_LEN);
        unsafe {
            self.read_slice_with_wrap_unchecked(dst, offset);
        }
    }

    /// Read a slice from the buffer with wrapping, but no bounds checking
    ///
    /// # Safety
    /// Will fail if the destination requests more bytes than the size of the buffer.
    unsafe fn read_slice_with_wrap_unchecked(&self, dst: &mut [[u8; 8]], offset: usize) {
        if dst.len() + offset <= Self::DATA_LEN {
            self.read_slice(dst, offset);
        } else {
            let first_part_len = Self::DATA_LEN - offset;
            let (first, second) = dst.split_at_mut(first_part_len);
            self.read_slice(first, offset);
            self.read_slice(second, 0);
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

            fn base_ptr(&self) -> *const [u8; 8] {
                self.0.cast::<[u8; 8]>()
            }
        }

        impl TransmitRingbufferInterface for $tx {
            const DATA_LEN: usize = <$tx>::DATA_LEN;

            fn base_ptr(&self) -> *mut [u8; 8] {
                self.0.cast::<[u8; 8]>()
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

pub struct AlignedReceiveBuffer<Rx, Tx>
where
    Rx: ReceiveRingbufferInterface,
    Tx: TransmitRingbufferInterface,
{
    pub rx: Rx,
    rx_alignment_offset: Option<usize>,
    tx_reference: usize,
    _tx: core::marker::PhantomData<Tx>,
}

impl<Rx, Tx> AlignedReceiveBuffer<Rx, Tx>
where
    Rx: ReceiveRingbufferInterface,
    Tx: TransmitRingbufferInterface,
{
    pub fn new(rx: Rx) -> Self {
        Self {
            rx,
            rx_alignment_offset: None,
            tx_reference: 0,
            _tx: core::marker::PhantomData,
        }
    }

    pub fn is_aligned(&self) -> bool {
        self.rx_alignment_offset.is_some()
    }

    pub fn get_alignment_offset(&self) -> Option<usize> {
        self.rx_alignment_offset
    }

    pub fn align(&mut self, tx: &Tx) {
        tx.clear();
        let announce_pattern = [ALIGNMENT_ANNOUNCE.to_le_bytes()];
        tx.write_slice(&announce_pattern, 0);

        let rx_offset = 'outer: loop {
            for rx_idx in 0..Rx::DATA_LEN {
                let mut data_buf = [[0u8; 8]; 1];
                self.rx.read_slice(&mut data_buf, rx_idx);
                let value = u64::from_le_bytes(data_buf[0]);

                if value == ALIGNMENT_ANNOUNCE || value == ALIGNMENT_ACKNOWLEDGE {
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
        self.tx_reference = tx.base_ptr() as *const _ as usize;
    }

    pub fn clear_alignment(&mut self) {
        self.rx_alignment_offset = None;
        self.tx_reference = 0;
    }

    pub fn verify_aligned_to(&self, tx: &Tx) -> bool {
        self.is_aligned() && self.tx_reference == (tx.base_ptr() as *const _ as usize)
    }

    pub fn get_alignment_reference(&self) -> usize {
        self.tx_reference
    }

    pub fn read_slice(&self, dst: &mut [[u8; 8]], offset: usize) {
        assert!(dst.len() + offset <= Rx::DATA_LEN);
        let rx_offset = self
            .rx_alignment_offset
            .expect("Alignment offset not discovered yet. Call align() first.");
        let mut aligned_offset = offset + rx_offset;
        if aligned_offset >= Rx::DATA_LEN {
            aligned_offset -= Rx::DATA_LEN;
        }
        self.rx.read_slice_with_wrap(dst, aligned_offset)
    }
}
