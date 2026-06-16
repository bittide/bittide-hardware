// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use clash_bindings::{bitvector::BitVectorInterface, unsigned::UnsignedInterface, IntoAs};
use clash_macros::{bitvector, unsigned, BitVector, Unsigned};

pub mod self_test;

pub trait AxiRxInterface {
    const ARI_DATA_LEN: usize;
    const ARI_PACKET_LENGTH_WIDTH: usize;
    const ARI_STATUS_WIDTH: usize;

    type Data: BitVectorInterface;
    type PacketLen: UnsignedInterface<Inner: IntoAs<usize>> + IntoAs<usize>;
    type Status: BitVectorInterface;

    /// Returns the raw pointer to the AXI RX interface.
    fn ari_ptr(&self) -> *const u8;

    /// Construct a new AXI RX interface.
    ///
    /// # Safety
    ///
    /// The underlying pointer must be properly aligned and be a memory mapped address belonging to
    /// an AXI RX peripheral. If this is not true, very little is likely to happen, but also any
    /// assumptions made about how this type behaves will be broken.
    unsafe fn ari_new(addr: *mut u8) -> Self;

    /// Buffer for incoming AXI4 stream packets
    fn ari_data(&self, idx: usize) -> Option<Self::Data>;

    /// Buffer for incoming AXI4 stream packets
    ///
    /// # Safety
    ///
    /// The user must ensure that `idx` is within bounds (`ARI_DATA_LEN`) for this function to be
    /// called safely.
    unsafe fn ari_data_unchecked(&self, idx: usize) -> Self::Data;

    /// Buffer for incoming AXI4 stream packets
    fn ari_data_volatile_iter(&self) -> impl DoubleEndedIterator<Item = Self::Data> + '_;

    /// Number of bytes in the buffer
    fn ari_packet_length(&self) -> Self::PacketLen;

    /// Status register: bit 1 = packet_complete, bit 0 = buffer_full
    fn ari_status(&self) -> Self::Status;

    /// Number of bytes in the buffer
    fn ari_set_packet_length(&self, val: Self::PacketLen);

    /// Status register: bit 1 = packet_complete, bit 0 = buffer_full
    fn ari_set_status(&self, val: Self::Status);
}

pub struct AxiRxStatus {
    pub buffer_full: bool,
    pub packet_complete: bool,
}

pub trait AxiRx: AxiRxInterface<Status = BitVector!(2), PacketLen = Unsigned!(32)> {
    /// Returns true if there is a packet in the buffer or the buffer is full.
    #[inline]
    fn has_data(&self) -> bool {
        self.ari_status().into_inner()[0] != 0
    }

    /// Returns a struct with the status of the buffer.
    #[inline]
    fn read_status(&self) -> AxiRxStatus {
        let [bits] = self.ari_status().into_inner();
        AxiRxStatus {
            buffer_full: bits & 0b1 == 0b1,
            packet_complete: bits & 0b10 == 0b10,
        }
    }

    /// Clears the bits in the status register.
    #[inline]
    fn clear_status(&self) {
        self.ari_set_status(bitvector!(0b00, n = 2));
    }

    /// Clear packet register by setting packet length to 0.
    #[inline]
    fn clear_packet_register(&self) {
        self.ari_set_packet_length(unsigned!(0, n = 32));
    }

    /// Clears any received packet by clearing both the status register and packet length register.
    #[inline]
    fn clear_packet(&self) {
        self.clear_packet_register();
        self.clear_status();
    }

    /// Make up to `attempts` attempts to receive a packet. If this count is exceeded, `None` is
    /// returned. If `Some(n)` is returned, `n` is the number of bytes received.
    #[inline]
    fn receive_with_timeout(&self, buffer: &mut [u8], attempts: usize) -> Option<usize> {
        for _ in 0..attempts {
            if let Some(s) = self.try_receive(buffer) {
                return Some(s);
            }
        }
        None
    }

    /// Blocks until a packet is successfully received, and returns the number of bytes read.
    #[inline]
    fn receive_blocking(&self, buffer: &mut [u8]) -> usize {
        loop {
            if let Some(s) = self.try_receive(buffer) {
                return s;
            }
        }
    }

    /// If a completed packet is present in the buffer, copy the data into `buffer` and return
    /// `Some(_)` containing the number of bytes read. Otherwise, return `None`.
    #[inline]
    fn try_receive(&self, buffer: &mut [u8]) -> Option<usize> {
        // Check if there is data to receive, this means either the buffer is full or a packet is
        // complete. We can check by using the utility function read_status
        if self.ari_status() == bitvector!(0b00, n = 2) {
            return None;
        }

        // Get length of the incoming data
        let len = self.ari_packet_length().into_as();

        debug_assert!(len <= buffer.len(), "Buffer too small to receive packet");

        unsafe {
            core::ptr::copy_nonoverlapping(self.ari_ptr(), buffer.as_mut_ptr(), len);
        }

        Some(len)
    }

    /// Get access to the backing buffer as a slice.
    #[inline]
    fn get_slice(&self) -> &[u8] {
        let l = self.ari_packet_length().into_as();
        unsafe { core::slice::from_raw_parts(self.ari_ptr(), l) }
    }
}

macro_rules! impl_axi_rx {
    ($($t:ty),+$(,)?) => {
        $(
            impl AxiRxInterface for $t {
                const ARI_DATA_LEN: usize = Self::DATA_LEN;
                const ARI_PACKET_LENGTH_WIDTH: usize = Self::PACKET_LENGTH_WIDTH;
                const ARI_STATUS_WIDTH: usize = Self::STATUS_WIDTH;

                type Data = BitVector!(32);
                type PacketLen = Unsigned!(32);
                type Status = BitVector!(2);

                #[inline]
                fn ari_ptr(&self) -> *const u8 {
                    self.0
                }

                #[inline]
                unsafe fn ari_new(addr: *mut u8) -> Self {
                    Self::new(addr)
                }

                #[inline]
                fn ari_data(&self, idx: usize) -> Option<Self::Data> {
                    self.data(idx)
                }

                #[inline]
                unsafe fn ari_data_unchecked(&self, idx: usize) -> Self::Data {
                    self.data_unchecked(idx)
                }

                #[inline]
                fn ari_data_volatile_iter(&self) -> impl DoubleEndedIterator<Item = Self::Data> + '_ {
                    self.data_volatile_iter()
                }

                #[inline]
                fn ari_packet_length(&self) -> Self::PacketLen {
                    self.packet_length()
                }

                #[inline]
                fn ari_status(&self) -> Self::Status {
                    self.status()
                }

                #[inline]
                fn ari_set_packet_length(&self, val: Self::PacketLen) {
                    self.set_packet_length(val)
                }

                #[inline]
                fn ari_set_status(&self, val: Self::Status) {
                    self.set_status(val)
                }
            }

            impl AxiRx for $t {}
        )+
    };
}

impl_axi_rx! {
    crate::axi_stream_self_test::devices::AxiRxBuffer,
    crate::ethernet::devices::AxiRxBuffer,
}

pub trait AxiTxInterface {
    const ATI_DATA_LEN: usize;

    type Data: BitVectorInterface;

    /// Returns the raw pointer to the AXI TX interface
    fn ati_ptr(&self) -> *const u8;

    /// Construct a new AXI TX interface.
    ///
    /// # Safety
    ///
    /// The underlying pointer must be properly aligned and be a memory mapped address belonging to
    /// an AXI TX peripheral. If this is not true, very little is likely to happen, but also any
    /// assumptions made about how this type behaves will be broken.
    unsafe fn ati_new(addr: *mut u8) -> Self;

    /// Write data to the AXI4 stream
    fn ati_set_data(&self, idx: usize, val: Self::Data) -> Option<()>;

    /// Write data to the AXI4 stream without doing a bounds check
    ///
    /// # Safety
    ///
    /// The user must ensure that `idx` is within bounds (`ATI_DATA_LEN`) before calling this
    /// function.
    unsafe fn ati_set_data_unchecked(&self, idx: usize, val: Self::Data);

    /// Write to send a transfer with _tlast set
    fn ati_set_send(&self);
}

pub trait AxiTx: AxiTxInterface {
    #[inline]
    fn send(&self, packet: &[u8]) {
        let _: () = const {
            if core::mem::size_of::<usize>() != core::mem::size_of::<u32>() {
                panic!("This function is written assuming that `usize` is the same size as `u32`.");
            }
        };

        if packet.is_empty() {
            self.ati_set_send();
            return;
        }

        // Deal with unaligned packets by splitting them into 3 parts
        // The use of `align_to` is safe because the binary representation of 4 bytes is still the
        // same as 1 word
        let (bytes_slice_prefix, words_slice, bytes_slice_suffix) =
            unsafe { packet.align_to::<u32>() };

        for &byte in bytes_slice_prefix {
            unsafe { self.ati_ptr().cast_mut().write_volatile(byte) };
        }
        // Coerce the payload address to a u32 pointer
        let dst: *mut u32 = self.ati_ptr().cast_mut().cast();
        for &word in words_slice {
            unsafe { dst.write_volatile(word) };
        }
        for &byte in bytes_slice_suffix {
            unsafe { self.ati_ptr().cast_mut().write_volatile(byte) };
        }

        // Initiate transmission by writing to the `send` register
        self.ati_set_send();
    }
}

macro_rules! impl_axi_tx {
    ($($t:ty),+$(,)?) => {
        $(
            impl AxiTxInterface for $t {
                const ATI_DATA_LEN: usize = Self::DATA_LEN;

                type Data = BitVector!(32);

                #[inline]
                fn ati_ptr(&self) -> *const u8 {
                    self.0
                }

                #[inline]
                unsafe fn ati_new(addr: *mut u8) -> Self {
                    Self::new(addr)
                }

                #[inline]
                fn ati_set_data(&self, idx: usize, val: Self::Data) -> Option<()> {
                    self.set_data(idx, val)
                }

                #[inline]
                unsafe fn ati_set_data_unchecked(&self, idx: usize, val: Self::Data) {
                    self.set_data_unchecked(idx, val);
                }

                #[inline]
                fn ati_set_send(&self) {
                    self.set_send(());
                }
            }

            impl AxiTx for $t {}
        )+
    };
}

impl_axi_tx! {
    crate::shared_devices::AxiStreamTx,
}
