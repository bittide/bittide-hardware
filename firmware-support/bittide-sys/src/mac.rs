// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use ufmt::derive::uDebug;
pub struct Mac {
    status: *const MacStatus,
}

impl Mac {
    pub fn new(addr: usize) -> Mac {
        Mac {
            status: addr as *const MacStatus,
        }
    }
    pub fn read(&self) -> MacStatus {
        unsafe { *self.status }
    }
}

/// Contains counters that are used the occurance of various events in the MAC.
#[repr(C)]
#[derive(uDebug, PartialEq, Eq, Copy, Clone)]
pub struct MacStatus {
    pub tx_fifo_underflow_counter: u32,
    pub tx_fifo_overflow_counter: u32,
    pub tx_fifo_bad_frame_counter: u32,
    pub tx_fifo_good_frame_counter: u32,
    pub rx_bad_frame_counter: u32,
    pub rx_bad_fcs_counter: u32,
    pub rx_fifo_overflow_counter: u32,
    pub rx_fifo_bad_frame_counter: u32,
    pub rx_fifo_good_frame_counter: u32,
}
