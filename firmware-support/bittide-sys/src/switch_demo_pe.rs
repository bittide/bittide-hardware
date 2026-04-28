// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use ufmt::derive::uDebug;

#[repr(C)]
#[derive(uDebug, PartialEq, Eq, Copy, Clone)]
pub struct NodeData {
    pub local_counter: u64,
    pub dna: u128,
}

pub trait SdpeUtils {
    type ReadCycles;
    type WriteCycles;

    fn set_read(&mut self, read_start: u64, read_cycles: Self::ReadCycles);
    fn set_write(&mut self, write_start: u64, write_cycles: Self::WriteCycles);
    fn get_read(&self) -> (u64, Self::ReadCycles);
    fn get_write(&self) -> (u64, Self::WriteCycles);
    fn node_data(&self) -> impl Iterator<Item = NodeData> + '_;
}

macro_rules! impl_sdpe_utils {
    (
        $(
            $ty:path => $rc:ty, $wc:ty;
        )+
    ) => {
        $(
            impl SdpeUtils for $ty {
                type ReadCycles = $rc;
                type WriteCycles = $wc;

                fn set_read(&mut self, read_start: u64, read_cycles: $rc) {
                    self.set_read_start(read_start);
                    self.set_read_cycles(read_cycles);
                }

                fn set_write(&mut self, write_start: u64, write_cycles: $wc) {
                    self.set_write_start(write_start);
                    self.set_write_cycles(write_cycles);
                }

                fn get_read(&self) -> (u64, $rc) {
                    (self.read_start(), self.read_cycles())
                }

                fn get_write(&self) -> (u64, $wc) {
                    (self.write_start(), self.write_cycles())
                }

                fn node_data(&self) -> impl Iterator<Item = NodeData> + '_ {
                    const LEN: usize = <$ty>::BUFFER_LEN / 3;
                    const INDICES: [usize; LEN] = {
                        let mut indices = [0; LEN];
                        let mut i = 0;
                        while i < indices.len() {
                            indices[i] = i * 3;
                            i += 1;
                        }
                        indices
                    };
                    INDICES
                        .into_iter()
                        .map(|idx| unsafe {
                            let local_counter = u64::from_ne_bytes(self.buffer_unchecked(idx));
                            let dna_lo = u64::from_ne_bytes(self.buffer_unchecked(idx + 1));
                            let dna_hi = u64::from_ne_bytes(self.buffer_unchecked(idx + 2));
                            NodeData {
                                local_counter,
                                dna: dna_lo as u128 | ((dna_hi as u128) << 64),
                            }
                        })
                }
            }
        )+
    }
}

impl_sdpe_utils! {
    bittide_hal::hals::switch_demo_mu::devices::SwitchDemoPe => u8, u8;
    bittide_hal::hals::switch_demo_pe_test::devices::SwitchDemoPe => u8, u8;
}
