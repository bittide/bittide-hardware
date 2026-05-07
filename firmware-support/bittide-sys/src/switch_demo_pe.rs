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

pub trait NodeIterator {
    fn node_data(&self) -> impl Iterator<Item = NodeData> + '_;
}

macro_rules! impl_node_iterator {
    ($($ty:path),+$(,)?) => {
        $(
            impl NodeIterator for $ty {
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

impl_node_iterator! {
    bittide_hal::hals::switch_demo_mu::devices::SwitchDemoPe,
    bittide_hal::hals::switch_demo_pe_test::devices::SwitchDemoPe,
}
