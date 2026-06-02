// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use crate::types::mode::Mode;

pub trait HandshakesInterface {
    fn set_modes(&self, idx: usize, val: Mode) -> Option<()>;

    fn software_ready(&self, idx: usize) -> Option<bool>;
    fn set_software_ready(&self, idx: usize, val: bool) -> Option<()>;

    fn neighbor_software_ready(&self, idx: usize) -> Option<bool>;

    fn receive_ready(&self, idx: usize) -> Option<bool>;
    fn set_receive_ready(&self, idx: usize, val: bool) -> Option<()>;

    fn receive_done(&self, idx: usize) -> Option<bool>;

    fn handshake_done(&self, idx: usize) -> Option<bool>;
}

macro_rules! impl_handshakes_interface {
    ($t:ty) => {
        impl HandshakesInterface for $t {
            fn set_modes(&self, idx: usize, val: Mode) -> Option<()> {
                <$t>::set_modes(self, idx, val)
            }

            fn software_ready(&self, idx: usize) -> Option<bool> {
                <$t>::software_readys(self).get(idx)
            }
            fn set_software_ready(&self, idx: usize, val: bool) -> Option<()> {
                let mut mask = <$t>::software_readys(self);
                mask.set(idx, val)?;
                <$t>::set_software_readys(self, mask);
                Some(())
            }

            fn neighbor_software_ready(&self, idx: usize) -> Option<bool> {
                <$t>::neighbor_software_readys(self).get(idx)
            }

            fn receive_ready(&self, idx: usize) -> Option<bool> {
                <$t>::receive_readys(self).get(idx)
            }
            fn set_receive_ready(&self, idx: usize, val: bool) -> Option<()> {
                let mut mask = <$t>::receive_readys(self);
                mask.set(idx, val)?;
                <$t>::set_receive_readys(self, mask);
                Some(())
            }

            fn receive_done(&self, idx: usize) -> Option<bool> {
                <$t>::receive_dones(self).get(idx)
            }

            fn handshake_done(&self, idx: usize) -> Option<bool> {
                <$t>::handshake_dones(self).get(idx)
            }
        }
    };
}

impl_handshakes_interface!(crate::shared_devices::Handshakes);
