// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use core::result::Result::{self, *};

use crate::shared::devices::uart::*;

impl core::fmt::Write for Uart {
    fn write_str(&mut self, s: &str) -> core::fmt::Result {
        for b in s.as_bytes() {
            self.set_data(*b);
        }
        Ok(())
    }
}

impl ufmt::uWrite for Uart {
    type Error = ();

    fn write_str(&mut self, s: &str) -> Result<(), Self::Error> {
        for b in s.as_bytes() {
            self.set_data(*b);
        }
        Ok(())
    }
}
