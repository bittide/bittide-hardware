// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
use ufmt::{uDisplay, uWrite};

#[derive(Clone)]
pub struct String<const SIZE: usize>(pub heapless::String<SIZE>);

impl <const SIZE: usize> String<SIZE> {
    pub fn new() -> Self {
        String(heapless::String::new())
    }
}

impl<const SIZE: usize> uDisplay for String<SIZE> {
    #[inline(always)]
    fn fmt<W>(&self, f: &mut ufmt::Formatter<'_, W>) -> Result<(), W::Error>
    where
        W: uWrite + ?Sized,
    {
        f.write_str(&self.0.as_str())
    }
}

impl <const SIZE: usize>  ufmt::uWrite for String<SIZE> {
    fn write_str(&mut self, s: &str) -> Result<(), Self::Error> {
        self.0.push_str(s)?;
        Ok(())
    }
    type Error = ();
}
