// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

static mut DEVICE_ADDR: Option<*mut u8> = None;

pub struct CharacterDevice;

/// Initialises the character device
///
/// # Safety
///
/// The `character_device_addr` argument must be the address of the character
/// device interface.
pub unsafe fn initialise(character_device_addr: *mut u8) -> bool {
    if DEVICE_ADDR.is_some() {
        return false;
    }

    DEVICE_ADDR = Some(character_device_addr);

    true
}

impl ufmt::uWrite for CharacterDevice {
    type Error = ();

    fn write_str(&mut self, s: &str) -> Result<(), Self::Error> {
        // SAFETY: this RISC-V implementation does not support multiple threads
        //         of execution, so the access is essentially thread-local.
        let addr = if let Some(addr) = unsafe { DEVICE_ADDR } {
            addr
        } else {
            return Err(());
        };

        for b in s.bytes() {
            // SAFETY: the address has been set by the `initialise` function, so
            //         that the option is `Some` and contains a valid pointer.
            unsafe {
                core::ptr::write_volatile(addr, b);
            }
        }
        Ok(())
    }
}

//
// re-export of common macros from the `std` for IO.
//

#[macro_export]
macro_rules! print {
    () => {
        $crate::print!("")
    };
    ($($t:tt)*) => {{
        let _ = ufmt::uwrite!(&mut $crate::character_device::CharacterDevice, $($t)*);
    }};
}

#[macro_export]
macro_rules! println {
    () => {
        $crate::println!("")
    };
    ($($t:tt)*) => {{
        let _ = ufmt::uwriteln!(&mut $crate::character_device::CharacterDevice, $($t)*).unwrap();
    }};
}

#[macro_export]
macro_rules! dbg {
    // NOTE: We cannot use `concat!` to make a static string as a format argument
    // of `eprintln!` because `file!` could contain a `{` or
    // `$val` expression could be a block (`{ .. }`), in which case the `eprintln!`
    // will be malformed.
    () => {
        $crate::println!("[{}:{}]", core::file!(), core::line!())
    };
    ($val:expr $(,)?) => {
        // Use of `match` here is intentional because it affects the lifetimes
        // of temporaries - https://stackoverflow.com/a/48732525/1063961
        match $val {
            tmp => {
                $crate::println!("[{}:{}] {} = {:#?}",
                    core::file!(), core::line!(), core::stringify!($val), &tmp);
                tmp
            }
        }
    };
    ($($val:expr),+ $(,)?) => {
        ($($crate::dbg!($val)),+,)
    };
}
