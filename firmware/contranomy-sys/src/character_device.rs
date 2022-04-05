static mut DEVICE_ADDR: usize = 0;

pub struct CharacterDevice;

/// Initialises the character device
///
/// # Safety
///
/// The `character_device_addr` argument must be the integer representation of
/// the address which contains the character device interface.
pub unsafe fn initialise(character_device_addr: usize) {
    DEVICE_ADDR = character_device_addr;
}

fn write_byte(b: u8) {
    unsafe {
        core::ptr::write_volatile(DEVICE_ADDR as *mut u8, b);
    }
}

impl core::fmt::Write for CharacterDevice {
    fn write_str(&mut self, s: &str) -> core::fmt::Result {
        for b in s.bytes() {
            write_byte(b);
        }
        Ok(())
    }
}

//
// re-export of common macros from the `std` for IO.
//

#[macro_export]
macro_rules! print {
    ($($t:tt)*) => {
        write!($crate::character_device::CharacterDevice, $($t)*).unwrap();
    };
}

#[macro_export]
macro_rules! println {
    ($($t:tt)*) => {
        writeln!($crate::character_device::CharacterDevice, $($t)*).unwrap();
    };
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
