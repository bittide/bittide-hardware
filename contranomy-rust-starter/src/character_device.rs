pub const CHARACTER_DEVICE_ADDR: usize = 0x90000000;

pub fn write_byte(b: u8) {
    unsafe {
        core::ptr::write_volatile(CHARACTER_DEVICE_ADDR as *mut u8, b);
    }
}

pub fn write_str(s: &str) {
    for b in s.bytes() {
        write_byte(b);
    }
}

pub struct DebugWriter;

impl core::fmt::Write for DebugWriter {
    fn write_str(&mut self, s: &str) -> core::fmt::Result {
        write_str(s);
        Ok(())
    }
}

pub const DEBUG_IO: DebugWriter = DebugWriter;
