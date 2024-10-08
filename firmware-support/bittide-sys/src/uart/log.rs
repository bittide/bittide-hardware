// SPDX-FileCopyrightText: 2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
use crate::uart;

// The logger utilizes core::fmt to format the log messages because ufmt formatting is not
// compatible with (dependencies of) the log crate.
use core::fmt::Write;
use log::LevelFilter;

/// A global logger instance to be used with the `log` crate.
///
/// Use `set_logger` to set the `Uart` instance to be used for logging.
/// # Safety
/// Using this logger is only safe if there is only one thread of execution.
/// Even though `UartLogger` is `Send` and `Sync`, The underlying `Uart` is not `Send` or `Sync`.
pub static mut LOGGER: UartLogger = UartLogger {
    uart: None,
    display_level: LevelFilter::Trace,
    display_source: LevelFilter::Trace,
};

/// Wrapper for `Uart` to be used as a logger with the `log` crate
/// Instead of making a new logger, use the `set_logger` method of the `LOGGER` instance.
/// # Safety
/// Using this logger is only safe if there is only one thread of execution.
/// Even though `UartLogger` is `Send` and `Sync`, The underlying `Uart` is not `Send` or `Sync`.
pub struct UartLogger {
    uart: Option<uart::Uart>,
    pub display_level: LevelFilter,
    pub display_source: LevelFilter,
}

impl UartLogger {
    /// Set the logger to use the given UART.
    /// # Safety
    /// Using this function and logger is only safe if there is only one thread of execution.
    /// This function is used to assign the `Uart` instance to a global (`static mut`), but `Uart` is not `Send` or `Sync`.
    pub unsafe fn set_logger(&mut self, uart: uart::Uart) {
        self.uart = Some(uart);
    }
}

impl log::Log for UartLogger {
    fn enabled(&self, metadata: &log::Metadata) -> bool {
        log::Level::Info <= metadata.level()
    }

    fn log(&self, record: &log::Record) {
        if self.enabled(record.metadata()) {
            unsafe {
                match &mut LOGGER.uart {
                    Some(l) => {
                        if record.level() <= self.display_level {
                            write!(l, "{} | ", record.level()).unwrap()
                        }
                        if record.level() <= self.display_source {
                            write!(
                                l,
                                "{}:{} - ",
                                record.file().unwrap(),
                                record.line().unwrap()
                            )
                            .unwrap();
                        }
                        writeln!(l, "{}", record.args()).unwrap();
                    }
                    None => panic!("Logger not set"),
                }
            }
        }
    }

    fn flush(&self) {}
}

unsafe impl core::marker::Send for UartLogger {}
unsafe impl core::marker::Sync for UartLogger {}
