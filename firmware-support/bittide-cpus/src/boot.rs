// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use bittide_hal::manual_additions::si539x_spi::{Config, WriteError};
use bittide_hal::shared_devices::{Si539xSpi, Timer, Transceivers, Uart};
use ufmt::uwriteln;

/// Writing a configuration can sometimes fail, usually because a written register was not
/// the same when not read back. This happens roughly once in 200 runs. The 'run' function
/// therefore retries when this happens up to 'MAX_RETRIES' times.
const MAX_RETRIES: usize = 10;

/// Run the steps of a boot CPU.
///
/// Panics when writing a configuration fails 'MAX_RETRIES' times.
pub fn run(
    si539x_spi: Si539xSpi,
    timer: Timer,
    transceivers: Transceivers,
    uart: &mut Uart,
    config: &Config<3, 590, 5>,
) -> ! {
    uwriteln!(uart, "Writing Si539x configuration..").unwrap();
    let mut retries = 0;
    loop {
        match si539x_spi.write_configuration(&timer, config) {
            Ok(()) => {
                uwriteln!(uart, "Done.").unwrap();
                break;
            }
            Err(WriteError::NotConfirmed { entry, read_data }) => {
                retries += 1;
                if retries >= MAX_RETRIES {
                    panic!(
                        "[ERROR] failed to write Si539x configuration after {MAX_RETRIES} retries, panicking"
                    );
                }
                uwriteln!(
                    uart,
                    "[ERROR] failed to write Si539x configuration, retrying..."
                )
                .unwrap();
                uwriteln!(
                    uart,
                    "At 0x{:02X}{:02X} wrote 0x{:02X}, but read back 0x{:02X}",
                    entry.page,
                    entry.address,
                    entry.data,
                    read_data,
                )
                .unwrap();
            }
        }
    }

    uwriteln!(uart, "Enabling bittide domain..").unwrap();
    transceivers.set_transceiver_enable(true);

    uwriteln!(uart, "Enabling all transceiver channels..").unwrap();
    for channel in 0..Transceivers::CHANNEL_ENABLES_LEN {
        transceivers.set_channel_enables(channel, true);
    }

    uwriteln!(uart, "Going into infinite loop..").unwrap();
    loop {
        continue;
    }
}
