// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use bittide_hal::manual_additions::si539x_spi::{Config, WriteError};
use bittide_hal::shared_devices::{Si539xSpi, Timer, Transceivers, Uart};
use ufmt::uwriteln;

pub fn run(
    si539x_spi: Si539xSpi,
    timer: Timer,
    transceivers: Transceivers,
    uart: &mut Uart,
    config: &Config<3, 590, 5>,
) -> ! {
    uwriteln!(uart, "Writing Si539x configuration..").unwrap();
    match si539x_spi.write_configuration(&timer, config) {
        Ok(()) => {
            uwriteln!(uart, "Done.").unwrap();
        }
        Err(WriteError::NotConfirmed { entry, read_data }) => {
            uwriteln!(uart, "[ERROR] failed to write Si539x configuration:").unwrap();
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
