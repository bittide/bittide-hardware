// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use bittide_hal::manual_additions::clock_chip::{ClockChip, Config, WriteError};
use bittide_hal::shared_devices::{I2cController, Si539xSpi, Timer, Transceivers, Uart};
use ufmt::derive::uDebug;
use ufmt::uwriteln;

use crate::fmc_power::{set_vadj_1v8, VadjError};

/// Writing a configuration can sometimes fail, usually because a written register was not
/// the same when not read back. This happens roughly once in 200 runs. The
/// 'program_clock_board' function therefore retries when this happens up to 'MAX_RETRIES'
/// times.
const MAX_RETRIES: usize = 10;

/// Write the given configuration to the Si539x clock board.
///
/// Panics when writing a configuration fails 'MAX_RETRIES' times.
pub fn program_clock_board(
    si539x_spi: &Si539xSpi,
    timer: &Timer,
    uart: &mut Uart,
    config: &Config<3, 590, 5>,
) {
    uwriteln!(uart, "Writing Si539x configuration..").unwrap();
    let mut retries = 0;
    loop {
        match si539x_spi.write_configuration(timer, config) {
            Ok(()) => {
                uwriteln!(uart, "Done.").unwrap();
                return;
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
}

/// Enable the transceiver block and all of its channels.
pub fn enable_transceivers(transceivers: &Transceivers, uart: &mut Uart) {
    uwriteln!(uart, "Enabling bittide domain..").unwrap();
    transceivers.set_transceiver_enable(true);

    uwriteln!(uart, "Enabling all transceiver channels..").unwrap();
    for channel in 0..Transceivers::CHANNEL_ENABLES_LEN {
        transceivers.set_channel_enables(channel, true);
    }
}

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
    program_clock_board(&si539x_spi, &timer, uart, config);
    enable_transceivers(&transceivers, uart);

    uwriteln!(uart, "Going into infinite loop..").unwrap();
    loop {
        continue;
    }
}

#[derive(uDebug, Copy, Clone)]
pub enum StartError {
    /// Setting VADJ failed. The clock chip was not touched.
    Vadj(VadjError),
    /// Writing the clock chip configuration failed, also after retrying.
    ClockChip(WriteError),
}

/// Start the clock chip on the FMC card, for designs with a `fmcBootPe`: set VADJ to
/// 1.8 V, then write the given configuration to the clock chip, with up to `max_retries`
/// attempts. VADJ powers the Si5345 and the I/O of the card through which we reach it, so
/// the order matters: without VADJ, waiting for the clock chip would hang.
pub fn start_fmc_clock<
    C: ClockChip,
    const PRE_LEN: usize,
    const CFG_LEN: usize,
    const PST_LEN: usize,
>(
    main_i2c: &I2cController,
    clock_chip: &C,
    timer: &Timer,
    uart: &mut Uart,
    config: &Config<PRE_LEN, CFG_LEN, PST_LEN>,
    max_retries: usize,
) -> Result<(), StartError> {
    let outcome = set_vadj_1v8(main_i2c, timer, uart).map_err(StartError::Vadj)?;
    uwriteln!(uart, "VADJ: {:?}", outcome).unwrap();

    uwriteln!(uart, "Writing clock chip configuration..").unwrap();
    clock_chip
        .write_configuration_with_retry(timer, config, max_retries)
        .map_err(StartError::ClockChip)?;
    uwriteln!(uart, "Done.").unwrap();
    Ok(())
}
