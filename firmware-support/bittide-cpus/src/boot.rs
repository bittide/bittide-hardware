// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use bittide_hal::manual_additions::clock_chip::{ClockChip, Config, WriteError};
use bittide_hal::shared_devices::{I2cController, Timer, Transceivers, Uart};
use ufmt::derive::uDebug;
use ufmt::uwriteln;

use crate::fmc_power::{set_vadj_1v8, VadjError};

/// Writing a configuration can sometimes fail. The 'program_fmc_clock' function therefore
/// tries up to 'MAX_RETRIES' times.
const MAX_RETRIES: usize = 10;

/// Enable the transceiver block and all of its channels.
pub fn enable_transceivers(transceivers: &Transceivers, uart: &mut Uart) {
    uwriteln!(uart, "Enabling bittide domain..").unwrap();
    transceivers.set_transceiver_enable(true);

    uwriteln!(uart, "Enabling all transceiver channels..").unwrap();
    for channel in 0..Transceivers::CHANNEL_ENABLES_LEN {
        transceivers.set_channel_enables(channel, true);
    }
}

/// Start the clock chip on the FMC card with [`start_fmc_clock`], with up to
/// 'MAX_RETRIES' attempts to write the given configuration.
///
/// Panics when starting the clock chip fails.
pub fn program_fmc_clock<
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
) {
    if let Err(e) = start_fmc_clock(main_i2c, clock_chip, timer, uart, config, MAX_RETRIES) {
        uwriteln!(uart, "[ERROR] failed to start the clock chip: {:?}", e).unwrap();
        panic!("[ERROR] failed to start the clock chip, panicking");
    }
}

/// Run the steps of a boot CPU with a clock chip on the FMC card.
///
/// Panics when starting the clock chip fails, see [`program_fmc_clock`].
pub fn run<C: ClockChip, const PRE_LEN: usize, const CFG_LEN: usize, const PST_LEN: usize>(
    main_i2c: I2cController,
    clock_chip: C,
    timer: Timer,
    transceivers: Transceivers,
    uart: &mut Uart,
    config: &Config<PRE_LEN, CFG_LEN, PST_LEN>,
) -> ! {
    program_fmc_clock(&main_i2c, &clock_chip, &timer, uart, config);
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
