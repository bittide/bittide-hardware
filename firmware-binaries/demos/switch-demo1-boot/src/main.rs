#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use core::panic::PanicInfo;
use ufmt::uwriteln;

use bittide_hal::manual_additions::si539x_spi::{Config, WriteError};
use bittide_hal::shared_devices::transceivers::Transceivers;
use bittide_hal::switch_demo_boot::DeviceInstances;
use bittide_macros::load_clock_config_csv;

#[cfg(not(test))]
use riscv_rt::entry;

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };
const CONFIG_200: Config<3, 590, 5> = load_clock_config_csv!(
    "../../../bittide/data/clock_configs/Si5395J-200MHz-10ppb-Registers.csv"
);

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let si539x_spi = INSTANCES.si539x_spi;
    let timer = INSTANCES.timer;
    let transceivers = INSTANCES.transceivers;
    let mut uart = INSTANCES.uart;

    uwriteln!(uart, "Writing Si539x configuration..").unwrap();
    match si539x_spi.write_configuration(&timer, &CONFIG_200) {
        Ok(()) => {
            uwriteln!(uart, "Done.").unwrap();
        }
        Err(WriteError::NotConfirmed { entry, read_data }) => {
            uwriteln!(uart, "ERROR: failed to write Si539x configuration:").unwrap();
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
    uwriteln!(uart, "Done.").unwrap();

    uwriteln!(uart, "Enabling all transceiver channels..").unwrap();
    for channel in 0..Transceivers::CHANNEL_ENABLES_LEN {
        transceivers.set_channel_enables(channel, true);
    }
    uwriteln!(uart, "Done.").unwrap();

    uwriteln!(uart, "Going into infinite loop..").unwrap();
    #[allow(clippy::empty_loop)]
    loop {}
}

#[panic_handler]
fn panic_handler(_info: &PanicInfo) -> ! {
    #[allow(clippy::empty_loop)]
    loop {}
}
