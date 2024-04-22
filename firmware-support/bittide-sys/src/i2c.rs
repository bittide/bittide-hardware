// SPDX-FileCopyrightText: 2023-2024 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use ufmt::derive::uDebug;
#[derive(Copy, Clone, uDebug)]
pub struct I2CFlags {
    pub bus_busy: bool,
    pub arbitration_lost: bool,
    pub acknowledge_incoming: bool,
    pub transaction_acknowledged: bool,
    pub bus_claimed: bool,
    pub statemachine_reset: bool,
}

#[derive(uDebug)]
pub enum I2CError {
    ArbitrationLost,
    BusClaimedByOther,
    NotAcknowledged,
}

/// `I2C` is a structure representing a universal asynchronous receiver-transmitter.
#[derive(uDebug)]
pub struct I2C {
    /// `payload_addr` is a mutable pointer to the address of the data payload.
    payload_addr: *mut u8,
    /// `flags_addr` is a constant pointer to the address of the flags.
    flags_addr: *mut u8,
    /// `clk_div_addr` is a constant pointer to the address of the clock divider.
    clk_div_addr: *mut u16,
}

impl I2C {
    /// Create a new [`I2C`] instance given a base address.
    ///
    /// # Safety
    ///
    /// The `base_addr` pointer MUST BE a valid pointer that is backed
    /// by either a memory mapped UART instance or at valid read-writable memory
    /// (which will likely cause incorrect behaviour, but not break memory safety)
    pub const unsafe fn new(base_addr: *mut u8) -> I2C {
        I2C {
            payload_addr: base_addr,
            clk_div_addr: base_addr.add(4).cast(),
            flags_addr: base_addr.add(8).cast(),
        }
    }

    pub fn init(&mut self) {
        let flags = I2CFlags {
            bus_busy: false,
            arbitration_lost: false,
            acknowledge_incoming: false,
            transaction_acknowledged: false,
            bus_claimed: false,
            statemachine_reset: false,
        };
        self.write_flags(flags);
    }

    pub fn claim_bus(&mut self) -> Result<(), I2CError> {
        let mut flags = self.read_flags();
        if flags.bus_busy && !flags.bus_claimed {
            Err(I2CError::BusClaimedByOther)
        } else {
            flags.bus_claimed = true;
            self.write_flags(flags);
            Ok(())
        }
    }

    pub fn claim_bus_with_retry(&mut self) -> Result<(), I2CError> {
        const MAX_RETRIES: usize = 10;
        for _ in 0..MAX_RETRIES {
            let mut flags = self.read_flags();
            if !flags.bus_busy || flags.bus_claimed {
                flags.bus_claimed = true;
                self.write_flags(flags);
                return Ok(());
            }
        }
        // If all retries failed, return an error
        Err(I2CError::BusClaimedByOther)
    }

    pub fn release_bus(&mut self) {
        let mut flags = self.read_flags();
        flags.bus_claimed = false;
        flags.arbitration_lost = false;
        self.write_flags(flags);
    }

    pub fn read_byte(&mut self) -> Result<u8, I2CError> {
        let flags = self.read_flags();
        let already_claimed = flags.bus_claimed;
        if !already_claimed {
            self.claim_bus_with_retry()?;
        };
        let data = unsafe { self.payload_addr.read_volatile() };
        let old_flags = self.read_flags();
        let mut new_flags = old_flags;
        if !already_claimed {
            new_flags.bus_claimed = false
        };
        new_flags.arbitration_lost = false;
        new_flags.transaction_acknowledged = false;
        self.write_flags(new_flags);
        if old_flags.arbitration_lost {
            Err(I2CError::ArbitrationLost)
        } else {
            Ok(data)
        }
    }

    pub fn write_byte(&mut self, data: u8) -> Result<(), I2CError> {
        let flags = self.read_flags();
        let already_claimed = flags.bus_claimed;
        if !already_claimed {
            self.claim_bus_with_retry()?;
        };
        unsafe { self.payload_addr.write_volatile(data) };
        let old_flags = self.read_flags();
        let mut new_flags = old_flags;
        if !already_claimed {
            new_flags.bus_claimed = false
        };
        new_flags.arbitration_lost = false;
        new_flags.transaction_acknowledged = false;
        self.write_flags(new_flags);
        if old_flags.arbitration_lost {
            Err(I2CError::ArbitrationLost)
        } else if !old_flags.transaction_acknowledged {
            Err(I2CError::NotAcknowledged)
        } else {
            Ok(())
        }
    }

    ///  I2C status register output
    pub fn read_flags(&mut self) -> I2CFlags {
        let flags = unsafe { self.flags_addr.read_volatile() } as u32;

        let bus_busy = check_bit(flags, 5);
        let arbitration_lost = check_bit(flags, 4);
        let transaction_acknowledged = check_bit(flags, 3);
        let acknowledge_incoming = check_bit(flags, 2);
        let bus_claimed = check_bit(flags, 1);
        let statemachine_reset = check_bit(flags, 0);

        I2CFlags {
            bus_busy,
            arbitration_lost,
            transaction_acknowledged,
            acknowledge_incoming,
            bus_claimed,
            statemachine_reset,
        }
    }

    /// UART status register output
    pub fn write_flags(&mut self, status: I2CFlags) {
        let mut flags: u8 = 0;

        if status.arbitration_lost {
            flags |= 0b010000;
        }
        if status.transaction_acknowledged {
            flags |= 0b001000;
        }
        if status.acknowledge_incoming {
            flags |= 0b000100;
        }
        if status.bus_claimed {
            flags |= 0b000010;
        }
        if status.statemachine_reset {
            flags |= 0b000001;
        }

        unsafe { self.flags_addr.write_volatile(flags) };
    }

    pub fn get_clock_divider(&mut self) -> u16 {
        unsafe { self.clk_div_addr.read_volatile() }
    }

    pub fn set_clock_divider(&mut self, clk_div: u16) {
        unsafe { self.clk_div_addr.write_volatile(clk_div) }
    }
}

fn check_bit(a: u32, index: u32) -> bool {
    let mask = 1 << index;
    a & mask == mask
}
