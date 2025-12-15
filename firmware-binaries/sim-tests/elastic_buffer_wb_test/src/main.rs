// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
#![no_std]
#![cfg_attr(not(test), no_main)]

use bittide_hal::elastic_buffer_wb_test::DeviceInstances;
use bittide_hal::manual_additions::timer::Duration;
use bittide_hal::shared_devices::ElasticBuffer;
use bittide_hal::types::EbCommand;
#[cfg(not(test))]
use riscv_rt::entry;
use ufmt::uwriteln;

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let mut uart = INSTANCES.uart;
    let elastic_buffer = INSTANCES.elastic_buffer;
    let timer = INSTANCES.timer;

    let mut all_passed = true;

    // ========================================================================
    // Test 1: Set occupancy to midpoint (0)
    // Verifies that set_occupancy() correctly adjusts the buffer
    // to the target occupancy level (midpoint = 0) from any initial state.
    // ========================================================================
    {
        uwriteln!(uart, "Test 1: Set occupancy to midpoint (0)").unwrap();

        // Preconditions: Verify flags are clear
        let underflow_initial = elastic_buffer.underflow();
        let overflow_initial = elastic_buffer.overflow();
        if underflow_initial {
            all_passed = false;
            uwriteln!(uart, "  FAIL: Underflow flag should be clear").unwrap();
        }
        if overflow_initial {
            all_passed = false;
            uwriteln!(uart, "  FAIL: Overflow flag should be clear").unwrap();
        }

        // Action: Calculate how many elements to add/remove to reach 0
        let count_before = elastic_buffer.data_count();
        let target = 0i8;
        let delta = target - count_before;

        uwriteln!(
            uart,
            "  Before: count={}, target={}, delta={}",
            count_before,
            target,
            delta
        )
        .unwrap();

        // Use set_occupancy HAL function to reach the target
        elastic_buffer.set_occupancy(target);

        let count_after = elastic_buffer.data_count();
        uwriteln!(
            uart,
            "  After set_occupancy({}): count={}",
            target,
            count_after
        )
        .unwrap();

        // Verify: Count is exactly at the midpoint (0)
        if !underflow_initial && !overflow_initial && count_after == target {
            uwriteln!(uart, "  PASS").unwrap();
        } else {
            all_passed = false;
            uwriteln!(
                uart,
                "  FAIL: Expected count={}, got count={}",
                target,
                count_after
            )
            .unwrap();
        }
    }

    // ========================================================================
    // Test 2: Multiple Drain commands accumulate correctly
    // Verifies that decrease_occupancy() correctly decrements
    // the buffer occupancy by the specified number of frames.
    // ========================================================================
    {
        uwriteln!(uart, "Test 2: Multiple Drain commands accumulate").unwrap();

        // Preconditions: Verify flags are still clear
        let underflow_initial = elastic_buffer.underflow();
        let overflow_initial = elastic_buffer.overflow();
        if underflow_initial {
            all_passed = false;
            uwriteln!(uart, "  FAIL: Underflow flag should be clear").unwrap();
        }
        if overflow_initial {
            all_passed = false;
            uwriteln!(uart, "  FAIL: Overflow flag should be clear").unwrap();
        }

        // Action: Use decrease_occupancy HAL function to remove 5 frames
        let count_before = elastic_buffer.data_count() as i32;
        uwriteln!(
            uart,
            "  Before decrease_occupancy(5): count={}",
            count_before
        )
        .unwrap();

        elastic_buffer.decrease_occupancy(5);

        let count_after = elastic_buffer.data_count() as i32;
        uwriteln!(uart, "  After decrease_occupancy(5): count={}", count_after).unwrap();

        // Verify: Count decreased by exactly 5
        let expected = count_before - 5;
        if !underflow_initial && !overflow_initial && count_after == expected {
            uwriteln!(uart, "  PASS").unwrap();
        } else {
            all_passed = false;
            uwriteln!(
                uart,
                "  FAIL: Expected count={}, got count={}",
                expected,
                count_after
            )
            .unwrap();
        }
    }

    // ========================================================================
    // Test 3: Underflow flag triggers and is sticky
    // Description: Verifies that the underflow flag is set when draining past
    // minimum occupancy, remains set (sticky) after subsequent operations,
    // and can be cleared by writing to the status register.
    // ========================================================================
    {
        uwriteln!(uart, "Test 3: Underflow flag triggers and is sticky").unwrap();

        // Preconditions: Verify underflow flag is clear
        let underflow_initial = elastic_buffer.underflow();
        let overflow_initial = elastic_buffer.overflow();
        if underflow_initial {
            all_passed = false;
            uwriteln!(uart, "  FAIL: Underflow flag should be clear before test").unwrap();
        }
        if overflow_initial {
            all_passed = false;
            uwriteln!(uart, "  FAIL: Overflow flag should be clear").unwrap();
        }

        // Action 1: Drain past empty to trigger underflow
        let count_before = elastic_buffer.data_count();
        let drains_needed = count_before as i32 - (ElasticBuffer::MIN_OCCUPANCY as i32 - 1);
        uwriteln!(
            uart,
            "  Before Drain: count={}, will drain {} times",
            count_before,
            drains_needed
        )
        .unwrap();

        elastic_buffer.decrease_occupancy(drains_needed as u32);

        let count_after_drain = elastic_buffer.data_count();
        let underflow_after_drain = elastic_buffer.underflow();
        uwriteln!(
            uart,
            "  After Drain: count={}, underflow={}",
            count_after_drain,
            underflow_after_drain
        )
        .unwrap();

        // Verify 1: Underflow flag is set
        if !underflow_after_drain {
            all_passed = false;
            uwriteln!(
                uart,
                "  FAIL: Underflow flag should be set after draining past empty"
            )
            .unwrap();
        }

        // Action 2: Issue Fill command to test stickiness
        elastic_buffer.set_command(EbCommand::Fill);
        timer.wait(Duration::from_micros(1));

        let underflow_after_fill = elastic_buffer.underflow();
        let count_after_fill = elastic_buffer.data_count();
        uwriteln!(
            uart,
            "  After Fill: count={}, underflow={}",
            count_after_fill,
            underflow_after_fill
        )
        .unwrap();

        // Verify 2: Underflow flag remains set (sticky)
        if !underflow_after_fill {
            all_passed = false;
            uwriteln!(uart, "  FAIL: Underflow flag should remain set (sticky)").unwrap();
        }
        // Verify 3: Overflow flag is still clear
        if elastic_buffer.overflow() {
            all_passed = false;
            uwriteln!(uart, "  FAIL: Overflow flag should be clear").unwrap();
        }

        // Action 3: Clear underflow flag using the HAL convenience function
        elastic_buffer.clear_underflow();
        timer.wait(Duration::from_micros(1));

        let underflow_after_clear = elastic_buffer.underflow();
        uwriteln!(uart, "  After Clear: underflow={}", underflow_after_clear).unwrap();

        // Verify 4: Underflow flag is cleared by write
        if underflow_after_clear {
            all_passed = false;
            uwriteln!(uart, "  FAIL: Underflow flag should be cleared by write").unwrap();
        }

        if !underflow_initial
            && !overflow_initial
            && underflow_after_drain
            && underflow_after_fill
            && !underflow_after_clear
        {
            uwriteln!(uart, "  PASS").unwrap();
        }
    }

    // ========================================================================
    // Test 4: Overflow flag triggers and is sticky
    // Description: Verifies that the overflow flag is set when filling past
    // maximum occupancy, remains set (sticky) after subsequent operations,
    // and can be cleared by writing to the status register.
    // ========================================================================
    {
        uwriteln!(uart, "Test 4: Overflow flag triggers and is sticky").unwrap();

        // Preconditions: Verify overflow flag is clear
        let underflow_initial = elastic_buffer.underflow();
        let overflow_initial = elastic_buffer.overflow();

        if underflow_initial {
            all_passed = false;
            uwriteln!(uart, "  FAIL: Underflow flag should be clear").unwrap();
        }

        if overflow_initial {
            all_passed = false;
            uwriteln!(uart, "  FAIL: Overflow flag should be clear before test").unwrap();
        }

        // Action 1: Fill past capacity to trigger overflow
        let count_before = elastic_buffer.data_count() as i32;
        let fills_needed = ElasticBuffer::MAX_OCCUPANCY as i32 - count_before + 1;
        uwriteln!(
            uart,
            "  Before Fill: count={}, will fill {} times",
            count_before,
            fills_needed
        )
        .unwrap();

        elastic_buffer.increase_occupancy(fills_needed as u32);

        let count_after_fill = elastic_buffer.data_count() as i32;
        let overflow_after_fill = elastic_buffer.overflow();
        uwriteln!(
            uart,
            "  After Fill: count={}, overflow={}",
            count_after_fill,
            overflow_after_fill
        )
        .unwrap();

        // Verify 1: Overflow flag is set
        if !overflow_after_fill {
            all_passed = false;
            uwriteln!(
                uart,
                "  FAIL: Overflow flag should be set after filling past capacity"
            )
            .unwrap();
        }

        // Action 2: Issue Drain command to test stickiness
        elastic_buffer.set_command(EbCommand::Drain);
        timer.wait(Duration::from_micros(1));

        let overflow_after_drain = elastic_buffer.overflow();
        uwriteln!(uart, "  After Drain: overflow={}", overflow_after_drain).unwrap();

        // Verify 2: Overflow flag remains set (sticky)
        if !overflow_after_drain {
            all_passed = false;
            uwriteln!(uart, "  FAIL: Overflow flag should remain set (sticky)").unwrap();
        }

        // Action 3: Clear overflow flag using the HAL convenience function
        elastic_buffer.clear_overflow();
        timer.wait(Duration::from_micros(1));

        let overflow_after_clear = elastic_buffer.overflow();
        uwriteln!(uart, "  After Clear: overflow={}", overflow_after_clear).unwrap();

        // Verify 3: Overflow flag is cleared by write
        if overflow_after_clear {
            all_passed = false;
            uwriteln!(uart, "  FAIL: Overflow flag should be cleared by write").unwrap();
        }

        if !underflow_initial
            && !overflow_initial
            && overflow_after_fill
            && overflow_after_drain
            && !overflow_after_clear
        {
            uwriteln!(uart, "  PASS").unwrap();
        }
    }

    if all_passed {
        uwriteln!(uart, "All elastic buffer tests passed").unwrap();
    } else {
        uwriteln!(uart, "Some elastic buffer tests failed").unwrap();
    }

    loop {
        continue;
    }
}

#[panic_handler]
fn panic_handler(_info: &core::panic::PanicInfo) -> ! {
    loop {
        continue;
    }
}
