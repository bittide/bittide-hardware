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

/// Test 1: Set occupancy to midpoint (0)
/// Verifies that set_occupancy() correctly adjusts the buffer
/// to the target occupancy level (midpoint = 0) from any initial state.
fn test_set_occupancy_to_midpoint(
    uart: &mut bittide_hal::shared_devices::Uart,
    elastic_buffer: &bittide_hal::shared_devices::ElasticBuffer,
) -> bool {
    uwriteln!(uart, "Test 1: Set occupancy to midpoint (0)").unwrap();

    // Preconditions: Verify flags are clear
    let underflow_initial = elastic_buffer.underflow();
    let overflow_initial = elastic_buffer.overflow();
    if underflow_initial {
        uwriteln!(uart, "  FAIL: Underflow flag should be clear").unwrap();
        return false;
    }
    if overflow_initial {
        uwriteln!(uart, "  FAIL: Overflow flag should be clear").unwrap();
        return false;
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
        true
    } else {
        uwriteln!(
            uart,
            "  FAIL: Expected count={}, got count={}",
            target,
            count_after
        )
        .unwrap();
        false
    }
}

/// Test 2: Multiple Drain commands accumulate correctly
/// Verifies that decrease_occupancy() correctly decrements
/// the buffer occupancy by the specified number of frames.
fn test_multiple_drain_commands(
    uart: &mut bittide_hal::shared_devices::Uart,
    elastic_buffer: &bittide_hal::shared_devices::ElasticBuffer,
) -> bool {
    uwriteln!(uart, "Test 2: Multiple Drain commands accumulate").unwrap();

    // Preconditions: Verify flags are still clear
    let underflow_initial = elastic_buffer.underflow();
    let overflow_initial = elastic_buffer.overflow();
    if underflow_initial {
        uwriteln!(uart, "  FAIL: Underflow flag should be clear").unwrap();
        return false;
    }
    if overflow_initial {
        uwriteln!(uart, "  FAIL: Overflow flag should be clear").unwrap();
        return false;
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
        true
    } else {
        uwriteln!(
            uart,
            "  FAIL: Expected count={}, got count={}",
            expected,
            count_after
        )
        .unwrap();
        false
    }
}

/// Test 3: Underflow flag triggers and is sticky
/// Verifies that the underflow flag is set when draining past
/// minimum occupancy, remains set (sticky) after subsequent operations,
/// and can be cleared by writing to the status register.
fn test_underflow_flag_sticky(
    uart: &mut bittide_hal::shared_devices::Uart,
    elastic_buffer: &bittide_hal::shared_devices::ElasticBuffer,
    timer: &bittide_hal::shared_devices::Timer,
) -> bool {
    uwriteln!(uart, "Test 3: Underflow flag triggers and is sticky").unwrap();

    // Preconditions: Verify underflow flag is clear
    let underflow_initial = elastic_buffer.underflow();
    let overflow_initial = elastic_buffer.overflow();
    if underflow_initial {
        uwriteln!(uart, "  FAIL: Underflow flag should be clear before test").unwrap();
        return false;
    }
    if overflow_initial {
        uwriteln!(uart, "  FAIL: Overflow flag should be clear").unwrap();
        return false;
    }

    // Action 1: Drain past empty to trigger underflow
    let count_before = elastic_buffer.data_count();
    let drains_needed = count_before as i32 - (ElasticBuffer::MIN_OCCUPANCY as i32 - 1);
    uwriteln!(
        uart,
        "  Before Drain: count={}, will drain {} times",
        count_before as i8,
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
        uwriteln!(
            uart,
            "  FAIL: Underflow flag should be set after draining past empty"
        )
        .unwrap();
        return false;
    }

    // Action 2: Issue Fill command to test stickiness
    elastic_buffer.set_command(EbCommand::Fill { n: 1 });
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
        uwriteln!(uart, "  FAIL: Underflow flag should remain set (sticky)").unwrap();
        return false;
    }
    // Verify 3: Overflow flag is still clear
    if elastic_buffer.overflow() {
        uwriteln!(uart, "  FAIL: Overflow flag should be clear").unwrap();
        return false;
    }

    // Action 3: Clear underflow flag using the HAL convenience function
    elastic_buffer.clear_underflow();
    timer.wait(Duration::from_micros(1));

    let underflow_after_clear = elastic_buffer.underflow();
    uwriteln!(uart, "  After Clear: underflow={}", underflow_after_clear).unwrap();

    // Verify 4: Underflow flag is cleared by write
    if underflow_after_clear {
        uwriteln!(uart, "  FAIL: Underflow flag should be cleared by write").unwrap();
        return false;
    }

    if !underflow_initial
        && !overflow_initial
        && underflow_after_drain
        && underflow_after_fill
        && !underflow_after_clear
    {
        uwriteln!(uart, "  PASS").unwrap();
        true
    } else {
        false
    }
}

/// Test 4: Overflow flag triggers and is sticky
/// Verifies that the overflow flag is set when filling past
/// maximum occupancy, remains set (sticky) after subsequent operations,
/// and can be cleared by writing to the status register.
fn test_overflow_flag_sticky(
    uart: &mut bittide_hal::shared_devices::Uart,
    elastic_buffer: &bittide_hal::shared_devices::ElasticBuffer,
    timer: &bittide_hal::shared_devices::Timer,
) -> bool {
    uwriteln!(uart, "Test 4: Overflow flag triggers and is sticky").unwrap();

    // Preconditions: Verify overflow flag is clear
    let underflow_initial = elastic_buffer.underflow();
    let overflow_initial = elastic_buffer.overflow();

    if underflow_initial {
        uwriteln!(uart, "  FAIL: Underflow flag should be clear").unwrap();
        return false;
    }

    if overflow_initial {
        uwriteln!(uart, "  FAIL: Overflow flag should be clear before test").unwrap();
        return false;
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
        uwriteln!(
            uart,
            "  FAIL: Overflow flag should be set after filling past capacity"
        )
        .unwrap();
        return false;
    }

    // Action 2: Issue Drain command to test stickiness
    elastic_buffer.set_command(EbCommand::Drain { n: 1 });
    timer.wait(Duration::from_micros(1));

    let overflow_after_drain = elastic_buffer.overflow();
    uwriteln!(uart, "  After Drain: overflow={}", overflow_after_drain).unwrap();

    // Verify 2: Overflow flag remains set (sticky)
    if !overflow_after_drain {
        uwriteln!(uart, "  FAIL: Overflow flag should remain set (sticky)").unwrap();
        return false;
    }

    // Action 3: Clear overflow flag using the HAL convenience function
    elastic_buffer.clear_overflow();
    timer.wait(Duration::from_micros(1));

    let overflow_after_clear = elastic_buffer.overflow();
    uwriteln!(uart, "  After Clear: overflow={}", overflow_after_clear).unwrap();

    // Verify 3: Overflow flag is cleared by write
    if overflow_after_clear {
        uwriteln!(uart, "  FAIL: Overflow flag should be cleared by write").unwrap();
        return false;
    }

    if !underflow_initial
        && !overflow_initial
        && overflow_after_fill
        && overflow_after_drain
        && !overflow_after_clear
    {
        uwriteln!(uart, "  PASS").unwrap();
        true
    } else {
        false
    }
}

/// Test 5: Drain/Fill multiple items using command field
/// Verifies that the `n` parameter in EbCommand correctly
/// drains or fills multiple items in a single command.
fn test_multiple_items_command(
    uart: &mut bittide_hal::shared_devices::Uart,
    elastic_buffer: &bittide_hal::shared_devices::ElasticBuffer,
    timer: &bittide_hal::shared_devices::Timer,
) -> bool {
    uwriteln!(uart, "Test 5: Drain/Fill multiple items using command").unwrap();

    // Set buffer to a known state (midpoint)
    elastic_buffer.set_occupancy(0);
    timer.wait(Duration::from_micros(1));

    let count_before = elastic_buffer.data_count();
    uwriteln!(uart, "  Initial count: {}", count_before).unwrap();

    // Test 1: Fill multiple items (n=10)
    elastic_buffer.set_command(EbCommand::Fill { n: 10 });
    timer.wait(Duration::from_micros(1));

    let count_after_fill = elastic_buffer.data_count();
    uwriteln!(uart, "  After Fill{{n:10}}: count={}", count_after_fill).unwrap();

    if count_after_fill != count_before + 10 {
        uwriteln!(
            uart,
            "  FAIL: Expected count={}, got count={}",
            count_before + 10,
            count_after_fill
        )
        .unwrap();
        return false;
    }

    // Test 2: Drain multiple items (n=5)
    elastic_buffer.set_command(EbCommand::Drain { n: 5 });
    timer.wait(Duration::from_micros(1));

    let count_after_drain = elastic_buffer.data_count();
    uwriteln!(uart, "  After Drain{{n:5}}: count={}", count_after_drain).unwrap();

    if count_after_drain != count_after_fill - 5 {
        uwriteln!(
            uart,
            "  FAIL: Expected count={}, got count={}",
            count_after_fill - 5,
            count_after_drain
        )
        .unwrap();
        return false;
    }

    uwriteln!(uart, "  PASS").unwrap();
    true
}

/// Test 6: prepare -> go -> prepare -> go sequence
/// Verifies that we can reliably execute two commands back-to-back
/// without an explicit wait between them.
fn test_prepare_go_prepare_go(
    uart: &mut bittide_hal::shared_devices::Uart,
    elastic_buffer: &bittide_hal::shared_devices::ElasticBuffer,
    timer: &bittide_hal::shared_devices::Timer,
) -> bool {
    uwriteln!(uart, "Test 6: prepare -> go -> prepare -> go").unwrap();

    // Set buffer to a known state
    elastic_buffer.set_occupancy(0);
    timer.wait(Duration::from_micros(1));

    let count_initial = elastic_buffer.data_count();
    uwriteln!(uart, "  Initial count: {}", count_initial).unwrap();

    // First command: prepare Fill(3) and go
    elastic_buffer.set_command_prepare(EbCommand::Fill { n: 3 });
    elastic_buffer.set_command_go(());
    timer.wait(Duration::from_micros(1));

    let count_after_first = elastic_buffer.data_count();
    uwriteln!(
        uart,
        "  After first Fill{{n:3}}: count={}",
        count_after_first
    )
    .unwrap();

    // Second command: prepare Drain(2) and go (without explicit wait)
    elastic_buffer.set_command_prepare(EbCommand::Drain { n: 2 });
    elastic_buffer.set_command_go(());
    timer.wait(Duration::from_micros(1));

    let count_after_second = elastic_buffer.data_count();
    uwriteln!(
        uart,
        "  After second Drain{{n:2}}: count={}",
        count_after_second
    )
    .unwrap();

    // Verify: count should be initial + 3 - 2 = initial + 1
    let expected = count_initial + 1;
    if count_after_second == expected {
        uwriteln!(uart, "  PASS").unwrap();
        true
    } else {
        uwriteln!(
            uart,
            "  FAIL: Expected count={}, got count={}",
            expected,
            count_after_second
        )
        .unwrap();
        false
    }
}

/// Test 7: prepare -> go -> wait -> prepare -> go -> wait sequence
/// Verifies that explicit waits between commands work correctly.
fn test_prepare_go_wait_prepare_go_wait(
    uart: &mut bittide_hal::shared_devices::Uart,
    elastic_buffer: &bittide_hal::shared_devices::ElasticBuffer,
    timer: &bittide_hal::shared_devices::Timer,
) -> bool {
    uwriteln!(
        uart,
        "Test 7: prepare -> go -> wait -> prepare -> go -> wait"
    )
    .unwrap();

    // Set buffer to a known state
    elastic_buffer.set_occupancy(0);
    timer.wait(Duration::from_micros(1));

    let count_initial = elastic_buffer.data_count();
    uwriteln!(uart, "  Initial count: {}", count_initial).unwrap();

    // First command: prepare, go, and wait
    elastic_buffer.set_command_prepare(EbCommand::Fill { n: 7 });
    elastic_buffer.set_command_go(());
    elastic_buffer.set_command_wait(());
    timer.wait(Duration::from_micros(1));

    let count_after_first = elastic_buffer.data_count();
    uwriteln!(
        uart,
        "  After first Fill{{n:7}} + wait: count={}",
        count_after_first
    )
    .unwrap();

    // Second command: prepare, go, and wait
    elastic_buffer.set_command_prepare(EbCommand::Drain { n: 4 });
    elastic_buffer.set_command_go(());
    elastic_buffer.set_command_wait(());
    timer.wait(Duration::from_micros(1));

    let count_after_second = elastic_buffer.data_count();
    uwriteln!(
        uart,
        "  After second Drain{{n:4}} + wait: count={}",
        count_after_second
    )
    .unwrap();

    // Verify: count should be initial + 7 - 4 = initial + 3
    let expected = count_initial + 3;
    if count_after_second == expected {
        uwriteln!(uart, "  PASS").unwrap();
        true
    } else {
        uwriteln!(
            uart,
            "  FAIL: Expected count={}, got count={}",
            expected,
            count_after_second
        )
        .unwrap();
        false
    }
}

/// Test 8: prepare -> go -> prepare -> wait -> go -> wait sequence
/// Verifies that we can prepare a second command before waiting
/// on the first one to complete.
fn test_prepare_go_prepare_wait_go_wait(
    uart: &mut bittide_hal::shared_devices::Uart,
    elastic_buffer: &bittide_hal::shared_devices::ElasticBuffer,
    timer: &bittide_hal::shared_devices::Timer,
) -> bool {
    uwriteln!(
        uart,
        "Test 8: prepare -> go -> prepare -> wait -> go -> wait"
    )
    .unwrap();

    // Set buffer to a known state
    elastic_buffer.set_occupancy(0);
    timer.wait(Duration::from_micros(1));

    let count_initial = elastic_buffer.data_count();
    uwriteln!(uart, "  Initial count: {}", count_initial).unwrap();

    // First command: prepare and go (but don't wait yet)
    elastic_buffer.set_command_prepare(EbCommand::Fill { n: 6 });
    elastic_buffer.set_command_go(());

    // Prepare second command before waiting on first
    elastic_buffer.set_command_prepare(EbCommand::Drain { n: 3 });

    // Now wait for first command to complete
    elastic_buffer.set_command_wait(());
    timer.wait(Duration::from_micros(1));

    let count_after_first = elastic_buffer.data_count();
    uwriteln!(
        uart,
        "  After Fill{{n:6}} with interleaved prepare: count={}",
        count_after_first
    )
    .unwrap();

    // Execute second command: go and wait
    elastic_buffer.set_command_go(());
    elastic_buffer.set_command_wait(());
    timer.wait(Duration::from_micros(1));

    let count_after_second = elastic_buffer.data_count();
    uwriteln!(
        uart,
        "  After second Drain{{n:3}} + wait: count={}",
        count_after_second
    )
    .unwrap();

    // Verify: count should be initial + 6 - 3 = initial + 3
    let expected = count_initial + 3;
    if count_after_second == expected {
        uwriteln!(uart, "  PASS").unwrap();
        true
    } else {
        uwriteln!(
            uart,
            "  FAIL: Expected count={}, got count={}",
            expected,
            count_after_second
        )
        .unwrap();
        false
    }
}

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let mut uart = INSTANCES.uart;
    let elastic_buffer = INSTANCES.elastic_buffer;
    let timer = INSTANCES.timer;

    let mut all_passed = true;

    all_passed &= test_set_occupancy_to_midpoint(&mut uart, &elastic_buffer);
    all_passed &= test_multiple_drain_commands(&mut uart, &elastic_buffer);
    all_passed &= test_underflow_flag_sticky(&mut uart, &elastic_buffer, &timer);
    all_passed &= test_overflow_flag_sticky(&mut uart, &elastic_buffer, &timer);
    all_passed &= test_multiple_items_command(&mut uart, &elastic_buffer, &timer);
    all_passed &= test_prepare_go_prepare_go(&mut uart, &elastic_buffer, &timer);
    all_passed &= test_prepare_go_wait_prepare_go_wait(&mut uart, &elastic_buffer, &timer);
    all_passed &= test_prepare_go_prepare_wait_go_wait(&mut uart, &elastic_buffer, &timer);

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
