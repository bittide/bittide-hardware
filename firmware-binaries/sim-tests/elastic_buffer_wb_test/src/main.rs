// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
#![no_std]
#![cfg_attr(not(test), no_main)]

use bittide_hal::elastic_buffer_wb_test::DeviceInstances;
use bittide_hal::manual_additions::timer::Duration;
use bittide_hal::shared_devices::ElasticBuffer;
#[cfg(not(test))]
use riscv_rt::entry;
use ufmt::uwriteln;

const INSTANCES: DeviceInstances = unsafe { DeviceInstances::new() };

/// Verifies that set_occupancy() correctly adjusts the buffer to the target occupancy
/// level (midpoint = 0) from any initial state.
fn test_set_occupancy_to_midpoint(
    uart: &mut bittide_hal::shared_devices::Uart,
    elastic_buffer: &bittide_hal::shared_devices::ElasticBuffer,
) -> bool {
    uwriteln!(uart, "Test: Set occupancy to midpoint (0)").unwrap();

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

/// Verifies that an adjustment of zero is accepted and acknowledged without modifying the
/// buffer occupancy.
fn test_zero_adjustment(
    uart: &mut bittide_hal::shared_devices::Uart,
    elastic_buffer: &bittide_hal::shared_devices::ElasticBuffer,
    timer: &bittide_hal::shared_devices::Timer,
) -> bool {
    uwriteln!(uart, "Test: Zero adjustment (no-op)").unwrap();

    let count_before = elastic_buffer.data_count();
    uwriteln!(uart, "  Before adjustment(0): count={}", count_before).unwrap();

    // Test single zero adjustment
    elastic_buffer.set_adjustment(0);

    timer.wait(Duration::from_micros(1));
    let count_after_first = elastic_buffer.data_count();
    uwriteln!(uart, "  After adjustment(0): count={}", count_after_first).unwrap();

    if count_after_first != count_before {
        uwriteln!(
            uart,
            "  FAIL: Count should not change, was {}, now {}",
            count_before,
            count_after_first
        )
        .unwrap();
        return false;
    }

    // Test multiple zero adjustments in succession
    elastic_buffer.set_adjustment(0);
    elastic_buffer.set_adjustment(0);
    elastic_buffer.set_adjustment(0);

    timer.wait(Duration::from_micros(1));
    let count_after_multiple = elastic_buffer.data_count();

    uwriteln!(
        uart,
        "  After 3 more adjustment(0): count={}",
        count_after_multiple
    )
    .unwrap();

    if count_after_multiple != count_before {
        uwriteln!(
            uart,
            "  FAIL: Count should not change after multiple zeros, was {}, now {}",
            count_before,
            count_after_multiple
        )
        .unwrap();
        return false;
    }

    uwriteln!(uart, "  PASS").unwrap();
    true
}

/// Verifies that set_adjustment() with negative values correctly decrements the buffer
/// occupancy by the specified number of frames.
fn test_multiple_drain_commands(
    uart: &mut bittide_hal::shared_devices::Uart,
    elastic_buffer: &bittide_hal::shared_devices::ElasticBuffer,
) -> bool {
    uwriteln!(uart, "Test: Multiple Drain commands accumulate").unwrap();

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

    // Action: Use set_adjustment HAL function to remove 5 frames
    let count_before = elastic_buffer.data_count() as i32;
    uwriteln!(uart, "  Before set_adjustment(-5): count={}", count_before).unwrap();

    elastic_buffer.set_adjustment(-5);

    let count_after = elastic_buffer.data_count() as i32;
    uwriteln!(uart, "  After set_adjustment(-5): count={}", count_after).unwrap();

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

/// Verifies that the underflow flag is set when draining past minimum occupancy, remains
/// set (sticky) after subsequent operations, and can be cleared by writing to the status
/// register.
fn test_underflow_flag_sticky(
    uart: &mut bittide_hal::shared_devices::Uart,
    elastic_buffer: &bittide_hal::shared_devices::ElasticBuffer,
    timer: &bittide_hal::shared_devices::Timer,
) -> bool {
    uwriteln!(uart, "Test: Underflow flag triggers and is sticky").unwrap();

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
        "  Before draining: count={}, will drain {} times",
        count_before as i8,
        drains_needed
    )
    .unwrap();

    elastic_buffer.set_adjustment(-drains_needed);

    let count_after_drain = elastic_buffer.data_count();
    let underflow_after_drain = elastic_buffer.underflow();
    uwriteln!(
        uart,
        "  After draining: count={}, underflow={}",
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

    // Action 2: Issue Fill adjustment to test stickiness
    elastic_buffer.set_adjustment(1);
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
    elastic_buffer.set_clear_status_registers(true);

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

/// Verifies that the overflow flag is set when filling past maximum occupancy, remains
/// set (sticky) after subsequent operations, and can be cleared by writing to the status
/// register.
fn test_overflow_flag_sticky(
    uart: &mut bittide_hal::shared_devices::Uart,
    elastic_buffer: &bittide_hal::shared_devices::ElasticBuffer,
    timer: &bittide_hal::shared_devices::Timer,
) -> bool {
    uwriteln!(uart, "Test: Overflow flag triggers and is sticky").unwrap();

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
        "  Before filling: count={}, will fill {} times",
        count_before,
        fills_needed
    )
    .unwrap();

    elastic_buffer.set_adjustment(fills_needed);

    let count_after_fill = elastic_buffer.data_count() as i32;
    let overflow_after_fill = elastic_buffer.overflow();
    uwriteln!(
        uart,
        "  After filling: count={}, overflow={}",
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

    // Action 2: Issue Drain adjustment to test stickiness
    elastic_buffer.set_adjustment(-1);

    timer.wait(Duration::from_micros(1));
    let overflow_after_drain = elastic_buffer.overflow();
    uwriteln!(uart, "  After draining: overflow={}", overflow_after_drain).unwrap();

    // Verify 2: Overflow flag remains set (sticky)
    if !overflow_after_drain {
        uwriteln!(uart, "  FAIL: Overflow flag should remain set (sticky)").unwrap();
        return false;
    }

    // Action 3: Clear overflow flag using the HAL convenience function
    elastic_buffer.set_clear_status_registers(true);
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

/// Verifies that the `n` parameter in EbAdjustment correctly drains or fills multiple
/// items in a single command.
fn test_multiple_items_command(
    uart: &mut bittide_hal::shared_devices::Uart,
    elastic_buffer: &bittide_hal::shared_devices::ElasticBuffer,
    timer: &bittide_hal::shared_devices::Timer,
) -> bool {
    uwriteln!(uart, "Test: Drain/Fill multiple items using adjustment").unwrap();

    // Set buffer to a known state (midpoint)
    elastic_buffer.set_occupancy(0);

    timer.wait(Duration::from_micros(1));
    let count_before = elastic_buffer.data_count();
    uwriteln!(uart, "  Initial count: {}", count_before).unwrap();

    // Test 1: Fill multiple items (n=10)
    elastic_buffer.set_adjustment(10);

    timer.wait(Duration::from_micros(1));
    let count_after_fill = elastic_buffer.data_count();
    uwriteln!(uart, "  After filling 10: count={}", count_after_fill).unwrap();

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
    elastic_buffer.set_adjustment(-5);

    timer.wait(Duration::from_micros(1));
    let count_after_drain = elastic_buffer.data_count();
    uwriteln!(uart, "  After draining 5: count={}", count_after_drain).unwrap();

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

/// Verifies that we can submit two adjustments back-to-back. The second adjustment
/// should stall until the first completes.
fn test_back_to_back(
    uart: &mut bittide_hal::shared_devices::Uart,
    elastic_buffer: &bittide_hal::shared_devices::ElasticBuffer,
    timer: &bittide_hal::shared_devices::Timer,
) -> bool {
    uwriteln!(uart, "Test: back-to-back adjustments").unwrap();

    // Set buffer to a known state
    elastic_buffer.set_occupancy(0);

    timer.wait(Duration::from_micros(1));
    let count_initial = elastic_buffer.data_count();
    uwriteln!(uart, "  Initial count: {}", count_initial).unwrap();

    // Submit two adjustments back-to-back without waiting between them
    // The second should stall until the first completes
    elastic_buffer.set_adjustment_async(3);
    elastic_buffer.set_adjustment_async(-2);
    elastic_buffer.set_adjustment_wait(());

    timer.wait(Duration::from_micros(1));
    let count_after = elastic_buffer.data_count();
    uwriteln!(uart, "  After both adjustments: count={}", count_after).unwrap();

    // Verify: count should be initial + 3 - 2 = initial + 1
    let expected = count_initial + 1;
    if count_after == expected {
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

/// Verifies that explicit waits between adjustments work correctly.
fn test_async_wait_async_wait(
    uart: &mut bittide_hal::shared_devices::Uart,
    elastic_buffer: &bittide_hal::shared_devices::ElasticBuffer,
    timer: &bittide_hal::shared_devices::Timer,
) -> bool {
    uwriteln!(uart, "Test: async -> wait -> async -> wait").unwrap();

    // Set buffer to a known state
    elastic_buffer.set_occupancy(0);

    timer.wait(Duration::from_micros(1));
    let count_initial = elastic_buffer.data_count();
    uwriteln!(uart, "  Initial count: {}", count_initial).unwrap();

    // First adjustment: submit and wait
    elastic_buffer.set_adjustment_async(7);
    elastic_buffer.set_adjustment_wait(());

    timer.wait(Duration::from_micros(1));
    let count_after_first = elastic_buffer.data_count();
    uwriteln!(
        uart,
        "  After first filling 7 + wait: count={}",
        count_after_first
    )
    .unwrap();

    // Second adjustment: submit and wait
    elastic_buffer.set_adjustment_async(-4);
    elastic_buffer.set_adjustment_wait(());

    timer.wait(Duration::from_micros(1));
    let count_after_second = elastic_buffer.data_count();
    uwriteln!(
        uart,
        "  After second draining 4 + wait: count={}",
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

/// Verifies that the auto-center state machine centers the buffer when enabled.
fn test_auto_center_basic_centering(
    uart: &mut bittide_hal::shared_devices::Uart,
    elastic_buffer: &bittide_hal::shared_devices::ElasticBuffer,
    timer: &bittide_hal::shared_devices::Timer,
) -> bool {
    uwriteln!(uart, "Test: Auto-center basic centering").unwrap();

    // Reset and set up
    elastic_buffer.reset_auto_center();
    elastic_buffer.set_occupancy(0);
    timer.wait(Duration::from_micros(1));

    // Set buffer to +10
    elastic_buffer.set_occupancy(10);
    timer.wait(Duration::from_micros(1));
    let count_before = elastic_buffer.data_count();
    uwriteln!(uart, "  Initial count: {}", count_before).unwrap();

    // Set margin to 2 and enable auto-center
    elastic_buffer.set_auto_center_margin(2);
    elastic_buffer.set_auto_center_enable(true);

    // Wait for it to become idle
    elastic_buffer.wait_auto_center_idle();

    timer.wait(Duration::from_micros(1));
    let count_after = elastic_buffer.data_count();
    let total_adjustments = elastic_buffer.auto_center_total_adjustments();
    uwriteln!(
        uart,
        "  After auto-center: count={}, total_adjustments={}",
        count_after,
        total_adjustments
    )
    .unwrap();

    // Verify occupancy is exactly 0 (auto-center submits adjustment of -10)
    if count_after != 0 {
        uwriteln!(uart, "  FAIL: Expected count=0, got {}", count_after).unwrap();
        return false;
    }

    // Verify total adjustments equals -10 (moved from +10 to 0)
    if total_adjustments != -10 {
        uwriteln!(
            uart,
            "  FAIL: Expected total_adjustments=-10, got {}",
            total_adjustments
        )
        .unwrap();
        return false;
    }

    elastic_buffer.set_auto_center_enable(false);
    uwriteln!(uart, "  PASS").unwrap();
    true
}

/// Verifies that the margin parameter works correctly with different values.
fn test_auto_center_margin_parameter(
    uart: &mut bittide_hal::shared_devices::Uart,
    elastic_buffer: &bittide_hal::shared_devices::ElasticBuffer,
    timer: &bittide_hal::shared_devices::Timer,
) -> bool {
    uwriteln!(uart, "Test: Auto-center margin parameter").unwrap();

    let margins = [0u16, 1, 5];
    let test_offset = 15i8;

    for &margin in &margins {
        // Reset for each test
        elastic_buffer.reset_auto_center();
        elastic_buffer.set_occupancy(0);
        timer.wait(Duration::from_micros(1));

        // Set buffer outside margin
        elastic_buffer.set_occupancy(test_offset);
        timer.wait(Duration::from_micros(1));

        uwriteln!(uart, "  Testing margin={}", margin).unwrap();

        // Set margin and enable
        elastic_buffer.set_auto_center_margin(margin);
        elastic_buffer.set_auto_center_enable(true);

        // Wait for idle
        elastic_buffer.wait_auto_center_idle();

        timer.wait(Duration::from_micros(1));
        let count_after = elastic_buffer.data_count();
        let margin_signed = margin as i8;

        uwriteln!(
            uart,
            "    After centering: count={}, expected range=[{}, {}]",
            count_after,
            -margin_signed,
            margin_signed
        )
        .unwrap();

        // Verify within margin
        if count_after < -margin_signed || count_after > margin_signed {
            uwriteln!(
                uart,
                "    FAIL: Count {} not within margin [{}, {}]",
                count_after,
                -margin_signed,
                margin_signed
            )
            .unwrap();
            return false;
        }

        elastic_buffer.set_auto_center_enable(false);
    }

    uwriteln!(uart, "  PASS").unwrap();
    true
}

/// Verifies basic enable/disable functionality of auto-center.
fn test_auto_center_enable_disable(
    uart: &mut bittide_hal::shared_devices::Uart,
    elastic_buffer: &bittide_hal::shared_devices::ElasticBuffer,
    timer: &bittide_hal::shared_devices::Timer,
) -> bool {
    uwriteln!(uart, "Test: Auto-center enable/disable").unwrap();

    // Reset to known state
    elastic_buffer.reset_auto_center();
    timer.wait(Duration::from_micros(1));

    // Verify starts disabled
    let enabled_initial = elastic_buffer.auto_center_enable();
    if enabled_initial {
        uwriteln!(uart, "  FAIL: Should start disabled").unwrap();
        return false;
    }

    // Enable and verify
    elastic_buffer.set_auto_center_enable(true);
    let enabled_after_enable = elastic_buffer.auto_center_enable();
    if !enabled_after_enable {
        uwriteln!(uart, "  FAIL: Should be enabled after enable").unwrap();
        return false;
    }

    // Disable and verify
    elastic_buffer.set_auto_center_enable(false);
    let enabled_after_disable = elastic_buffer.auto_center_enable();
    if enabled_after_disable {
        uwriteln!(uart, "  FAIL: Should be disabled after disable").unwrap();
        return false;
    }

    uwriteln!(uart, "  PASS").unwrap();
    true
}

/// Verifies the idle state reporting is accurate.
fn test_auto_center_idle_state(
    uart: &mut bittide_hal::shared_devices::Uart,
    elastic_buffer: &bittide_hal::shared_devices::ElasticBuffer,
    timer: &bittide_hal::shared_devices::Timer,
) -> bool {
    uwriteln!(uart, "Test: Auto-center idle state").unwrap();

    // Reset and prepare
    elastic_buffer.reset_auto_center();
    elastic_buffer.set_occupancy(0);
    timer.wait(Duration::from_micros(1));

    // Verify idle when disabled
    let idle_when_disabled = elastic_buffer.auto_center_is_idle();
    if !idle_when_disabled {
        uwriteln!(uart, "  FAIL: Should be idle when disabled").unwrap();
        return false;
    }

    // Set buffer off-center
    elastic_buffer.set_occupancy(20);
    timer.wait(Duration::from_micros(1));

    // Enable auto-center
    elastic_buffer.set_auto_center_margin(2);
    elastic_buffer.set_auto_center_enable(true);

    // Wait for completion
    elastic_buffer.wait_auto_center_idle();

    // Verify idle when centered
    let idle_when_centered = elastic_buffer.auto_center_is_idle();
    if !idle_when_centered {
        uwriteln!(uart, "  FAIL: Should be idle when centered").unwrap();
        return false;
    }

    elastic_buffer.set_auto_center_enable(false);
    uwriteln!(
        uart,
        "  Idle states: disabled={}, centered={}",
        idle_when_disabled,
        idle_when_centered
    )
    .unwrap();
    uwriteln!(uart, "  PASS").unwrap();
    true
}

/// Verifies that total adjustments are tracked correctly across multiple operations.
fn test_auto_center_total_adjustments_tracking(
    uart: &mut bittide_hal::shared_devices::Uart,
    elastic_buffer: &bittide_hal::shared_devices::ElasticBuffer,
    timer: &bittide_hal::shared_devices::Timer,
) -> bool {
    uwriteln!(uart, "Test: Auto-center total adjustments tracking").unwrap();

    // Reset and start from 0
    elastic_buffer.reset_auto_center();
    elastic_buffer.set_occupancy(0);
    timer.wait(Duration::from_micros(1));

    // Verify total starts at 0
    let total_initial = elastic_buffer.auto_center_total_adjustments();
    if total_initial != 0 {
        uwriteln!(
            uart,
            "  FAIL: Total should be 0 initially, got {}",
            total_initial
        )
        .unwrap();
        return false;
    }

    // Set buffer to +15 and center
    elastic_buffer.set_occupancy(15);
    timer.wait(Duration::from_micros(1));
    elastic_buffer.set_auto_center_margin(0);
    elastic_buffer.set_auto_center_enable(true);
    elastic_buffer.wait_auto_center_idle();

    let total_after_first = elastic_buffer.auto_center_total_adjustments();
    uwriteln!(
        uart,
        "  After centering from +15: total={}",
        total_after_first
    )
    .unwrap();

    if total_after_first != -15 {
        uwriteln!(
            uart,
            "  FAIL: Expected total=-15, got {}",
            total_after_first
        )
        .unwrap();
        return false;
    }

    // Manually set buffer to -10 and let it center again
    elastic_buffer.set_auto_center_enable(false);
    elastic_buffer.wait_auto_center_idle();
    elastic_buffer.set_occupancy(-10);
    timer.wait(Duration::from_micros(1));
    elastic_buffer.set_auto_center_enable(true);
    elastic_buffer.wait_auto_center_idle();

    let total_after_second = elastic_buffer.auto_center_total_adjustments();
    uwriteln!(
        uart,
        "  After centering from -10: total={}",
        total_after_second
    )
    .unwrap();

    // Total should be -15 + 10 = -5
    if total_after_second != -5 {
        uwriteln!(
            uart,
            "  FAIL: Expected total=-5, got {}",
            total_after_second
        )
        .unwrap();
        return false;
    }

    elastic_buffer.set_auto_center_enable(false);
    uwriteln!(uart, "  PASS").unwrap();
    true
}

/// Verifies the reset functionality works correctly.
fn test_auto_center_reset_functionality(
    uart: &mut bittide_hal::shared_devices::Uart,
    elastic_buffer: &bittide_hal::shared_devices::ElasticBuffer,
    timer: &bittide_hal::shared_devices::Timer,
) -> bool {
    uwriteln!(uart, "Test: Auto-center reset functionality").unwrap();

    // Make some adjustments first
    elastic_buffer.reset_auto_center();
    elastic_buffer.set_occupancy(12);
    timer.wait(Duration::from_micros(1));
    elastic_buffer.set_auto_center_margin(0);
    elastic_buffer.set_auto_center_enable(true);
    elastic_buffer.wait_auto_center_idle();

    let total_before_reset = elastic_buffer.auto_center_total_adjustments();
    uwriteln!(
        uart,
        "  Total adjustments before reset: {}",
        total_before_reset
    )
    .unwrap();

    if total_before_reset == 0 {
        uwriteln!(uart, "  FAIL: Total should be non-zero before reset").unwrap();
        return false;
    }

    // Reset
    elastic_buffer.reset_auto_center();
    timer.wait(Duration::from_micros(1));

    // Verify total is 0
    let total_after_reset = elastic_buffer.auto_center_total_adjustments();
    if total_after_reset != 0 {
        uwriteln!(
            uart,
            "  FAIL: Total should be 0 after reset, got {}",
            total_after_reset
        )
        .unwrap();
        return false;
    }

    // Verify disabled
    let enabled_after_reset = elastic_buffer.auto_center_enable();
    if enabled_after_reset {
        uwriteln!(uart, "  FAIL: Should be disabled after reset").unwrap();
        return false;
    }

    // Verify idle
    let idle_after_reset = elastic_buffer.auto_center_is_idle();
    if !idle_after_reset {
        uwriteln!(uart, "  FAIL: Should be idle after reset").unwrap();
        return false;
    }

    uwriteln!(uart, "  PASS").unwrap();
    true
}

/// Verifies interaction between manual and auto-center adjustments.
fn test_auto_center_with_manual_adjustments(
    uart: &mut bittide_hal::shared_devices::Uart,
    elastic_buffer: &bittide_hal::shared_devices::ElasticBuffer,
    timer: &bittide_hal::shared_devices::Timer,
) -> bool {
    uwriteln!(uart, "Test: Auto-center with manual adjustments").unwrap();

    // Reset and prepare
    elastic_buffer.reset_auto_center();
    elastic_buffer.set_occupancy(10);
    timer.wait(Duration::from_micros(1));

    // Enable auto-center
    elastic_buffer.set_auto_center_margin(2);
    elastic_buffer.set_auto_center_enable(true);
    timer.wait(Duration::from_micros(1));

    // Submit a manual adjustment while auto-center is active
    elastic_buffer.set_adjustment(5);
    timer.wait(Duration::from_micros(1));

    // Wait for auto-center to finish
    elastic_buffer.wait_auto_center_idle();

    let count_final = elastic_buffer.data_count();
    let total_adjustments = elastic_buffer.auto_center_total_adjustments();

    uwriteln!(
        uart,
        "  Final count={}, auto_center_total={}",
        count_final,
        total_adjustments
    )
    .unwrap();

    // The manual +5 and auto-center adjustments should both be applied
    // Auto-center should have applied adjustments to center the buffer
    // Total adjustments should only count auto-center's contributions (not manual)

    // After manual +5, buffer was at 15, auto-center should bring it to within [-2, 2]
    if !(-2..=2).contains(&count_final) {
        uwriteln!(
            uart,
            "  FAIL: Final count {} not within margin",
            count_final
        )
        .unwrap();
        return false;
    }

    elastic_buffer.set_auto_center_enable(false);
    uwriteln!(uart, "  PASS").unwrap();
    true
}

/// Verifies that auto-center doesn't make adjustments when already centered.
fn test_auto_center_already_centered(
    uart: &mut bittide_hal::shared_devices::Uart,
    elastic_buffer: &bittide_hal::shared_devices::ElasticBuffer,
    timer: &bittide_hal::shared_devices::Timer,
) -> bool {
    uwriteln!(uart, "Test: Auto-center already centered").unwrap();

    // Reset and set to 0
    elastic_buffer.reset_auto_center();
    elastic_buffer.set_occupancy(0);
    timer.wait(Duration::from_micros(1));

    let count_before = elastic_buffer.data_count();

    // Enable auto-center with margin 2
    elastic_buffer.set_auto_center_margin(2);
    elastic_buffer.set_auto_center_enable(true);
    elastic_buffer.wait_auto_center_idle();

    let count_after = elastic_buffer.data_count();
    let total_adjustments = elastic_buffer.auto_center_total_adjustments();

    uwriteln!(
        uart,
        "  Before={}, After={}, Total adjustments={}",
        count_before,
        count_after,
        total_adjustments
    )
    .unwrap();

    // Should make no adjustments
    if total_adjustments != 0 {
        uwriteln!(
            uart,
            "  FAIL: Should make no adjustments when already centered, got {}",
            total_adjustments
        )
        .unwrap();
        return false;
    }

    // Buffer should stay at 0
    if count_after != 0 {
        uwriteln!(uart, "  FAIL: Buffer should stay at 0, got {}", count_after).unwrap();
        return false;
    }

    elastic_buffer.set_auto_center_enable(false);
    uwriteln!(uart, "  PASS").unwrap();
    true
}

/// Verifies the wait_auto_center_idle helper function.
fn test_auto_center_wait_idle(
    uart: &mut bittide_hal::shared_devices::Uart,
    elastic_buffer: &bittide_hal::shared_devices::ElasticBuffer,
    timer: &bittide_hal::shared_devices::Timer,
) -> bool {
    uwriteln!(uart, "Test: Auto-center wait_idle helper").unwrap();

    // Reset and prepare
    elastic_buffer.reset_auto_center();
    elastic_buffer.set_occupancy(25);
    timer.wait(Duration::from_micros(1));

    // Enable auto-center
    elastic_buffer.set_auto_center_margin(2);
    elastic_buffer.set_auto_center_enable(true);

    // Immediately call wait_idle
    elastic_buffer.wait_auto_center_idle();

    // Should not return until idle, and buffer should be centered
    let count_after = elastic_buffer.data_count();
    let is_idle = elastic_buffer.auto_center_is_idle();

    uwriteln!(
        uart,
        "  After wait_idle: count={}, is_idle={}",
        count_after,
        is_idle
    )
    .unwrap();

    if !is_idle {
        uwriteln!(uart, "  FAIL: Should be idle after wait_idle returns").unwrap();
        return false;
    }

    if !(-2..=2).contains(&count_after) {
        uwriteln!(
            uart,
            "  FAIL: Should be centered after wait_idle, got {}",
            count_after
        )
        .unwrap();
        return false;
    }

    elastic_buffer.set_auto_center_enable(false);
    uwriteln!(uart, "  PASS").unwrap();
    true
}

/// Verifies edge case with margin = 0 (centers exactly at 0).
fn test_auto_center_zero_margin(
    uart: &mut bittide_hal::shared_devices::Uart,
    elastic_buffer: &bittide_hal::shared_devices::ElasticBuffer,
    timer: &bittide_hal::shared_devices::Timer,
) -> bool {
    uwriteln!(uart, "Test: Auto-center zero margin").unwrap();

    // Reset and set off-center
    elastic_buffer.reset_auto_center();
    elastic_buffer.set_occupancy(8);
    timer.wait(Duration::from_micros(1));

    // Set margin to 0
    elastic_buffer.set_auto_center_margin(0);
    elastic_buffer.set_auto_center_enable(true);
    elastic_buffer.wait_auto_center_idle();
    timer.wait(Duration::from_micros(1));

    let count_after = elastic_buffer.data_count();
    uwriteln!(
        uart,
        "  After centering with margin=0: count={}",
        count_after
    )
    .unwrap();

    if count_after != 0 {
        uwriteln!(
            uart,
            "  FAIL: Should be exactly 0 with margin=0, got {}",
            count_after
        )
        .unwrap();
        return false;
    }

    elastic_buffer.set_auto_center_enable(false);
    uwriteln!(uart, "  PASS").unwrap();
    true
}

/// Verifies that auto-center continuously maintains centering across perturbations.
fn test_auto_center_persistence_across_multiple_perturbations(
    uart: &mut bittide_hal::shared_devices::Uart,
    elastic_buffer: &bittide_hal::shared_devices::ElasticBuffer,
    timer: &bittide_hal::shared_devices::Timer,
) -> bool {
    uwriteln!(uart, "Test: Auto-center persistence across perturbations").unwrap();

    // Reset and enable
    elastic_buffer.reset_auto_center();
    elastic_buffer.set_occupancy(0);
    timer.wait(Duration::from_micros(1));
    elastic_buffer.set_auto_center_margin(2);
    elastic_buffer.set_auto_center_enable(true);
    elastic_buffer.wait_auto_center_idle();

    let perturbations = [7i32, -12, 18, -5];

    for &perturbation in &perturbations {
        uwriteln!(uart, "  Applying perturbation: {}", perturbation).unwrap();

        // Manually adjust buffer off-center
        elastic_buffer.set_adjustment(perturbation);
        timer.wait(Duration::from_micros(1));

        // Wait for auto-center to fix it
        elastic_buffer.wait_auto_center_idle();

        let count = elastic_buffer.data_count();
        uwriteln!(uart, "    After re-centering: count={}", count).unwrap();

        // Verify it's centered again
        if !(-2..=2).contains(&count) {
            uwriteln!(
                uart,
                "    FAIL: Should re-center after perturbation, got {}",
                count
            )
            .unwrap();
            return false;
        }
    }

    elastic_buffer.set_auto_center_enable(false);
    uwriteln!(uart, "  PASS").unwrap();
    true
}

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let mut uart = INSTANCES.uart;
    let elastic_buffer = INSTANCES.elastic_buffer;
    let timer = INSTANCES.timer;

    let mut all_passed = true;

    all_passed &= test_set_occupancy_to_midpoint(&mut uart, &elastic_buffer);
    all_passed &= test_zero_adjustment(&mut uart, &elastic_buffer, &timer);
    all_passed &= test_multiple_drain_commands(&mut uart, &elastic_buffer);
    all_passed &= test_underflow_flag_sticky(&mut uart, &elastic_buffer, &timer);
    all_passed &= test_overflow_flag_sticky(&mut uart, &elastic_buffer, &timer);
    all_passed &= test_multiple_items_command(&mut uart, &elastic_buffer, &timer);
    all_passed &= test_back_to_back(&mut uart, &elastic_buffer, &timer);
    all_passed &= test_async_wait_async_wait(&mut uart, &elastic_buffer, &timer);

    // Auto-center tests
    all_passed &= test_auto_center_basic_centering(&mut uart, &elastic_buffer, &timer);
    all_passed &= test_auto_center_margin_parameter(&mut uart, &elastic_buffer, &timer);
    all_passed &= test_auto_center_enable_disable(&mut uart, &elastic_buffer, &timer);
    all_passed &= test_auto_center_idle_state(&mut uart, &elastic_buffer, &timer);
    all_passed &= test_auto_center_total_adjustments_tracking(&mut uart, &elastic_buffer, &timer);
    all_passed &= test_auto_center_reset_functionality(&mut uart, &elastic_buffer, &timer);
    all_passed &= test_auto_center_with_manual_adjustments(&mut uart, &elastic_buffer, &timer);
    all_passed &= test_auto_center_already_centered(&mut uart, &elastic_buffer, &timer);
    all_passed &= test_auto_center_wait_idle(&mut uart, &elastic_buffer, &timer);
    all_passed &= test_auto_center_zero_margin(&mut uart, &elastic_buffer, &timer);
    all_passed &= test_auto_center_persistence_across_multiple_perturbations(
        &mut uart,
        &elastic_buffer,
        &timer,
    );

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
