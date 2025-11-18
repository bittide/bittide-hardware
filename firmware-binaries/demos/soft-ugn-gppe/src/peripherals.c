// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#include "peripherals.h"
#include "softugndemogppe_memmap.h"

void peripherals_init(Peripherals* peripherals) {
    // Initialize UART
    peripherals->uart = uart_init(UART_DATA, UART_STATUS);

    // Initialize timer
    peripherals->timer = timer_init(
        TIMER_COMMAND,
        TIMER_SCRATCHPAD,
        TIMER_FREQUENCY,
        TIMER_CMP_RESULT
    );

    // Initialize scatter units (ports 0-6)
    peripherals->scatter_units[0] = scatter_unit_init(
        (volatile uint64_t*)SCATTER_UNIT_0_SCATTER_MEMORY,
        (volatile uint32_t*)SCATTER_UNIT_0_METACYCLE_COUNT,
        (volatile uint32_t*)SCATTER_UNIT_0_METACYCLE_REGISTER,
        1000
    );
    peripherals->scatter_units[1] = scatter_unit_init(
        (volatile uint64_t*)SCATTER_UNIT_1_SCATTER_MEMORY,
        (volatile uint32_t*)SCATTER_UNIT_1_METACYCLE_COUNT,
        (volatile uint32_t*)SCATTER_UNIT_1_METACYCLE_REGISTER,
        1000
    );
    peripherals->scatter_units[2] = scatter_unit_init(
        (volatile uint64_t*)SCATTER_UNIT_2_SCATTER_MEMORY,
        (volatile uint32_t*)SCATTER_UNIT_2_METACYCLE_COUNT,
        (volatile uint32_t*)SCATTER_UNIT_2_METACYCLE_REGISTER,
        1000
    );
    peripherals->scatter_units[3] = scatter_unit_init(
        (volatile uint64_t*)SCATTER_UNIT_3_SCATTER_MEMORY,
        (volatile uint32_t*)SCATTER_UNIT_3_METACYCLE_COUNT,
        (volatile uint32_t*)SCATTER_UNIT_3_METACYCLE_REGISTER,
        1000
    );
    peripherals->scatter_units[4] = scatter_unit_init(
        (volatile uint64_t*)SCATTER_UNIT_4_SCATTER_MEMORY,
        (volatile uint32_t*)SCATTER_UNIT_4_METACYCLE_COUNT,
        (volatile uint32_t*)SCATTER_UNIT_4_METACYCLE_REGISTER,
        1000
    );
    peripherals->scatter_units[5] = scatter_unit_init(
        (volatile uint64_t*)SCATTER_UNIT_5_SCATTER_MEMORY,
        (volatile uint32_t*)SCATTER_UNIT_5_METACYCLE_COUNT,
        (volatile uint32_t*)SCATTER_UNIT_5_METACYCLE_REGISTER,
        1000
    );
    peripherals->scatter_units[6] = scatter_unit_init(
        (volatile uint64_t*)SCATTER_UNIT_6_SCATTER_MEMORY,
        (volatile uint32_t*)SCATTER_UNIT_6_METACYCLE_COUNT,
        (volatile uint32_t*)SCATTER_UNIT_6_METACYCLE_REGISTER,
        1000
    );
    peripherals->scatter_units[7] = scatter_unit_init(
        (volatile uint64_t*)SCATTER_UNIT_7_SCATTER_MEMORY,
        (volatile uint32_t*)SCATTER_UNIT_7_METACYCLE_COUNT,
        (volatile uint32_t*)SCATTER_UNIT_7_METACYCLE_REGISTER,
        1000
    );
    // Initialize gather units (ports 0-6)
    peripherals->gather_units[0] = gather_unit_init(
        (volatile uint64_t*)GATHER_UNIT_0_GATHER_MEMORY,
        (volatile uint32_t*)GATHER_UNIT_0_METACYCLE_COUNT,
        (volatile uint32_t*)GATHER_UNIT_0_METACYCLE_REGISTER,
        1000
    );
    peripherals->gather_units[1] = gather_unit_init(
        (volatile uint64_t*)GATHER_UNIT_1_GATHER_MEMORY,
        (volatile uint32_t*)GATHER_UNIT_1_METACYCLE_COUNT,
        (volatile uint32_t*)GATHER_UNIT_1_METACYCLE_REGISTER,
        1000
    );
    peripherals->gather_units[2] = gather_unit_init(
        (volatile uint64_t*)GATHER_UNIT_2_GATHER_MEMORY,
        (volatile uint32_t*)GATHER_UNIT_2_METACYCLE_COUNT,
        (volatile uint32_t*)GATHER_UNIT_2_METACYCLE_REGISTER,
        1000
    );
    peripherals->gather_units[3] = gather_unit_init(
        (volatile uint64_t*)GATHER_UNIT_3_GATHER_MEMORY,
        (volatile uint32_t*)GATHER_UNIT_3_METACYCLE_COUNT,
        (volatile uint32_t*)GATHER_UNIT_3_METACYCLE_REGISTER,
        1000
    );
    peripherals->gather_units[4] = gather_unit_init(
        (volatile uint64_t*)GATHER_UNIT_4_GATHER_MEMORY,
        (volatile uint32_t*)GATHER_UNIT_4_METACYCLE_COUNT,
        (volatile uint32_t*)GATHER_UNIT_4_METACYCLE_REGISTER,
        1000
    );
    peripherals->gather_units[5] = gather_unit_init(
        (volatile uint64_t*)GATHER_UNIT_5_GATHER_MEMORY,
        (volatile uint32_t*)GATHER_UNIT_5_METACYCLE_COUNT,
        (volatile uint32_t*)GATHER_UNIT_5_METACYCLE_REGISTER,
        1000
    );
    peripherals->gather_units[6] = gather_unit_init(
        (volatile uint64_t*)GATHER_UNIT_6_GATHER_MEMORY,
        (volatile uint32_t*)GATHER_UNIT_6_METACYCLE_COUNT,
        (volatile uint32_t*)GATHER_UNIT_6_METACYCLE_REGISTER,
        1000
    );
    peripherals->gather_units[7] = gather_unit_init(
        (volatile uint64_t*)GATHER_UNIT_7_GATHER_MEMORY,
        (volatile uint32_t*)GATHER_UNIT_7_METACYCLE_COUNT,
        (volatile uint32_t*)GATHER_UNIT_7_METACYCLE_REGISTER,
        1000
    );
}
