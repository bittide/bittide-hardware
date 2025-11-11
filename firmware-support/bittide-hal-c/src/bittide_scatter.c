// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#include "bittide_scatter.h"

ScatterUnit scatter_unit_init(
    volatile uint64_t* scatter_memory,
    volatile uint32_t* metacycle_count,
    volatile uint32_t* metacycle_register,
    uint32_t memory_len
) {
    ScatterUnit unit = {
        .scatter_memory = scatter_memory,
        .metacycle_count = metacycle_count,
        .metacycle_register = metacycle_register,
        .memory_len = memory_len
    };
    return unit;
}

void scatter_unit_read_slice(
    const ScatterUnit* unit,
    uint64_t* dst,
    uint32_t offset,
    uint32_t len
) {
    for (uint32_t i = 0; i < len; i++) {
        dst[i] = unit->scatter_memory[offset + i];
    }
}

void scatter_unit_write_slice(
    const ScatterUnit* unit,
    const uint64_t* src,
    uint32_t offset,
    uint32_t len
) {
    for (uint32_t i = 0; i < len; i++) {
        unit->scatter_memory[offset + i] = src[i];
    }
}

void scatter_unit_wait_for_new_metacycle(const ScatterUnit* unit) {
    // Reading this register causes a stall until end of metacycle
    (void)*unit->metacycle_register;
}

uint32_t scatter_unit_metacycle_count(const ScatterUnit* unit) {
    return *unit->metacycle_count;
}
