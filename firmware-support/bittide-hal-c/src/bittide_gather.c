// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#include "bittide_gather.h"

GatherUnit gather_unit_init(
    volatile uint64_t* gather_memory,
    volatile uint32_t* metacycle_count,
    volatile uint32_t* metacycle_register,
    uint32_t memory_len
) {
    GatherUnit unit = {
        .gather_memory = gather_memory,
        .metacycle_count = metacycle_count,
        .metacycle_register = metacycle_register,
        .memory_len = memory_len
    };
    return unit;
}

void gather_unit_write_slice(
    const GatherUnit* unit,
    const uint64_t* src,
    uint32_t offset,
    uint32_t len
) {
    for (uint32_t i = 0; i < len; i++) {
        unit->gather_memory[offset + i] = src[i];
    }
}

void gather_unit_wait_for_new_metacycle(const GatherUnit* unit) {
    // Reading this register causes a stall until end of metacycle
    (void)*unit->metacycle_register;
}

uint32_t gather_unit_metacycle_count(const GatherUnit* unit) {
    return *unit->metacycle_count;
}
