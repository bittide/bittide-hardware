// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#ifndef PERIPHERALS_H
#define PERIPHERALS_H

#include "bittide_uart.h"
#include "bittide_scatter.h"
#include "bittide_gather.h"
#include "bittide_timer.h"

// Number of scatter/gather unit pairs available
#define NUM_PORTS 8

// Container for all peripherals used in the UGN demo
typedef struct {
    Uart uart;
    Timer timer;
    ScatterUnit scatter_units[NUM_PORTS];
    GatherUnit gather_units[NUM_PORTS];
} Peripherals;

// Initialize all peripherals from memory map
void peripherals_init(Peripherals* peripherals);

#endif // PERIPHERALS_H
