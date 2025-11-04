/* SPDX-FileCopyrightText: 2025 Google LLC
 *
 * SPDX-License-Identifier: Apache-2.0
 */

#ifndef VEXRISCV_MEMMAP_H
#define VEXRISCV_MEMMAP_H

#include "stdint.h"

/* Device base addresses from VexRiscv.json memory map */

/* Instruction Memory: 0x80000000 (2147483648) */
#define IMEM_BASE           0x80000000UL

/* Data Memory: 0x20000000 (536870912) */
#define DMEM_BASE           0x20000000UL

/* Timer: 0x40000000 (1073741824) */
#define TIMER_BASE          0x40000000UL
#define TIMER_COMMAND       ((volatile uint8_t*)(TIMER_BASE + 0x00))
#define TIMER_CMP_RESULT    ((volatile uint8_t*)(TIMER_BASE + 0x04))
#define TIMER_SCRATCHPAD    ((volatile uint64_t*)(TIMER_BASE + 0x08))
#define TIMER_FREQUENCY     ((volatile uint64_t*)(TIMER_BASE + 0x10))

/* UART: 0x60000000 (1610612736) */
#define UART_BASE           0x60000000UL
#define UART_DATA           ((volatile uint8_t*)(UART_BASE + 0x00))
#define UART_STATUS         ((volatile uint8_t*)(UART_BASE + 0x04))

/* Status Register: 0xA0000000 (2684354560) */
#define STATUS_BASE         0xA0000000UL
#define STATUS_REG          ((volatile uint8_t*)(STATUS_BASE + 0x00))

/* Test status values (TestStatus enum) */
#define TEST_STATUS_RUNNING  0
#define TEST_STATUS_SUCCESS  1
#define TEST_STATUS_FAIL     2

/* Timer command values (TimeCmd enum) */
#define TIME_CMD_CAPTURE     0
#define TIME_CMD_WAIT_CMP    1

#endif /* VEXRISCV_MEMMAP_H */
