// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#ifndef VEX_RISCV_FFI_H
#define VEX_RISCV_FFI_H

#include <stdint.h>

typedef int bit;

typedef struct {
  bit      reset;
  bit      timerInterrupt;
  bit      externalInterrupt;
  bit      softwareInterrupt;

  bit      iBusWishbone_ACK;
  uint32_t iBusWishbone_DAT_MISO;
  bit      iBusWishbone_ERR;

  bit      dBusWishbone_ACK;
  uint32_t dBusWishbone_DAT_MISO;
  bit      dBusWishbone_ERR;
} INPUT;

typedef struct {
  bit      jtag_TMS;
  bit      jtag_TDI;
} JTAG_INPUT;

typedef struct {
  bit      iBusWishbone_CYC;
  bit      iBusWishbone_STB;
  bit      iBusWishbone_WE;
  uint32_t iBusWishbone_ADR;
  uint32_t iBusWishbone_DAT_MOSI;
  uint8_t  iBusWishbone_SEL;
  uint8_t  iBusWishbone_CTI;
  uint8_t  iBusWishbone_BTE;

  bit      dBusWishbone_CYC;
  bit      dBusWishbone_STB;
  bit      dBusWishbone_WE;
  uint32_t dBusWishbone_ADR;
  uint32_t dBusWishbone_DAT_MOSI;
  uint8_t  dBusWishbone_SEL;
  uint8_t  dBusWishbone_CTI;
  uint8_t  dBusWishbone_BTE;
} OUTPUT;

typedef struct {
  bit      debug_resetOut;
  bit      jtag_TDO;
} JTAG_OUTPUT;

#endif