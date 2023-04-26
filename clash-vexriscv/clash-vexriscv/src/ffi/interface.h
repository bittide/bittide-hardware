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


#endif

/*
  input               reset
  input               timerInterrupt,
  input               externalInterrupt,
  input               softwareInterrupt,

  input               iBusWishbone_ACK,
  input      [31:0]   iBusWishbone_DAT_MISO,
  input               iBusWishbone_ERR,

  input               dBusWishbone_ACK,
  input      [31:0]   dBusWishbone_DAT_MISO,
  input               dBusWishbone_ERR,


  output              iBusWishbone_CYC,
  output              iBusWishbone_STB,
  output              iBusWishbone_WE,
  output     [29:0]   iBusWishbone_ADR,
  output     [31:0]   iBusWishbone_DAT_MOSI,
  output     [3:0]    iBusWishbone_SEL,
  output     [2:0]    iBusWishbone_CTI,
  output     [1:0]    iBusWishbone_BTE,

  output              dBusWishbone_CYC,
  output              dBusWishbone_STB,
  output              dBusWishbone_WE,
  output     [29:0]   dBusWishbone_ADR,
  output     [31:0]   dBusWishbone_DAT_MOSI,
  output reg [3:0]    dBusWishbone_SEL,
  output     [2:0]    dBusWishbone_CTI,
  output     [1:0]    dBusWishbone_BTE,
*/
