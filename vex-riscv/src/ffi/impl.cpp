// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#include "VVexRiscv.h"
#include "verilated.h"
#include "interface.h"

extern "C" {
  VVexRiscv* vexr_init();
  void vexr_shutdown(VVexRiscv *top);
  void vexr_step(VVexRiscv *top, const INPUT *input, OUTPUT *output);
}


VVexRiscv* vexr_init()
{
  return new VVexRiscv();
}

void vexr_shutdown(VVexRiscv *top)
{
  delete top;
}

void vexr_step(VVexRiscv *top, const INPUT *input, OUTPUT *output)
{
  // set inputs
  top->reset = input->reset;
  top->timerInterrupt = input->timerInterrupt;
  top->externalInterrupt = input->externalInterrupt;
  top->softwareInterrupt = input->softwareInterrupt;
  top->iBusWishbone_ACK = input->iBusWishbone_ACK;
  top->iBusWishbone_DAT_MISO = input->iBusWishbone_DAT_MISO;
  top->iBusWishbone_ERR = input->iBusWishbone_ERR;
  top->dBusWishbone_ACK = input->dBusWishbone_ACK;
  top->dBusWishbone_DAT_MISO = input->dBusWishbone_DAT_MISO;
  top->dBusWishbone_ERR = input->dBusWishbone_ERR;

  // run one cycle of the simulation
  top->clk = true;
  top->eval();
  top->clk = false;
  top->eval();

  // update outputs
  output->iBusWishbone_CYC = top->iBusWishbone_CYC;
  output->iBusWishbone_STB = top->iBusWishbone_STB;
  output->iBusWishbone_WE = top->iBusWishbone_WE;
  output->iBusWishbone_ADR = top->iBusWishbone_ADR;
  output->iBusWishbone_DAT_MOSI = top->iBusWishbone_DAT_MOSI;
  output->iBusWishbone_SEL = top->iBusWishbone_SEL;
  output->iBusWishbone_CTI = top->iBusWishbone_CTI;
  output->iBusWishbone_BTE = top->iBusWishbone_BTE;
  output->dBusWishbone_CYC = top->dBusWishbone_CYC;
  output->dBusWishbone_STB = top->dBusWishbone_STB;
  output->dBusWishbone_WE = top->dBusWishbone_WE;
  output->dBusWishbone_ADR = top->dBusWishbone_ADR;
  output->dBusWishbone_DAT_MOSI = top->dBusWishbone_DAT_MOSI;
  output->dBusWishbone_SEL = top->dBusWishbone_SEL;
  output->dBusWishbone_CTI = top->dBusWishbone_CTI;
  output->dBusWishbone_BTE = top->dBusWishbone_BTE;
}
