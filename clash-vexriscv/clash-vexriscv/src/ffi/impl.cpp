// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#include "VVexRiscv.h"
#include "verilated.h"
#include "interface.h"

extern "C" {
	VVexRiscv* vexr_init();
	void vexr_shutdown(VVexRiscv *top);

	void vexr_init_stage1(VVexRiscv *top, const NON_COMB_INPUT *input, OUTPUT *output);
	void vexr_init_stage2(VVexRiscv *top, const COMB_INPUT *input);
	void vexr_step_rising_edge(VVexRiscv *top, uint64_t time_add, const NON_COMB_INPUT *input, OUTPUT *output);
	void vexr_step_falling_edge(VVexRiscv *top, uint64_t time_add, const COMB_INPUT *input);
}

static VerilatedContext* contextp = 0;

VVexRiscv* vexr_init()
{
	contextp = new VerilatedContext;
	VVexRiscv *v = new VVexRiscv(contextp);
	Verilated::traceEverOn(true);
	v->clk = false;
	return v;
}

// Set all inputs that cannot combinationaly depend on outputs. I.e., all inputs
// except the Wishbone buses.
void set_non_comb_inputs(VVexRiscv *top, const NON_COMB_INPUT *input)
{
	top->reset = input->reset;
	top->timerInterrupt = input->timerInterrupt;
	top->externalInterrupt = input->externalInterrupt;
	top->softwareInterrupt = input->softwareInterrupt;
}

// Set all inputs that can combinationaly depend on outputs. I.e., the Wishbone
// buses.
void set_comb_inputs(VVexRiscv *top, const COMB_INPUT *input)
{
	top->iBusWishbone_ACK = input->iBusWishbone_ACK;
	top->iBusWishbone_DAT_MISO = input->iBusWishbone_DAT_MISO;
	top->iBusWishbone_ERR = input->iBusWishbone_ERR;
	top->dBusWishbone_ACK = input->dBusWishbone_ACK;
	top->dBusWishbone_DAT_MISO = input->dBusWishbone_DAT_MISO;
	top->dBusWishbone_ERR = input->dBusWishbone_ERR;
}

// Set all outputs
void set_ouputs(VVexRiscv *top, OUTPUT *output)
{
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

void vexr_init_stage1(VVexRiscv *top, const NON_COMB_INPUT *input, OUTPUT *output)
{
	// Set all inputs that cannot combinationaly depend on outputs. I.e., all inputs
	// except the Wishbone buses.
	set_non_comb_inputs(top, input);

	// Combinatorially respond to the inputs
	top->eval();
	set_ouputs(top, output);

	// Advance time by 50 nanoseconds. This is an arbitrary value. Ideally, we would
	// do something similar to Clash's template tag "~LONGESTPERIOD".
	contextp->timeInc(50000);
}

void vexr_init_stage2(VVexRiscv *top, const COMB_INPUT *input)
{
	set_comb_inputs(top, input);
}

void vexr_shutdown(VVexRiscv *top)
{
	delete top;
	delete contextp;
	contextp = 0;
}


void vexr_step_rising_edge(VVexRiscv *top, uint64_t time_add, const NON_COMB_INPUT *input, OUTPUT *output)
{
	// Advance time since last event. Note that this is 0 for the first call to
	// this function. To get a sensisble waveform, vexr_init has already advanced
	// time.
	contextp->timeInc(time_add); // XXX: time_add is in femtoseconds, timeinc expects picoseconds

	// docssss
	set_non_comb_inputs(top, input);

	top->clk = true;
	top->eval();

	// Set all outputs
	set_ouputs(top, output);
}

void vexr_step_falling_edge(VVexRiscv *top, uint64_t time_add, const COMB_INPUT *input)
{
	// advance time since last event
	contextp->timeInc(time_add); // time_add is in femtoseconds, timeinc expects picoseconds

	// Update inputs
	top->clk = false;
	set_comb_inputs(top, input);

	// Evaluate the simulation
	top->eval();
}
