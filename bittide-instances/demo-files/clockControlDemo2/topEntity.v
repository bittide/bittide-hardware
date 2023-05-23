`timescale 1ns / 1ps
// SPDX-FileCopyrightText: 2022-2023 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

module topEntity
    ( // Inputs
      input wire USER_SMA_CLOCK_P // clock
    , input wire USER_SMA_CLOCK_N // clock
    , input wire FMC_HPC_CLK1_M2C_P // clock
    , input wire FMC_HPC_CLK1_M2C_N // clock
    , input wire FMC_LPC_CLK1_M2C_P // clock
    , input wire FMC_LPC_CLK1_M2C_N // clock

    , input wire rstA // reset
    , input wire rstB // reset
    , input wire rstC // reset

    // , input wire drainA_B
    // , input wire drainA_C

    // , input wire drainB_A
    // , input wire drainB_C

    // , input wire drainC_A
    // , input wire drainC_B

      // Outputs
    , output wire domA_FINC
    , output wire domA_FDEC
    , output wire domA_StableB
    , output wire domA_StableC
    , output wire domB_FINC
    , output wire domB_FDEC
    , output wire domB_StableA
    , output wire domB_StableC
    , output wire domC_FINC
    , output wire domC_FDEC
    , output wire domC_StableA
    , output wire domC_StableB
    );

    wire clkA;
    wire clkB;
    wire clkC;
    wire drainA_B = 0;
    wire drainA_C = 0;
    wire drainB_A = 0;
    wire drainB_C = 0;
    wire drainC_A = 0;
    wire drainC_B = 0;

    clockControlDemo2 clockControlDemo2
    ( // Inputs
      .clkA(clkA) // clock
    , .clkB(clkB) // clock
    , .clkC(clkC) // clock
    , .rstA(rstA) // reset
    , .rstB(rstB) // reset
    , .rstC(rstC) // reset
    , .drainA_B(drainA_B)
    , .drainA_C(drainA_C)
    , .drainB_A(drainB_A)
    , .drainB_C(drainB_C)
    , .drainC_A(drainC_A)
    , .drainC_B(drainC_B)

      // Outputs
    , .domA_FINC(domA_FINC)
    , .domA_FDEC(domA_FDEC)
    , .domA_StableB(domA_StableB)
    , .domA_StableC(domA_StableC)
    , .domB_FINC(domB_FINC)
    , .domB_FDEC(domB_FDEC)
    , .domB_StableA(domB_StableA)
    , .domB_StableC(domB_StableC)
    , .domC_FINC(domC_FINC)
    , .domC_FDEC(domC_FDEC)
    , .domC_StableA(domC_StableA)
    , .domC_StableB(domC_StableB)

    );

    clk_wiz_0 clk_wiz_0
    (
        .clkA(clkA),
        // Status and control signals
        .clk_in1_p(FMC_HPC_CLK1_M2C_P),
        .clk_in1_n(FMC_HPC_CLK1_M2C_N));
    clk_wiz_1 clk_wiz_1
    (
       .clkB(clkB),
       .clk_in1_p(FMC_LPC_CLK1_M2C_P),
       .clk_in1_n(FMC_LPC_CLK1_M2C_N));

    clk_wiz_2 clk_wiz_2
    (
       .clkC(clkC),
       .clk_in1_p(USER_SMA_CLOCK_P),
       .clk_in1_n(USER_SMA_CLOCK_N));

endmodule
