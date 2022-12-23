`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company:
// Engineer:
//
// Create Date: 11/23/2022 04:39:35 PM
// Design Name:
// Module Name: topEntity
// Project Name:
// Target Devices:
// Tool Versions:
// Description:
//
// Dependencies:
//
// Revision:
// Revision 0.01 - File Created
// Additional Comments:
//
//////////////////////////////////////////////////////////////////////////////////

// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0


module topEntity
    ( // Inputs
      input wire  USER_SMA_CLOCK_P // clock
    , input wire  USER_SMA_CLOCK_N // clock
    , input wire  FMC_HPC_CLK1_M2C_P // clock
    , input wire  FMC_HPC_CLK1_M2C_N // clock
    , input wire  drainFifoA
    , input wire  drainFifoB

      // Outputs
    , output wire  domA_FINC
    , output wire  domA_FDEC
    , output wire  domA_isStable
    , output wire  domB_FINC
    , output wire  domB_FDEC
    , output wire  domB_isStable
    );

    wire clkA;
    wire clkB;

    clockControlDemo1 clockControlDemo1
    ( // Inputs
      .clkA(clkA) // clock
    , .clkB(clkB) // clock
    , .drainFifoA(drainFifoA)
    , .drainFifoB(drainFifoB)

      // Outputs
    , .domA_FINC(domA_FINC)
    , .domA_FDEC(domA_FDEC)
    , .domA_isStable(domA_isStable)
    , .domB_FINC(domB_FINC)
    , .domB_FDEC(domB_FDEC)
    , .domB_isStable(domB_isStable)

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
       .clk_in1_p(USER_SMA_CLOCK_P),
       .clk_in1_n(USER_SMA_CLOCK_N));

endmodule
