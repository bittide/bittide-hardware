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
      input wire  SYSCLK_300_P // clock
    , input wire  SYSCLK_300_N // clock
    , input wire  USER_SMA_CLOCK_P // clock
    , input wire  USER_SMA_CLOCK_N // clock
    , input wire  reset // reset
    , input wire  drainFifo
    , input wire  stabilityCheckReset

      // Outputs
    , output wire  FINC
    , output wire  FDEC
    , output wire  Underflowed
    , output wire  Overflowed
    , output wire  isStable
    , output wire [1:0] EbMode
    );

    wire clkInternal;
    wire clkExternal;
    clockControlDemo0 clockControlDemo0
    ( // Inputs
      .clkInteral(clkInternal) // clock
    , .clkExternal(clkExternal) // clock
    , .rstExternal(reset) // reset
    , .drainFifo(drainFifo)
    , .stabilityCheckReset(stabilityCheckReset)
      // Outputs
    , .FINC(FINC)
    , .FDEC(FDEC)

    , .Underflowed(Underflowed)
    , .Overflowed(Overflowed)
    , .isStable(isStable)
    , .EbMode(EbMode)

    );

    clk_wiz_0 clk_wiz_0
    (
        .clkInternal(clkInternal),
        // Status and control signals
        .clk_in1_p(SYSCLK_300_P),
        .clk_in1_n(SYSCLK_300_N));

    clk_wiz_1 clk_wiz_1
        (
            .clkExternal(clkExternal),
             .clk_in1_p(USER_SMA_CLOCK_P),
             .clk_in1_n(USER_SMA_CLOCK_N));

endmodule
