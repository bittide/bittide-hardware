-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE QuasiQuotes #-}

module Clash.Cores.Extra where

import Clash.Annotations.Primitive
import Clash.Explicit.Prelude

import Data.String.Interpolate (__i)


-- | A typical dual flipflop synchronizer, prepended with a flipflop operating
-- in the source domain. The two flipflops operating in the target domain are
-- packed tightly together using Vivado's ASYNC_REG synthesis attribute. For more
-- information see:
--
--     https://docs.xilinx.com/r/en-US/ug901-vivado-synthesis/ASYNC_REG
--
-- The first flipflop, i.e. the one operating in the source domain, is called
-- @dff_sync_a@. This might not exactly match the name Clash produces, as it
-- needs to generate unique names. I.e., the real signal names will be named
-- according to the following pattern:
--
--   * @dff_sync_a@
--   * @dff_sync_a_0@
--   * @dff_sync_a_1@
--   * etc.
--
-- Similarly, the two flipflops operating in the target domain are called
-- @dff_sync_b@ and @dff_sync_c@. While the registers are automatically packed
-- together, you'll still need to set max delay constraints to prevent undesirable,
-- long paths between the first flipflop and the pair of flipflops. To do so,
-- use the following:
--
-- @
-- set_max_delay
--   -from [get_pins * -filter {NAME =~ "dff_sync_a*_reg/C"}]
--   -to   [get_pins * -filter {NAME =~ "dff_sync_b*_reg/D"}]
--   -datapath_only
--   [get_property -min PERIOD [get_clocks]]
-- @
--
-- __N.B.__: You cannot synchronize words by combining multiple instantiations
--           of 'tripleFlipFlopSynchronizer'. If you want to do this, look into
--           'dcFifo'.
--
tripleFlipFlopSynchronizer ::
  forall dom1 dom2 a.
  ( KnownDomain dom1
  , KnownDomain dom2
  , NFDataX a
  , BitSize a ~ 1 ) =>
  Clock dom1 ->
  Clock dom2 ->
  a ->
  Signal dom1 a ->
  Signal dom2 a
tripleFlipFlopSynchronizer clk1 clk2 initVal =
    flipflop clk2
  . flipflop clk2
  . unsafeSynchronizer clk1 clk2
  . flipflop clk1
 where
  flipflop :: KnownDomain dom => Clock dom -> Signal dom a -> Signal dom a
  flipflop clk = delay clk enableGen initVal
{-# NOINLINE tripleFlipFlopSynchronizer #-}
{-# ANN tripleFlipFlopSynchronizer hasBlackBox #-}
{-# ANN tripleFlipFlopSynchronizer (
  let
    (  dom1
     : dom2
     : _nfdatax
     : _bitsize
     : clock1
     : clock2
     : initVal
     : inp
     : _
     ) = [(0::Int)..]

    (  regA
     : regB
     : regC
     : _
     ) = [(0::Int)..]
  in
    InlineYamlPrimitive [Verilog, SystemVerilog] [__i|
      BlackBox:
        kind: Declaration
        name: Clash.Cores.Extra.tripleFlipFlopSynchronizer
        template: |-
          // begin tripleFlipFlopSynchronizer
          (* DONT_TOUCH = "yes" *) reg ~GENSYM[dff_sync_a][#{regA}] = ~CONST[#{initVal}];
          (* ASYNC_REG = "TRUE" *) reg ~GENSYM[dff_sync_b][#{regB}] = ~CONST[#{initVal}], ~GENSYM[dff_sync_c][#{regC}] = ~CONST[#{initVal}];

          always @(~IF~ACTIVEEDGE[Rising][#{dom1}]~THENposedge~ELSEnegedge~FI ~ARG[#{clock1}]) begin
            ~SYM[#{regA}] <= ~VAR[in][#{inp}];
          end

          always @(~IF~ACTIVEEDGE[Rising][#{dom2}]~THENposedge~ELSEnegedge~FI ~ARG[#{clock2}]) begin
            ~SYM[#{regB}] <= ~SYM[#{regA}];
            ~SYM[#{regC}] <= ~SYM[#{regB}];
          end

          assign ~RESULT = ~SYM[#{regC}];
          // end tripleFlipFlopSynchronizer
|]) #-}
{-# ANN tripleFlipFlopSynchronizer (
  let
    (  dom1
     : dom2
     : _nfdatax
     : _bitsize
     : clock1
     : clock2
     : initVal
     : inp
     : _
     ) = [(0::Int)..]

    (  regA
     : regB
     : regC
     : block
     : _
     ) = [(0::Int)..]
  in
    InlineYamlPrimitive [VHDL] [__i|
      BlackBox:
        kind: Declaration
        name: Clash.Cores.Extra.tripleFlipFlopSynchronizer
        template: |-
          -- begin tripleFlipFlopSynchronizer
          ~GENSYM[tripleFlipFlopSynchronizer][#{block}] : block
            signal ~GENSYM[dff_sync_a][#{regA}] : ~TYPO := ~CONST[#{initVal}];
            signal ~GENSYM[dff_sync_b][#{regB}] : ~TYPO := ~CONST[#{initVal}];
            signal ~GENSYM[dff_sync_c][#{regC}] : ~TYPO := ~CONST[#{initVal}];

            attribute DONT_TOUCH : string;
            attribute DONT_TOUCH of ~SYM[#{regA}] : signal is "TRUE";

            attribute ASYNC_REG : string;
            attribute ASYNC_REG of ~SYM[#{regB}] : signal is "TRUE";
            attribute ASYNC_REG of ~SYM[#{regC}] : signal is "TRUE";
          begin
            process(~ARG[#{clock1}])
            begin
              if ~IF~ACTIVEEDGE[Rising][#{dom1}]~THENrising_edge~ELSEfalling_edge~FI(~ARG[#{clock1}]) then
                ~SYM[#{regA}] <= ~VAR[in][#{inp}];
              end if;
            end process;

            process(~ARG[#{clock2}])
            begin
              if ~IF~ACTIVEEDGE[Rising][#{dom2}]~THENrising_edge~ELSEfalling_edge~FI(~ARG[#{clock2}]) then
                ~SYM[#{regB}] <= ~SYM[#{regA}];
                ~SYM[#{regC}] <= ~SYM[#{regB}];
              end if;
            end process;

            ~RESULT <= ~SYM[#{regC}];
          end block;
          -- end tripleFlipFlopSynchronizer
|]) #-}
