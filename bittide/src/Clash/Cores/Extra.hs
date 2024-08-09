-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Clash.Cores.Extra where

import Clash.Annotations.Primitive
import Clash.Explicit.Prelude hiding (Fixed, (:<))

import Clash.Netlist.Types (BlackBoxContext (..), HWType (..), TemplateFunction (..))
import Clash.Netlist.Util (stripVoid)
import Data.Fixed (E3, Fixed (..))
import Data.List.Infinite (Infinite ((:<)), (...))
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (__i)

{- | A typical dual flipflop synchronizer, prepended with a flipflop operating
in the source domain. The two flipflops operating in the target domain are
packed tightly together using Vivado's ASYNC_REG synthesis attribute. For more
information see:

    https://docs.xilinx.com/r/en-US/ug901-vivado-synthesis/ASYNC_REG

HDL generation also generates an @.sdc@ file for Vivado with the correct
timing constraints for the synchronizer. The HDL contains unique register
names so the SDC can match on just these registers.

__N.B.__: You cannot synchronize words by combining multiple instantiations
          of 'safeDffSynchronizer'. If you want to do this, look into
          'dcFifo'.
-}
safeDffSynchronizer ::
  forall dom1 dom2 a.
  ( KnownDomain dom1
  , KnownDomain dom2
  , NFDataX a
  , BitSize a ~ 1
  ) =>
  Clock dom1 ->
  Clock dom2 ->
  a ->
  Signal dom1 a ->
  Signal dom2 a
safeDffSynchronizer clk1 clk2 initVal i =
  snd $ safeDffSynchronizer0 clk1 clk2 initVal i

{- | Like 'safeDffSynchronizer', but the source register is provided on the
output for further use in the source domain
-}
safeDffSynchronizer0 ::
  forall dom1 dom2 a.
  ( KnownDomain dom1
  , KnownDomain dom2
  , NFDataX a
  , BitSize a ~ 1
  ) =>
  Clock dom1 ->
  Clock dom2 ->
  a ->
  Signal dom1 a ->
  (Signal dom1 a, Signal dom2 a)
safeDffSynchronizer0 clk1 clk2 initVal i = (sOut, dOut)
 where
  dOut =
    flipflop clk2
      . flipflop clk2
      $ unsafeSynchronizer clk1 clk2 sOut
  sOut = flipflop clk1 i
  flipflop :: (KnownDomain dom) => Clock dom -> Signal dom a -> Signal dom a
  flipflop clk = delay clk enableGen initVal
{-# OPAQUE safeDffSynchronizer0 #-}
{-# ANN safeDffSynchronizer0 hasBlackBox #-}
{-# ANN
  safeDffSynchronizer0
  ( let
      ( dom1
          :< dom2
          :< _nfdatax
          :< _bitsize
          :< clock1
          :< clock2
          :< initVal
          :< inp
          :< _
        ) = ((0 :: Int) ...)

      ( regA
          :< regB
          :< regC
          :< _
        ) = ((0 :: Int) ...)
      funcName = 'safeDffSynchronizer0
      tfName = 'safeDffSynchronizerTF
     in
      InlineYamlPrimitive
        [Verilog, SystemVerilog]
        [__i|
      BlackBox:
        kind: Declaration
        name: #{funcName}
        template: |-
          // begin safeDffSynchronizer
          (* DONT_TOUCH = "yes" *) reg ~INCLUDENAME[0]_~GENSYM[dff_sync_a][#{regA}] = ~CONST[#{initVal}];
          (* ASYNC_REG = "TRUE" *) reg ~INCLUDENAME[0]_~GENSYM[dff_sync_b][#{regB}] = ~CONST[#{initVal}], ~GENSYM[dff_sync_c][#{regC}] = ~CONST[#{initVal}];

          always @(~IF~ACTIVEEDGE[Rising][#{dom1}]~THENposedge~ELSEnegedge~FI ~ARG[#{clock1}]) begin
            ~INCLUDENAME[0]_~SYM[#{regA}] <= ~VAR[in][#{inp}];
          end

          always @(~IF~ACTIVEEDGE[Rising][#{dom2}]~THENposedge~ELSEnegedge~FI ~ARG[#{clock2}]) begin
            ~INCLUDENAME[0]_~SYM[#{regB}] <= ~INCLUDENAME[0]_~SYM[#{regA}];
            ~SYM[#{regC}] <= ~INCLUDENAME[0]_~SYM[#{regB}];
          end

          assign ~RESULT = {~INCLUDENAME[0]_~SYM[#{regA}], ~SYM[#{regC}]};
          // end safeDffSynchronizer
        includes:
          - extension: sdc
            name: dff_sync
            format: Haskell
            templateFunction: #{tfName}
|]
  )
  #-}
{-# ANN
  safeDffSynchronizer0
  ( let
      ( dom1
          :< dom2
          :< _nfdatax
          :< _bitsize
          :< clock1
          :< clock2
          :< initVal
          :< inp
          :< _
        ) = ((0 :: Int) ...)

      ( regA
          :< regB
          :< regC
          :< block
          :< _
        ) = ((0 :: Int) ...)
      funcName = 'safeDffSynchronizer0
      tfName = 'safeDffSynchronizerTF
     in
      InlineYamlPrimitive
        [VHDL]
        [__i|
      BlackBox:
        kind: Declaration
        name: #{funcName}
        template: |-
          -- begin safeDffSynchronizer
          ~GENSYM[_safeDffSynchronizer][#{block}] : block
            signal ~INCLUDENAME[0]_~GENSYM[dff_sync_a][#{regA}] : ~TYP[#{inp}] := ~CONST[#{initVal}];
            signal ~INCLUDENAME[0]_~GENSYM[dff_sync_b][#{regB}] : ~TYP[#{inp}] := ~CONST[#{initVal}];
            signal ~GENSYM[dff_sync_c][#{regC}] : ~TYP[#{inp}] := ~CONST[#{initVal}];

            attribute DONT_TOUCH : string;
            attribute DONT_TOUCH of ~INCLUDENAME[0]_~SYM[#{regA}] : signal is "TRUE";

            attribute ASYNC_REG : string;
            attribute ASYNC_REG of ~INCLUDENAME[0]_~SYM[#{regB}] : signal is "TRUE";
            attribute ASYNC_REG of ~SYM[#{regC}] : signal is "TRUE";
          begin
            process(~ARG[#{clock1}])
            begin
              if ~IF~ACTIVEEDGE[Rising][#{dom1}]~THENrising_edge~ELSEfalling_edge~FI(~ARG[#{clock1}]) then
                ~INCLUDENAME[0]_~SYM[#{regA}] <= ~VAR[in][#{inp}];
              end if;
            end process;

            process(~ARG[#{clock2}])
            begin
              if ~IF~ACTIVEEDGE[Rising][#{dom2}]~THENrising_edge~ELSEfalling_edge~FI(~ARG[#{clock2}]) then
                ~INCLUDENAME[0]_~SYM[#{regB}] <= ~INCLUDENAME[0]_~SYM[#{regA}];
                ~SYM[#{regC}] <= ~INCLUDENAME[0]_~SYM[#{regB}];
              end if;
            end process;

            ~RESULT <= (~INCLUDENAME[0]_~SYM[#{regA}], ~SYM[#{regC}]);
          end block;
          -- end safeDffSynchronizer
        includes:
          - extension: sdc
            name: dff_sync
            format: Haskell
            templateFunction: #{tfName}
|]
  )
  #-}

safeDffSynchronizerTF :: TemplateFunction
safeDffSynchronizerTF =
  let
    ( dom1Used
        :< dom2Used
        :< _nfdatax
        :< _bitsize
        :< _clock1
        :< _clock2
        :< _initVal
        :< _inp
        :< _
      ) = ((0 :: Int) ...)
   in
    TemplateFunction [dom1Used, dom2Used] (const True) $ \bbCtx ->
      pure . fromMaybe (error "Pattern match failure") $ do
        [compName] <- pure (bbQsysIncName bbCtx)
        [ (_, stripVoid -> dom1, _)
          , (_, stripVoid -> dom2, _)
          , _nfdatax
          , _bitsize
          , _clock1
          , _clock2
          , _initVal
          , _inp
          ] <-
          pure (bbInputs bbCtx)
        KnownDomain _ dom1Period _ _ _ _ <- pure dom1
        KnownDomain _ dom2Period _ _ _ _ <- pure dom2
        let minPeriodNs = MkFixed $ min dom1Period dom2Period :: Fixed E3
        [__i|
      set_max_delay \\
          -datapath_only \\
          -from \\
          [get_pins -hierarchical *#{compName}_dff_sync_a*/C] \\
          -to \\
          [get_pins -hierarchical *#{compName}_dff_sync_b*/D] \\
          #{minPeriodNs}
    |]
