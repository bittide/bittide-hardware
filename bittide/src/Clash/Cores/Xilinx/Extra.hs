-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module Clash.Cores.Xilinx.Extra (module Clash.Cores.Xilinx.Extra, module X) where

import Clash.Prelude

import Data.String.Interpolate (__i)

import Clash.Annotations.Primitive

import Clash.Cores.Xilinx.GTH as X

-- | A differential input buffer. For more information see:
--
--     https://docs.xilinx.com/r/en-US/ug974-vivado-ultrascale-libraries/IBUFDS
--
ibufds
  :: (KnownDomain dom)
  => Clock dom
  -> Clock dom
  -> Clock dom
ibufds !_ !_ = clockGen
{-# ANN ibufds hasBlackBox #-}
{-# NOINLINE ibufds #-}
{-# ANN ibufds (
    let
      primName = 'ibufds
      clk_p = 1 :: Int
      clk_n = 2 :: Int
    in
      InlineYamlPrimitive [Verilog] [__i|
  BlackBox:
    name: #{primName}
    kind: Declaration
    template: |-
      // ibufds begin
      IBUFDS ~GENSYM[ibufds_inst][0]
      (
        .I(~ARG[#{clk_p}])
      , .IB(~ARG[#{clk_n}])
      , .O(~RESULT)
      );
      // ibufds end
    |] ) #-}
