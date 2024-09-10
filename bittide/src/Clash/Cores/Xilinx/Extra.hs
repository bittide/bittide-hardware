-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Clash.Cores.Xilinx.Extra (
  bufgGt,
  ibufds,
  readDnaPortE2I,
  module GTH,

  -- * Internal
  unsafeBufgGt,
  ibufdsTF,
  unsafeBufgGtTF,
) where

import Clash.Prelude

import Data.String.Interpolate (__i)

import Clash.Annotations.Primitive
import Clash.Backend (Backend)
import Clash.Cores.Xilinx.Unisim.DnaPortE2 (readDnaPortE2)
import Clash.Netlist.Types
import Control.Monad.State (State)
import Data.Text.Prettyprint.Doc.Extra (Doc)
import Text.Show.Pretty (ppShow)

import Clash.Cores.Xilinx.GTH as GTH

import qualified Clash.Netlist.Id as Id
import qualified Clash.Primitives.DSL as DSL
import qualified Prelude as P

-- | Like 'dnaPortE2', but with a hidden clock, reset, and enable
readDnaPortE2I ::
  (HiddenClockResetEnable dom) =>
  -- | DNA value to use in simulation
  BitVector 96 ->
  -- | Extracted DNA value from FPGA. Will take ~100 cycles to become available.
  Signal dom (Maybe (BitVector 96))
readDnaPortE2I = hideClockResetEnable readDnaPortE2

{- | A differential input buffer. For more information see:

    https://docs.xilinx.com/r/en-US/ug974-vivado-ultrascale-libraries/IBUFDS
-}
ibufds :: (KnownDomain dom) => DiffClock dom -> Clock dom
ibufds !_ = clockGen
{-# ANN ibufds hasBlackBox #-}
{-# OPAQUE ibufds #-}
{-# ANN
  ibufds
  ( InlineYamlPrimitive
      [minBound ..]
      [__i|
  BlackBox:
    name: Clash.Cores.Xilinx.Extra.ibufds
    kind: Declaration
    format: Haskell
    templateFunction: Clash.Cores.Xilinx.Extra.ibufdsTF
  |]
  )
  #-}

{- | Template function for 'ibufds'.

TODO: Upstream to @clash-cores@
-}
ibufdsTF :: TemplateFunction
ibufdsTF = TemplateFunction used valid go
 where
  used = [0, 1]
  valid = const True

  go :: (Backend s) => BlackBoxContext -> State s Doc
  go bbCtx
    | [_knownDomain, clk] <- P.map fst (DSL.tInputs bbCtx)
    , DataCon (Product "Clash.Signal.Internal.DiffClock" _ clkTys) _ clkEs <- DSL.eex clk
    , [clkP@(Identifier _ Nothing), clkN@(Identifier _ Nothing)] <- clkEs
    , [clkPTy, clkNTy] <- clkTys =
        do
          instLabel <- Id.makeBasic "ibufds_inst"

          DSL.declarationReturn bbCtx "ibufds_block" $ do
            ibufdsOut <- DSL.declare "ibufds_out" Bit

            let
              compName = "IBUFDS"
              compInps = [("I", Bit), ("IB", Bit)]
              compOuts = [("O", Bit)]
              inps = [("I", DSL.TExpr clkPTy clkP), ("IB", DSL.TExpr clkNTy clkN)]
              outs = [("O", ibufdsOut)]

            DSL.compInBlock compName compInps compOuts
            DSL.instDecl Empty (Id.unsafeMake compName) instLabel [] inps outs

            pure [ibufdsOut]
  go bbCtx = error ("ibufdsTemplate:\n\n" <> ppShow bbCtx)


{- | Clock Buffer Driven by Gigabit Transceiver. For more information see:

    https://docs.xilinx.com/r/en-US/ug974-vivado-ultrascale-libraries/BUFG_GT

The actual divide value is the value provide in @SNat div@ plus 1.
So and @SNat 0@ gives you a division of
-}
bufgGt :: (KnownDomain domIn, KnownDomain domOut, 0 <= div, div <= 7) => SNat div -> Clock domIn -> Reset domIn -> Clock domOut
bufgGt = unsafeBufgGt

unsafeBufgGt :: (KnownDomain domOut) => SNat div -> Clock domIn -> Reset domIn -> Clock domOut
unsafeBufgGt !_ !_ !_ = clockGen
{-# ANN unsafeBufgGt hasBlackBox #-}
{-# OPAQUE unsafeBufgGt #-}
{-# ANN
  unsafeBufgGt
  ( InlineYamlPrimitive
      [Verilog]
      [__i|
  BlackBox:
    name: Clash.Cores.Xilinx.Extra.unsafeBufgGt
    kind: Declaration
    format: Haskell
    templateFunction: Clash.Cores.Xilinx.Extra.unsafeBufgGtTF
  |]
  )
  #-}

{- | Template function for 'unsafeBufgGt'.

TODO: Upstream to @clash-cores@
-}
unsafeBufgGtTF :: TemplateFunction
unsafeBufgGtTF = TemplateFunction used valid go
 where
  used = [1, 2, 3]
  valid = const True

  go :: (Backend s) => BlackBoxContext -> State s Doc
  go bbCtx
    | [_knownDomOut, div0, clk, clr] <- P.map fst (DSL.tInputs bbCtx)
    =
        do
          instLabel <- Id.makeBasic "bufgGt_inst"

          DSL.declarationReturn bbCtx "bufgGt_block" $ do
            bufgGtOut <- DSL.declare "bufgGt_out" Bit
            -- div1 <- DSL.pureToBVResized "div" 3 div0 -- TODO resize is currently VHDL only
            let
              compName = "BUFG_GT"
              compInps = [("I", Bit), ("CLR", Bit), ("DIV", BitVector 3), ("CE", Bit), ("CEMASK", Bit), ("CLRMASK", Bit)]
              compOuts = [("O", Bit)]
              inps = [("I", clk), ("CLR", clr), ("DIV", div0), ("CE", DSL.High), ("CEMASK", DSL.Low), ("CLRMASK", DSL.Low)]
              outs = [("O", bufgGtOut)]

            DSL.compInBlock compName compInps compOuts
            DSL.instDecl Empty (Id.unsafeMake compName) instLabel [] inps outs

            pure [bufgGtOut]
  go bbCtx = error ("bufgGunsafeBTemplate:\n\n" <> ppShow bbCtx)
