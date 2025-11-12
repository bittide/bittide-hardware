-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}

module Clash.Cores.Xilinx.Gth.BlackBoxes where

import Prelude

import Control.Monad.State (State)
import Data.Text.Prettyprint.Doc.Extra (Doc)
import GHC.Stack (HasCallStack)

import Clash.Backend (Backend)
import Clash.Netlist.BlackBox.Types (BlackBoxFunction, emptyBlackBoxMeta)
import Clash.Netlist.Types

import qualified Clash.Netlist.BlackBox.Types as N
import qualified Clash.Netlist.Id as Id
import qualified Clash.Netlist.Types as N
import qualified Clash.Primitives.DSL as DSL

ibufds_gte3BBF :: (HasCallStack) => BlackBoxFunction
ibufds_gte3BBF _isD _primName _args _resTys =
  let
    bbMeta = emptyBlackBoxMeta{N.bbKind = N.TDecl}

    bb :: BlackBox
    bb = BBFunction (show 'ibufds_gte3TF) 0 ibufds_gte3TF
   in
    pure $ Right (bbMeta, bb)

ibufds_gte3TF :: (HasCallStack) => TemplateFunction
ibufds_gte3TF =
  TemplateFunction
    [0, 1]
    (const True)
    ibufds_gte3BBTF

ibufds_gte3BBTF ::
  (Backend s) =>
  BlackBoxContext ->
  State s Doc
ibufds_gte3BBTF bbCtx
  | [_knownDomain, clk] <- map fst (DSL.tInputs bbCtx)
  , DataCon (Product "Clash.Signal.Internal.DiffClock" _ clkTys) _ clkEs <- DSL.eex clk
  , [clkP@(Identifier _ Nothing), clkN@(Identifier _ Nothing)] <- clkEs
  , [clkPTy, clkNTy] <- clkTys =
      do
        ibufds_gte3InstName <- Id.makeBasic "ibufds_gte3_inst"

        let
          inps =
            [ ("I", DSL.TExpr clkPTy clkP)
            , ("IB", DSL.TExpr clkNTy clkN)
            , -- Tied off:
              ("CEB", DSL.Low)
            ]

          compOuts =
            [ ("O", Bit)
            ]
          attrs =
            [ ("REFCLK_EN_TX_PATH", DSL.Low)
            , ("REFCLK_HROW_CK_SEL", DSL.bvLit 2 0b10)
            , ("REFCLK_ICNTL_RX", DSL.bvLit 2 0b00)
            ]
          ibufds_gte3Name = "IBUFDS_GTE3"
        DSL.declarationReturn bbCtx "ibufds_gte3_inst_block" $ do
          outs <- mapM (uncurry DSL.declare) compOuts
          DSL.instDecl
            N.Empty
            (Id.unsafeMake ibufds_gte3Name)
            ibufds_gte3InstName
            attrs
            inps
            (zip (fst <$> compOuts) outs)
          pure outs
ibufds_gte3BBTF bbCtx = error ("ibufds_gte3BBTF, bad bbCtx: " <> show bbCtx)
