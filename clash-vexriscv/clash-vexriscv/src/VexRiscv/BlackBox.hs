-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_HADDOCK hide #-}

module VexRiscv.BlackBox where

import Prelude

import Control.Monad.State (State)
import Data.List.Infinite (Infinite (..), (...))
import Data.String (fromString)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc.Extra (Doc)
import GHC.Stack (HasCallStack)
import Paths_clash_vexriscv (getDataFileName)
import System.IO.Unsafe (unsafePerformIO)
import Text.Show.Pretty (ppShow)

import Clash.Backend (Backend)
import Clash.Netlist.Types (BlackBox (BBFunction), BlackBoxContext, TemplateFunction (..))

import qualified Clash.Netlist.BlackBox.Types as N
import qualified Clash.Netlist.Id as Id
import qualified Clash.Netlist.Types as N
import qualified Clash.Primitives.DSL as DSL

listToTup5 :: (HasCallStack) => [a] -> (a, a, a, a, a)
listToTup5 [a, b, c, d, e] = (a, b, c, d, e)
listToTup5 _ = error "listToTup5: list must have 5 elements"

listToTup3 :: (HasCallStack) => [a] -> (a, a, a)
listToTup3 [a, b, c] = (a, b, c)
listToTup3 _ = error "listToTup3: list must have 3 elements"

vexRiscvBBF :: (HasCallStack) => N.BlackBoxFunction
vexRiscvBBF _isD _primName _args _resTys = pure $ Right (bbMeta, bb)
 where
  bbMeta =
    N.emptyBlackBoxMeta
      { N.bbKind = N.TDecl
      , N.bbIncludes = [(("VexRiscv", "v"), BBFunction (show 'vexRiscvVerilogTF) 0 vexRiscvVerilogTF)]
      }

  bb :: N.BlackBox
  bb = N.BBFunction (show 'vexRiscvTF) 0 vexRiscvTF

vexRiscvVerilogTF :: TemplateFunction
vexRiscvVerilogTF = TemplateFunction [] (const True) $ \_bbCtx -> do
  -- Note that we cannot make this file an argument to the black box, as we
  -- this file is also used by the (hardcoded) part of clash-vexriscv that
  -- generates the C++ and links against it at compile time.
  pure $ fromString $ unsafePerformIO $ readFile =<< getDataFileName "example-cpu/VexRiscv.v"

vexRiscvTF :: TemplateFunction
vexRiscvTF =
  let _hasCallStack :< _knownDomain :< _dumpVcd :< clk :< rst :< cpuIn :< jtagIn :< _ = (0 ...)
   in TemplateFunction [clk, rst, cpuIn, jtagIn] (const True) vexRiscvTF#

vexRiscvTF# :: (Backend backend) => BlackBoxContext -> State backend Doc
vexRiscvTF# bbCtx
  | [_hasCallStack, _knownDomain, _dumpVcd, clk, rst, cpuIn, jtagIn] <- map fst (DSL.tInputs bbCtx)
  , [outTy@(N.Product _ _ [cpuOutTy, jtagOutTy])] <- map snd (N.bbResults bbCtx)
  , N.Product _ _ [iWishboneM2Sty, dWishboneM2Sty, ndmresetTy, stoptimeTy] <- cpuOutTy
  , N.Product _ _ [adrTy, datMosiTy, selTy, _lockTy, cycTy, stbTy, weTy, ctiTy, bteTy] <- iWishboneM2Sty
  , tdoTy <- jtagOutTy = do
      let
        compName :: Text
        compName = "VexRiscv"

      instName <- Id.make (compName <> "_inst")
      DSL.declarationReturn bbCtx (compName <> "_block") $ do
        ( timerInterrupt
          , externalInterrupt
          , softwareInterrupt
          , iBusWbS2M
          , dBusWbS2M
          ) <-
          listToTup5 <$> DSL.deconstructProduct cpuIn ["timerInt", "extInt", "softInt", "iBusWbS2M", "dBusWbS2M"]

        ( iBusWishbone_DAT_MISO
          , iBusWishbone_ACK
          , iBusWishbone_ERR
          , _iBusWishbone_STL
          , _iBusWishbone_RTY
          ) <-
          listToTup5 <$> DSL.deconstructProduct iBusWbS2M ["i_rdata", "i_ack", "i_err", "i_stall", "i_retry"]

        ( dBusWishbone_DAT_MISO
          , dBusWishbone_ACK
          , dBusWishbone_ERR
          , _dBusWishbone_STL
          , _dBusWishbone_RTY
          ) <-
          listToTup5 <$> DSL.deconstructProduct dBusWbS2M ["d_rdata", "d_ack", "d_err", "d_stall", "d_retry"]

        ( tck
          , tms
          , tdi
          ) <-
          listToTup3 <$> DSL.deconstructProduct jtagIn ["tck", "tms", "tdi"]

        iBusWishbone_CYC <- DSL.declare "i_cyc" cycTy
        iBusWishbone_STB <- DSL.declare "i_stb" stbTy
        iBusWishbone_WE <- DSL.declare "i_we" weTy
        iBusWishbone_ADR <- DSL.declare "i_adr" adrTy
        iBusWishbone_DAT_MOSI <- DSL.declare "i_dat_mosi" datMosiTy
        iBusWishbone_SEL <- DSL.declare "i_sel" selTy
        iBusWishbone_CTI <- DSL.declare "i_cti" ctiTy
        iBusWishbone_BTE <- DSL.declare "i_bte" bteTy

        dBusWishbone_CYC <- DSL.declare "d_cyc" cycTy
        dBusWishbone_STB <- DSL.declare "d_stb" stbTy
        dBusWishbone_WE <- DSL.declare "d_we" weTy
        dBusWishbone_ADR <- DSL.declare "d_adr" adrTy
        dBusWishbone_DAT_MOSI <- DSL.declare "d_dat_mosi" datMosiTy
        dBusWishbone_SEL <- DSL.declare "d_sel" selTy
        dBusWishbone_CTI <- DSL.declare "d_cti" ctiTy
        dBusWishbone_BTE <- DSL.declare "d_bte" bteTy

        tdo <- DSL.declare "tdo" tdoTy

        ndmreset <- DSL.declare "ndmreset" ndmresetTy
        stoptime <- DSL.declare "stoptime" stoptimeTy

        let
          generics = []

          inps :: [(Text, DSL.TExpr)]
          inps =
            [ ("clk", clk)
            , ("reset", rst)
            , ("timerInterrupt", timerInterrupt)
            , ("externalInterrupt", externalInterrupt)
            , ("softwareInterrupt", softwareInterrupt)
            , ("iBusWishbone_DAT_MISO", iBusWishbone_DAT_MISO)
            , ("iBusWishbone_ACK", iBusWishbone_ACK)
            , ("iBusWishbone_ERR", iBusWishbone_ERR)
            , ("dBusWishbone_DAT_MISO", dBusWishbone_DAT_MISO)
            , ("dBusWishbone_ACK", dBusWishbone_ACK)
            , ("dBusWishbone_ERR", dBusWishbone_ERR)
            , ("jtag_tms", tms)
            , ("jtag_tdi", tdi)
            , ("jtag_tck", tck)
            ]

          outs :: [(Text, DSL.TExpr)]
          outs =
            [ ("iBusWishbone_CYC", iBusWishbone_CYC)
            , ("iBusWishbone_STB", iBusWishbone_STB)
            , ("iBusWishbone_WE", iBusWishbone_WE)
            , ("iBusWishbone_ADR", iBusWishbone_ADR)
            , ("iBusWishbone_DAT_MOSI", iBusWishbone_DAT_MOSI)
            , ("iBusWishbone_SEL", iBusWishbone_SEL)
            , ("iBusWishbone_CTI", iBusWishbone_CTI)
            , ("iBusWishbone_BTE", iBusWishbone_BTE)
            , ("dBusWishbone_CYC", dBusWishbone_CYC)
            , ("dBusWishbone_STB", dBusWishbone_STB)
            , ("dBusWishbone_WE", dBusWishbone_WE)
            , ("dBusWishbone_ADR", dBusWishbone_ADR)
            , ("dBusWishbone_DAT_MOSI", dBusWishbone_DAT_MOSI)
            , ("dBusWishbone_SEL", dBusWishbone_SEL)
            , ("dBusWishbone_CTI", dBusWishbone_CTI)
            , ("dBusWishbone_BTE", dBusWishbone_BTE)
            , ("jtag_tdo", tdo)
            , ("ndmreset", ndmreset)
            , ("stoptime", stoptime)
            ]

        DSL.instDecl N.Empty (Id.unsafeMake compName) instName generics inps outs

        let
          iWishboneOut =
            DSL.constructProduct
              iWishboneM2Sty
              [ iBusWishbone_ADR
              , iBusWishbone_DAT_MOSI
              , iBusWishbone_SEL
              , DSL.litTExpr (DSL.B False)
              , iBusWishbone_CYC
              , iBusWishbone_STB
              , iBusWishbone_WE
              , iBusWishbone_CTI
              , iBusWishbone_BTE
              ]

          dWishboneOut =
            DSL.constructProduct
              dWishboneM2Sty
              [ dBusWishbone_ADR
              , dBusWishbone_DAT_MOSI
              , dBusWishbone_SEL
              , DSL.litTExpr (DSL.B False)
              , dBusWishbone_CYC
              , dBusWishbone_STB
              , dBusWishbone_WE
              , dBusWishbone_CTI
              , dBusWishbone_BTE
              ]

        pure
          [ DSL.constructProduct
              outTy
              [ DSL.constructProduct cpuOutTy [iWishboneOut, dWishboneOut, ndmreset, stoptime]
              , tdo
              ]
          ]
vexRiscvTF# bbCtx = error (ppShow bbCtx)
