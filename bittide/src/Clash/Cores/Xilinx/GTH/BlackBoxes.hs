-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Clash.Cores.Xilinx.GTH.BlackBoxes where

import Prelude

import Control.Monad.State (State)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc.Extra (Doc)
import GHC.Stack (HasCallStack)
import Text.Show.Pretty (ppShow)

import Clash.Backend (Backend)
import Clash.Netlist.BlackBox.Types (BlackBoxFunction, emptyBlackBoxMeta)
import Clash.Netlist.BlackBox.Util (exprToString)
import Clash.Netlist.Types

import Clash.Cores.Xilinx.Internal (
  BraceTcl (..),
  IpConfig (properties),
  TclPurpose (..),
  defIpConfig,
  property,
  renderTcl,
 )

import qualified Clash.Netlist.BlackBox.Types as N
import qualified Clash.Netlist.Id as Id
import qualified Clash.Netlist.Types as N
import qualified Clash.Primitives.DSL as DSL

gthCoreBBF :: (HasCallStack) => BlackBoxFunction
gthCoreBBF _isD _primName _args _resTys = pure $ Right (bbMeta, bb)
 where
  bbMeta =
    emptyBlackBoxMeta
      { N.bbKind = N.TDecl
      , N.bbIncludes =
          [
            ( ("gth", "clash.tcl")
            , BBFunction (show 'gthCoreTclTF) 0 gthCoreTclTF
            )
          ]
      }

  bb :: BlackBox
  bb = BBFunction (show 'gthCoreTF) 0 gthCoreTF

-- | Number of constraints gthCore has
nConstraints :: Int
nConstraints = 6

-- | Number of "parameters" (non Signal arguments) gthCore has
nParamArgs :: Int
nParamArgs = 3

-- | Instantiate IP generated with 'gthCoreTclTF'
gthCoreTF :: (HasCallStack) => TemplateFunction
gthCoreTF =
  TemplateFunction
    [0 .. 15]
    (const True)
    gthCoreBBTF

gthCoreBBTF ::
  (Backend s) =>
  BlackBoxContext ->
  State s Doc
gthCoreBBTF bbCtx
  | args@[ _gthrxn_in -- " ::: Signal rxS (BitVector ChansUsed)
          , _gthrxp_in -- " ::: Signal rxS (BitVector ChansUsed)
          , gtwiz_reset_clk_freerun_in -- " ::: Clock freerun
          , _gtwiz_reset_all_in -- " ::: Reset freerun
          , _gtwiz_reset_rx_datapath_in -- " ::: Reset freerun
          , _gtwiz_userdata_tx_in -- " ::: Signal txUser2 (BitVector (ChansUsed*TX_DATA_WIDTH))
          , _txctrl2_in -- " ::: Signal txUser2 (BitVector (ChansUsed*TX_DATA_WIDTH/8))
          , _gtrefclk0_in -- " ::: Clock refclk0
          ] <-
      drop (nConstraints + nParamArgs) $ map fst (DSL.tInputs bbCtx)
  , [tResult] <- map DSL.ety (DSL.tResults bbCtx)
  , [gthCoreName] <- N.bbQsysIncName bbCtx =
      do
        gthCoreInstName <- Id.makeBasic "gthcore_inst"

        let
          chansUsed = 1
          tX_DATA_WIDTH = 64
          rX_DATA_WIDTH = tX_DATA_WIDTH
          compInps =
            [ ("gthrxn_in", N.BitVector chansUsed)
            , ("gthrxp_in", N.BitVector chansUsed)
            , ("gtwiz_reset_clk_freerun_in", N.Clock "freerun")
            , ("gtwiz_reset_all_in", N.Reset "freerun")
            , ("gtwiz_reset_rx_datapath_in", N.Reset "freerun")
            , ("gtwiz_userdata_tx_in", N.BitVector (chansUsed * tX_DATA_WIDTH))
            , ("txctrl2_in", N.BitVector (chansUsed * (tX_DATA_WIDTH `div` 8)))
            , -- , ("gtrefclk00_in",  N.Clock "refclk00" )
              ("gtrefclk0_in", N.Clock "refclk0")
            ]
              <> map (fmap DSL.ety) otherInps

          otherInps =
            [ ("drpclk_in", gtwiz_reset_clk_freerun_in)
            , ("txctrl0_in", DSL.bvLit 16 0)
            , ("txctrl1_in", DSL.bvLit 16 0)
            , ("gtwiz_reset_tx_pll_and_datapath_in", DSL.bvLit 1 0)
            , ("gtwiz_reset_tx_datapath_in", DSL.bvLit 1 0)
            , ("gtwiz_reset_rx_pll_and_datapath_in", DSL.bvLit 1 0)
            , ("tx8b10ben_in", DSL.bvLit 1 1)
            , ("rx8b10ben_in", DSL.bvLit 1 1)
            , ("gtwiz_userclk_tx_reset_in", DSL.bvLit 1 0)
            , ("gtwiz_userclk_rx_reset_in", DSL.bvLit 1 0)
            , ("rxcommadeten_in", DSL.bvLit 1 1)
            , ("rxmcommaalignen_in", DSL.bvLit 1 1)
            , ("rxpcommaalignen_in", DSL.bvLit 1 1)
            ]
          compOuts =
            [ ("gthtxn_out", N.BitVector chansUsed)
            , ("gthtxp_out", N.BitVector chansUsed)
            , ("gtwiz_userclk_tx_usrclk2_out", N.Clock "txUser2")
            , ("gtwiz_userclk_rx_usrclk2_out", N.Clock "rxUser2")
            , ("gtwiz_userdata_rx_out", N.BitVector (chansUsed * rX_DATA_WIDTH))
            , ("gtwiz_reset_tx_done_out", N.BitVector 1)
            , ("gtwiz_reset_rx_done_out", N.BitVector 1)
            , ("gtwiz_userclk_tx_active_out", N.BitVector 1)
            , ("rxctrl0_out", N.BitVector 16)
            , ("rxctrl1_out", N.BitVector 16)
            , ("rxctrl2_out", N.BitVector 8)
            , ("rxctrl3_out", N.BitVector 8)
            ]

        DSL.declarationReturn bbCtx "gthCore_inst_block" $ do
          DSL.compInBlock gthCoreName compInps compOuts

          let inps = zip (fst <$> compInps) args <> otherInps
          outs <- mapM (uncurry DSL.declare) compOuts
          DSL.instDecl
            N.Empty
            (Id.unsafeMake gthCoreName)
            gthCoreInstName
            []
            inps
            (zip (fst <$> compOuts) outs)
          pure [DSL.constructProduct tResult outs]
gthCoreBBTF bbCtx = error ("gthCoreBBTF, bad bbCtx:\n\n" <> ppShow bbCtx)

{- | Renders Tcl file conforming to the /Clash\<->Tcl API/, creating the Xilinx
IP with @create_ip@
-}
gthCoreTclTF :: (HasCallStack) => TemplateFunction
gthCoreTclTF =
  TemplateFunction
    (fmap (nConstraints +) [0, 1, 2]) -- used arguments
    (const True)
    gthCoreTclBBTF

gthCoreTclBBTF ::
  (Backend s) =>
  BlackBoxContext ->
  State s Doc
gthCoreTclBBTF bbCtx
  | [gthCoreName] <- N.bbQsysIncName bbCtx
  , (exprToString -> Just channelNm, _, _) : (exprToString -> Just refClkNm, _, _)
      : (exprToString -> Just txOutClkSrc, _, _)
      : _ <-
      drop nConstraints (N.bbInputs bbCtx) =
      pure (renderTcl [IpConfigPurpose $ ipConfig gthCoreName channelNm refClkNm txOutClkSrc])
 where
  ipConfig nm channelNm refClkNm txOutClkSrc =
    (defIpConfig "gtwizard_ultrascale " "1.7" nm)
      { properties = props channelNm refClkNm txOutClkSrc
      }

  props channelNm refClkNm txOutClkSrc =
    [ property @Text "CHANNEL_ENABLE" (fromString channelNm)
    , property @Text "LOCATE_COMMON" "CORE"
    , property @Text "LOCATE_IN_SYSTEM_IBERT_CORE" "NONE"
    , property @Text "LOCATE_RESET_CONTROLLER" "CORE"
    , property @Text "LOCATE_RX_BUFFER_BYPASS_CONTROLLER" "CORE"
    , property @Text "LOCATE_RX_USER_CLOCKING" "CORE"
    , property @Text "LOCATE_TX_BUFFER_BYPASS_CONTROLLER" "CORE"
    , property @Text "LOCATE_TX_USER_CLOCKING" "CORE"
    , property @Text "LOCATE_USER_DATA_WIDTH_SIZING" "CORE"
    , property @Text "FREERUN_FREQUENCY" "125.0"
    , property @Text "RX_REFCLK_FREQUENCY" "250"
    , -- .X_REFCLK_SOURCE syntax: X0Yn clk[0,1]([+,-]q
      property
        "RX_REFCLK_SOURCE"
        (BraceTcl @Text $ fromString $ unwords [channelNm, refClkNm])
    , property @Text "RX_DATA_DECODING" "8B10B"
    , property @Text "RX_INT_DATA_WIDTH" "40"
    , -- , property @Text "RX_JTOL_FC"                            "5.9988002"
      property @Text "RX_LINE_RATE" "10"
    , -- , property @Text "RX_MASTER_CHANNEL"                     "X0Y10"
      property @Text "RX_OUTCLK_SOURCE" "RXOUTCLKPMA"
    , property @Text "RX_PLL_TYPE" "CPLL"
    , property @Text "RX_PPM_OFFSET" "200"
    , property @Text "RX_USER_DATA_WIDTH" "64"
    , property @Text "RX_EQ_MODE" "LPM"
    , property @Text "RX_COMMA_PRESET" "K28.5"
    , property @Bool "RX_COMMA_P_ENABLE" True
    , property @Bool "RX_COMMA_M_ENABLE" True
    , -- , property @Text "RX_COMMA_P_VAL" "0101111100"
      -- , property @Text "RX_COMMA_M_VAL" "1010000011"
      -- , property @Text "RX_COMMA_MASK" "1111111111"
      property @Bool "RX_COMMA_SHOW_REALIGN_ENABLE" False
    , property @Text "TX_REFCLK_FREQUENCY" "250"
    , property
        "TX_REFCLK_SOURCE"
        (BraceTcl @Text $ fromString $ unwords [channelNm, refClkNm])
    , property @Text "TX_OUTCLK_SOURCE" (fromString txOutClkSrc)
    , property @Text "TX_DATA_ENCODING" "8B10B"
    , property @Text "TX_INT_DATA_WIDTH" "40"
    , property @Text "TX_LINE_RATE" "10"
    , -- , property @Text "TX_MASTER_CHANNEL"                     "X0Y10"
      property @Text "TX_PLL_TYPE" "CPLL"
    , property @Text "TXPROGDIV_FREQ_SOURCE" "CPLL"
    , property @Text "TX_USER_DATA_WIDTH" "64"
    ]
gthCoreTclBBTF bbCtx = error ("gthCoreTclBBTF, bad bbCtx:\n\n" <> ppShow bbCtx)

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
