-- SPDX-FileCopyrightText: 2023-2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module Bittide.Transceiver where

import Clash.Explicit.Prelude
import Clash.Prelude (withClockResetEnable)

import Clash.Cores.Xilinx.GTH
import Clash.Cores.Xilinx.Ila (IlaConfig(advancedTriggers, depth, stages), ilaConfig, ila, Depth(D1024))
import Clash.Cores.Xilinx.VIO (vioProbe)
import Clash.Cores.Xilinx.Xpm.Cdc.ArraySingle (xpmCdcArraySingle)
import Clash.Cores.Xilinx.Xpm.Cdc.Handshake.Extra (xpmCdcMaybeLossy)
import Clash.Cores.Xilinx.Xpm.Cdc.Single
import Clash.Explicit.Reset.Extra (Asserted(Asserted), delayReset, xpmResetSynchronizer)
import Control.Monad (when)
import Data.Maybe (isNothing, fromMaybe)

import qualified Bittide.Transceiver.Cdc as Cdc
import qualified Bittide.Transceiver.Comma as Comma
import qualified Bittide.Transceiver.Prbs as Prbs
import qualified Bittide.Transceiver.ResetManager as ResetManager
import qualified Bittide.Transceiver.WordAlign as WordAlign
import Bittide.Transceiver.ResetManager (IndexMs)
import Bittide.ElasticBuffer (sticky)
import Bittide.Arithmetic.Time (Milliseconds, trueFor)

-- | Meta information send along with the PRBS and alignment symbols. See module
-- documentation for more information.
data Meta = Meta
  { prbsOk :: Bool
  -- ^ PRBS data is valid
  , lastPrbsWord :: Bool
  -- ^ Next word will be user data
  , fpgaIndex :: Index 8
  , transceiverIndex :: Index 7
  }
  deriving (Generic, NFDataX, BitPack)

metaMagic :: BitVector 6
metaMagic = 0b11_0001

emptyMeta :: Meta
emptyMeta = Meta
  { prbsOk = False
  , lastPrbsWord = False
  , fpgaIndex = 0
  , transceiverIndex = 0
  }

data TransceiverOptions = TransceiverOptions
  { debugVio :: Bool
  -- ^ Instantiate a debug VIOs
  , debugIla :: Bool
  -- ^ Instantiate a debug ILAs
  }

defTransceiverOptions :: TransceiverOptions
defTransceiverOptions = TransceiverOptions
  { debugVio = False
  , debugIla = False
  }

-- | Careful: the domains for each transceiver are different, even if their
-- types say otherwise.
data TransceiverOutputs n tx rx txS freeclk = TransceiverOutputs
  { txClocks :: Vec n (Clock tx)
  -- ^ Transmit clocks. Can only be used if associated 'linkUps' is asserted.
  --
  -- TODO: Export more signals such that this clock can be used earlier.
  , rxClocks :: Vec n (Clock rx)
  -- ^ Receive clocks, recovered from the incoming data stream. Can only be used
  -- if associated 'linkUps' is asserted.
  --
  -- TODO: Export more signals such that these clocks can be used earlier.
  , txPs :: Vec n (Signal txS (BitVector 1))
  -- ^ Transmit data (and implicitly a clock), positive
  , txNs :: Vec n (Signal txS (BitVector 1))
  -- ^ Transmit data (and implicitly a clock), negative
  , linkUps :: Vec n (Signal freeclk Bool)
  -- ^ True if the link is considered stable. See 'PRBS.tracker'.
  , stats :: Vec n (Signal freeclk ResetManager.Statistics)
  -- ^ Statistics exported by 'ResetManager.resetManager'
  }

data TransceiverOutput tx rx txS freeclk = TransceiverOutput
  { txClock :: Clock tx
  -- ^ Transmit clock. Can only be used after 'linkUp' is asserted.
  --
  -- TODO: Export more signals such that this clock can be used earlier.
  , rxClock :: Clock rx
  -- ^ Receive clock, recovered from the incoming data stream. Can only be used
  -- after 'linkUp' is asserted.
  --
  -- TODO: Export more signals such that this clock can be used earlier.
  , txP :: Signal txS (BitVector 1)
  -- ^ Transmit data (and implicitly a clock), positive
  , txN :: Signal txS (BitVector 1)
  -- ^ Transmit data (and implicitly a clock), negative
  , linkUp :: Signal freeclk Bool
  -- ^ True if the link is considered stable. See 'Prbs.Tracker'.
  , stats :: Signal freeclk ResetManager.Statistics
  -- ^ Statistics exported by 'ResetManager.resetManager'
  }

transceiverPrbsN ::
  forall tx rx refclk freeclk txS rxS chansUsed .
  ( KnownNat chansUsed
  , HasSynchronousReset tx
  , HasDefinedInitialValues tx

  , HasSynchronousReset rx
  , HasDefinedInitialValues rx

  , HasSynchronousReset freeclk
  , HasDefinedInitialValues freeclk
  ) =>
  TransceiverOptions ->

  Clock refclk ->
  Clock freeclk ->

  Reset freeclk ->

  Signal freeclk (Index 8) ->

  Vec chansUsed String ->
  Vec chansUsed String ->

  Vec chansUsed (Signal rxS (BitVector 1)) ->
  Vec chansUsed (Signal rxS (BitVector 1)) ->

  TransceiverOutputs chansUsed tx rx txS freeclk
transceiverPrbsN opts refclk freeclk rst fpgaIndex chanNms clkPaths rxns rxps = TransceiverOutputs
  { txClocks = map (.txClock) outputs
  , rxClocks = map (.rxClock) outputs
  , txPs     = map (.txP)     outputs
  , txNs     = map (.txN)     outputs
  , linkUps  = map (.linkUp)  outputs
  , stats    = map (.stats)   outputs
  }
 where
  transIndices = iterateI (+1) 0
  outputs = zipWith5 (transceiverPrbs opts refclk freeclk rst fpgaIndex) transIndices chanNms clkPaths rxns rxps

transceiverPrbs ::
  forall tx rx refclk freeclk txS rxS .
  ( HasSynchronousReset tx
  , HasDefinedInitialValues tx

  , HasSynchronousReset rx
  , HasDefinedInitialValues rx

  , HasSynchronousReset freeclk
  , HasDefinedInitialValues freeclk
  ) =>
  TransceiverOptions ->

  Clock refclk ->
  Clock freeclk ->

  Reset freeclk ->  -- ^ rst all

  Signal freeclk (Index 8) ->

  Index 7 ->

  String -> -- ^ channel, example X0Y18
  String -> -- ^ clkPath, example clk0-2

  Signal rxS (BitVector 1) ->
  Signal rxS (BitVector 1) ->

  TransceiverOutput tx rx txS freeclk
transceiverPrbs opts gtrefclk freeclk rst_all_in fpgaIndex transIndex chan clkPath rxn rxp =
           when opts.debugVio debugVio
  `hwSeqX` when opts.debugVio debugIla
  `hwSeqX` result
 where
  debugIla :: Signal freeclk ()
  debugIla = ila
    ((ilaConfig $
         "ila_probe_fpgaIndex"
      :> "ila_probe_transIndex"
      :> "ila_probe_rx_data0"
      :> "ila_probe_alignedRxData0"
      :> "ila_probe_gtwiz_userdata_tx_in"
      :> "ila_probe_reset_rx_done"
      :> "ila_probe_reset_tx_done"
      :> "ila_probe_rst_all_in"
      :> "ila_probe_alignError"
      :> "ila_probe_prbsErrors"
      :> "ila_probe_alignedAlignBits"
      :> "ila_probe_alignedMetaBits"
      :> "ila_probe_rxCtrl0"
      :> "ila_probe_rxCtrl1"
      :> "ila_probe_rxCtrl2"
      :> "ila_probe_rxCtrl3"
      :> "ila_probe_prbsOk"
      :> "ila_probe_prbsOkDelayed"
      :> "ila_probe_rst_all"
      :> "ila_probe_rst_rx"
      :> "ila_probe_rxReset"
      :> "ila_probe_txStimRst"
      :> "ila_probe_metaTx"
      :> "ila_probe_failAfterUp"
      :> "ila_probe_timeout"
      :> "ila_probe_linkUp"
      :> "ila_probe_lastTxFree"
      :> "capture"
      :> "trigger"
      :> Nil) { advancedTriggers = True, stages = 1, depth = D1024 })
    freeclk
    fpgaIndex
    (pure transIndex :: Signal freeclk (Index 7))
    (xpmCdcArraySingle rx_clk freeclk rx_data0)
    (xpmCdcArraySingle rx_clk freeclk alignedRxData0)
    (xpmCdcArraySingle tx_clk freeclk gtwiz_userdata_tx_in)
    (xpmCdcArraySingle rx_clk freeclk reset_rx_done)
    (xpmCdcArraySingle tx_clk freeclk reset_tx_done)
    (unsafeToActiveHigh rst_all_in)
    (xpmCdcArraySingle rx_clk freeclk alignError)
    (xpmCdcArraySingle rx_clk freeclk prbsErrors)
    (xpmCdcArraySingle rx_clk freeclk alignedAlignBits)
    (xpmCdcArraySingle rx_clk freeclk (prettifyMetaBits <$> alignedMetaBits))
    (xpmCdcArraySingle rx_clk freeclk rxCtrl0)
    (xpmCdcArraySingle rx_clk freeclk rxCtrl1)
    (xpmCdcArraySingle rx_clk freeclk rxCtrl2)
    (xpmCdcArraySingle rx_clk freeclk rxCtrl3)
    (xpmCdcSingle rx_clk freeclk prbsOk)
    (xpmCdcSingle rx_clk freeclk prbsOkDelayed)
    (unsafeToActiveHigh rst_all)
    (unsafeToActiveHigh rst_rx)
    (xpmCdcSingle rx_clk freeclk $ unsafeToActiveHigh rxReset)
    (xpmCdcSingle tx_clk freeclk $ unsafeToActiveHigh txStimRst)
    (xpmCdcArraySingle tx_clk freeclk (prettifyMetaBits . pack <$> metaTx))
    failAfterUp
    timeout
    linkUp
    lastTxFree
    (pure True :: Signal freeclk Bool) -- capture
    (failAfterUp .||. (fmap not linkUp .&&. timeout) .||. lastTxFree) -- trigger

  prettifyMetaBits :: BitVector 8 -> BitVector 12
  prettifyMetaBits bv = pack $
    let meta = unpack @Meta bv in
    (low, low, meta.prbsOk, meta.lastPrbsWord, low, meta.fpgaIndex, low, meta.transceiverIndex)

  debugVio :: Signal freeclk ()
  debugVio = vioProbe
    (  "probe_link_up"
    :> "probe_alignedAlignBits"
    :> "probe_alignedMetaBits"
    :> "probe_stats_txRetries"
    :> "probe_stats_rxRetries"
    :> "probe_stats_rxFullRetries"
    :> "probe_stats_failAfterUps"
    :> Nil )
    Nil
    ()
    freeclk
    linkUp
    (xpmCdcMaybeLossy rx_clk freeclk (Just <$> alignedAlignBits))
    (xpmCdcMaybeLossy rx_clk freeclk (Just <$> alignedMetaBits))
    ((.txRetries) <$> stats)
    ((.rxRetries) <$> stats)
    ((.rxFullRetries) <$> stats)
    ((.failAfterUps) <$> stats)

  result = TransceiverOutput
    { txClock = tx_clk
    , rxClock = rx_clk
    , txP = txp
    , txN = txn
    , linkUp = linkUp
    , stats = stats
    }

  failAfterUp = isFalling freeclk rst_all_in enableGen False linkUp
  counter = register freeclk rst_all_in enableGen (0 :: IndexMs freeclk 10_000) (satSucc SatBound <$> counter)
  timeout = counter .==. pure maxBound

  linkUp =
         withLockTxFree userDataTx
    .&&. withLockRxFree userDataRx

  ( txn, txp, tx_clk, rx_clk, rx_data0, reset_tx_done, reset_rx_done, tx_active
   , rxCtrl0, rxCtrl1, rxCtrl2, rxCtrl3
   )
    = gthCore
        @tx @rx @refclk @freeclk @txS @rxS

        chan clkPath
        rxn
        rxp

        freeclk -- gtwiz_reset_clk_freerun_in

        (delayReset Asserted freeclk rst_all {-* filter glitches *-})
        noReset -- gtwiz_reset_tx_pll_and_datapath_in
        noReset -- gtwiz_reset_tx_datapath_in
        noReset -- gtwiz_reset_rx_pll_and_datapath_in
        (delayReset Asserted freeclk rst_rx {-* filter glitches *-}) -- gtwiz_reset_rx_datapath_in
        gtwiz_userdata_tx_in
        txctrl
        freeclk -- drpclk_in
        gtrefclk -- gtrefclk0_in

  prbsConfig = Prbs.conf31 @48

  (commas, txctrl) = Comma.generator Comma.defCycles tx_clk txStimRst
  commasDone = isNothing <$> commas
  prbs = Prbs.generator tx_clk (unsafeFromActiveLow commasDone) enableGen prbsConfig
  prbsWithMeta = WordAlign.joinMsbs @8 <$> fmap pack metaTx <*> prbs
  prbsWithMetaAndAlign = WordAlign.joinMsbs @8 WordAlign.alignSymbol <$> prbsWithMeta
  gtwiz_userdata_tx_in =
    mux
      userDataTx
      userData
      (fromMaybe <$> prbsWithMetaAndAlign <*> commas)

  userData =
    register tx_clk (unsafeFromActiveLow userDataTx) enableGen (0 :: BitVector 64) (userData + 1)

  rxReset =
    xpmResetSynchronizer Asserted rx_clk rx_clk $
                unsafeFromActiveLow (bitCoerce <$> reset_rx_done)
      `orReset` xpmResetSynchronizer Asserted freeclk rx_clk rst_all_in

  alignedRxData0 :: Signal rx (BitVector 64)
  alignedRxData0 = withClockResetEnable rx_clk rxReset enableGen $
    WordAlign.alignBytesFromMsbs @8 userDataRx rx_data0

  (alignedAlignBits, alignedRxData1) = unbundle $
    WordAlign.splitMsbs @8 @8 <$> alignedRxData0

  (alignedMetaBits, alignedRxData2) = unbundle $
    WordAlign.splitMsbs @8 @7 <$> alignedRxData1

  prbsErrors = Prbs.checker rx_clk rxReset enableGen prbsConfig alignedRxData2
  anyPrbsErrors = prbsErrors ./=. pure 0
  alignError = alignedAlignBits ./=. pure WordAlign.alignSymbol

  prbsOk =
    mux
      userDataRx
      (pure True)
      (Prbs.tracker rx_clk rxReset (anyPrbsErrors .||. alignError))

  prbsOkDelayed = trueFor (SNat @(Milliseconds 500)) rx_clk rxReset prbsOk
  validMeta = mux userDataRx (pure False) prbsOkDelayed

  metaRx = mux validMeta (Just . unpack @Meta <$> alignedMetaBits) (pure Nothing)
  lastRx = maybe False (.lastPrbsWord) <$> metaRx
  prbsOkRx = maybe False (.prbsOk) <$> metaRx

  userDataRx = sticky rx_clk rxReset lastRx
  userDataTx = sticky tx_clk txStimRst lastTx

  prbsOkTx = lastTx .||. withLockRxTx prbsOkDelayed
  lastTxFree = xpmCdcSingle tx_clk freeclk lastTx

  prbsOkRxSticky = sticky rx_clk rxReset prbsOkRx
  lastTx = withLockRxTx prbsOkRxSticky

  metaTx :: Signal tx Meta
  metaTx = Meta
    <$> prbsOkTx
    <*> lastTx
    -- We shouldn't sync with 'xpmCdcArraySingle' here, as the individual bits in
    -- 'fpgaIndex' are related to each other. Still, we know fpgaIndex is basically
    -- a constant so :shrug:.
    <*> xpmCdcArraySingle freeclk tx_clk fpgaIndex
    <*> pure transIndex

  (rst_all, rst_rx, _init_done, stats) =
    ResetManager.resetManager
      freeclk rst_all_in
      (withLockTxFree (pure True))
      (withLockRxFree (pure True))
      (withLockRxFree (prbsOk .||. userDataRx))

  txStimRst = xpmResetSynchronizer Asserted tx_clk tx_clk $
              unsafeFromActiveLow (bitCoerce <$> tx_active)
    `orReset` unsafeFromActiveLow (bitCoerce <$> reset_tx_done)
    `orReset` xpmResetSynchronizer Asserted freeclk tx_clk rst_all_in

  withLockTxFree = Cdc.withLock tx_clk (unpack <$> reset_tx_done) freeclk rst_all_in
  withLockRxFree = Cdc.withLock rx_clk (unpack <$> reset_rx_done) freeclk rst_all_in
  withLockRxTx   = Cdc.withLock rx_clk (unpack <$> reset_rx_done) tx_clk txStimRst

  -- withLockFreeRx = Cdc.withLock freeclk (unsafeToActiveLow rst_all_in) rx_clk rxReset
