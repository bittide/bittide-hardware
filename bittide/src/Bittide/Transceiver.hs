-- SPDX-FileCopyrightText: 2023-2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module Bittide.Transceiver where

import Clash.Explicit.Prelude
import Clash.Prelude (withClockResetEnable)

import Bittide.Transceiver.ResetManager (IndexMs)
import Bittide.ElasticBuffer (sticky)
import Bittide.Arithmetic.Time (Milliseconds, trueForSteps)
import Clash.Class.Counter (countSucc)
import Clash.Cores.Xilinx.GTH
import Clash.Cores.Xilinx.Ila (IlaConfig(advancedTriggers, depth, stages), ilaConfig, ila, Depth(D1024))
import Clash.Cores.Xilinx.VIO (vioProbe)
import Clash.Cores.Xilinx.Xpm.Cdc.ArraySingle (xpmCdcArraySingle)
import Clash.Cores.Xilinx.Xpm.Cdc.Single
import Clash.Explicit.Reset.Extra (Asserted(Asserted), delayReset, xpmResetSynchronizer)
import Control.Monad (when)
import Data.Maybe (isNothing, fromMaybe)
import Data.Proxy

import qualified Bittide.Transceiver.Cdc as Cdc
import qualified Bittide.Transceiver.Comma as Comma
import qualified Bittide.Transceiver.Prbs as Prbs
import qualified Bittide.Transceiver.ResetManager as ResetManager
import qualified Bittide.Transceiver.WordAlign as WordAlign

-- | Meta information send along with the PRBS and alignment symbols. See module
-- documentation for more information.
data Meta = Meta
  { prbsOk :: Bool
  -- ^ PRBS data is valid
  , lastPrbsWord :: Bool
  -- ^ Next word will be user data
  , fpgaIndex :: Unsigned 3
  -- ^ FPGA index to use for debug signals
  , transceiverIndex :: Unsigned 3
  -- ^ Transceiver index to use for debug signals
  }
  deriving (Generic, NFDataX, BitPack)

-- | Insert zeroes such that each of the following are encoded in 4 bits, making
-- them easier to read when formatted as a hex value:
--
--   * prbsOk, lastPrbsWord
--   * fpgaIndex
--   * transceiverIndex
--
-- This is useful for when we don't control formatting (such as when looking at
-- ILA traces).
prettifyMetaBits :: BitVector 8 -> BitVector 12
prettifyMetaBits bv = pack $
  let meta = unpack @Meta bv in
  ( low, low, meta.prbsOk
  , meta.lastPrbsWord
  , low, meta.fpgaIndex
  , low, meta.transceiverIndex
  )

data TransceiverOptions dom = TransceiverOptions
  { debugVio :: Bool
  -- ^ Instantiate a debug VIOs
  , debugIla :: Bool
  -- ^ Instantiate a debug ILAs
  , debugFpgaIndex :: Signal dom (Unsigned 3)
  -- ^ FPGA index to use for debug signals
  , resetManagerConfig :: ResetManager.Config
  -- ^ Configuration for 'ResetManager.resetManager'
  }

defTransceiverOptions :: TransceiverOptions dom
defTransceiverOptions = TransceiverOptions
  { debugVio = False
  , debugIla = False
  , debugFpgaIndex = pure 0
  , resetManagerConfig = ResetManager.defConfig
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
  , linkUpAfterMs :: Vec n (Signal freeclk (Unsigned 32))
  -- ^ Number of milliseconds it took for the transceivers to come up
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
  , linkUpAfterMs :: Signal freeclk (Unsigned 32)
  -- ^ Number of milliseconds it took for this transceiver to come up
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
  TransceiverOptions freeclk ->

  Clock refclk ->
  Clock freeclk ->

  Reset freeclk ->

  Vec chansUsed String ->
  Vec chansUsed String ->

  Vec chansUsed (Signal rxS (BitVector 1)) ->
  Vec chansUsed (Signal rxS (BitVector 1)) ->

  TransceiverOutputs chansUsed tx rx txS freeclk
transceiverPrbsN opts refclk freeclk rst chanNms clkPaths rxns rxps = TransceiverOutputs
  { txClocks      = map (.txClock)       outputs
  , rxClocks      = map (.rxClock)       outputs
  , txPs          = map (.txP)           outputs
  , txNs          = map (.txN)           outputs
  , linkUps       = map (.linkUp)        outputs
  , stats         = map (.stats)         outputs
  , linkUpAfterMs = map (.linkUpAfterMs) outputs
  }
 where
  transIndices = iterateI (+1) 0
  outputs = zipWith5 (transceiverPrbs opts refclk freeclk rst) transIndices chanNms clkPaths rxns rxps

transceiverPrbs ::
  forall tx rx refclk freeclk txS rxS .
  ( HasSynchronousReset tx
  , HasDefinedInitialValues tx

  , HasSynchronousReset rx
  , HasDefinedInitialValues rx

  , HasSynchronousReset freeclk
  , HasDefinedInitialValues freeclk
  ) =>
  TransceiverOptions freeclk ->

  Clock refclk ->
  Clock freeclk ->

  Reset freeclk ->  -- ^ rst all

  Unsigned 3 ->

  String -> -- ^ channel, example X0Y18
  String -> -- ^ clkPath, example clk0-2

  Signal rxS (BitVector 1) ->
  Signal rxS (BitVector 1) ->

  TransceiverOutput tx rx txS freeclk
transceiverPrbs opts gtrefclk freeclk rst_all_in transIndex chan clkPath rxn rxp =
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
      :> "ila_probe_linkUpAfterMs"
      :> "ila_probe_linkUpAfterSubMs"
      :> "capture"
      :> "trigger"
      :> Nil) { advancedTriggers = True, stages = 1, depth = D1024 })
    freeclk
    opts.debugFpgaIndex
    (pure transIndex :: Signal freeclk (Unsigned 3))
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
    linkUpAfterMs
    linkUpAfterSubMs
    (pure True :: Signal freeclk Bool) -- capture
    (failAfterUp .||. (fmap not linkUp .&&. timeout) .||. lastTxFree) -- trigger

  debugVio :: Signal freeclk ()
  debugVio = vioProbe
    (  "probe_link_up"
    :> "probe_resetManager_txRetries"
    :> "probe_resetManager_rxRetries"
    :> "probe_resetManager_rxFullRetries"
    :> "probe_resetManager_failAfterUps"
    :> "probe_linkUpAfterMs"
    :> "probe_linkUpAfterSubMs"
    :> "probe_transIndex"
    :> Nil )
    Nil
    ()
    freeclk
    linkUp
    ((.txRetries) <$> stats)
    ((.rxRetries) <$> stats)
    ((.rxFullRetries) <$> stats)
    ((.failAfterUps) <$> stats)
    linkUpAfterMs
    linkUpAfterSubMs
    (pure @(Signal freeclk) transIndex)

  result = TransceiverOutput
    { txClock = tx_clk
    , rxClock = rx_clk
    , txP = txp
    , txN = txn
    , linkUp = linkUp
    , linkUpAfterMs = linkUpAfterMs
    , stats = stats
    }

  failAfterUp = isFalling freeclk rst_all_in enableGen False linkUp
  counter = register freeclk rst_all_in enableGen (0 :: IndexMs freeclk 10_000) (satSucc SatBound <$> counter)
  timeout = counter .==. pure maxBound

  linkUpAfter@(unbundle -> (linkUpAfterMs, linkUpAfterSubMs)) =
    register
      freeclk
      rst_all_in
      (toEnable (not <$> linkUp))
      (0 :: Unsigned 32, 0 :: IndexMs freeclk 1)
      (countSucc <$> linkUpAfter)

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

  -- 'prbsWaitMs' is the number of milliseconds representing the worst case time
  -- it takes for the PRBS to stabilize. I.e., after this time we can be sure the
  -- neighbor doesn't reset its transceiver anymore.
  prbsWaitMs =
          ((1 :: Unsigned 1) `add` opts.resetManagerConfig.rxRetries)
    `mul` opts.resetManagerConfig.rxTimeout
  prbsOkDelayed = trueForSteps (Proxy @(Milliseconds 1)) prbsWaitMs rx_clk rxReset prbsOk
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
    <*> xpmCdcArraySingle freeclk tx_clk opts.debugFpgaIndex
    <*> pure transIndex

  (rst_all, rst_rx, _init_done, stats) =
    ResetManager.resetManager
      opts.resetManagerConfig
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
