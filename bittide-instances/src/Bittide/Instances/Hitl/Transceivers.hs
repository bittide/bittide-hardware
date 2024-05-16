-- SPDX-FileCopyrightText: 2023-2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

-- | Test whether clock boards are configurable and transceiver links come
-- online. This assumes to run on a fully connected mesh of 8 FPGAs. Also see
-- 'c_CHANNEL_NAMES' and 'c_CLOCK_PATHS'. It has two tricks up its sleeve:
--
--   1. It uses @SYNC_IN@/@SYNC_OUT@ to make sure each board starts programming
--      its clock boards at the same time.
--
--   2. It keeps track of how many times the GTH's reset manager had to reset
--      the connection and how often it lost connections after establishing
--      them.
--
-- This test will succeed if all links have been up for ten seconds.
--
module Bittide.Instances.Hitl.Transceivers where

import Clash.Prelude (withClockResetEnable)
import Clash.Explicit.Prelude

import Bittide.Arithmetic.Time
import Bittide.ClockControl.Si5395J
import Bittide.ClockControl.Si539xSpi
import Bittide.ElasticBuffer (sticky)
import Bittide.Hitl (HitlTests, NoPostProcData(..), FpgaIndex, hitlVio)
import Bittide.Instances.Domains
import Bittide.Instances.Hitl.Setup
import Bittide.Transceiver

import Clash.Cores.Xilinx.GTH
import Clash.Cores.Xilinx.Xpm.Cdc.Single (xpmCdcSingle)
import Clash.Xilinx.ClockGen
import Data.Maybe (fromMaybe, isJust)

import qualified Bittide.Transceiver.ResetManager as ResetManager
import qualified Clash.Explicit.Prelude as E
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Text as Text

-- | Start value of the counters used in 'counter' and 'expectCounter'. This is
-- a non-zero start value, as a regression test for a bug where the transceivers
-- would not come up if the counters started at zero.
counterStart :: BitVector 64
counterStart = 0xDEAD_BEEF_CA55_E77E

-- | A counter starting at 'counterStart'
counter :: KnownDomain dom => Clock dom -> Reset dom -> Signal dom Bool -> Signal dom (BitVector 64)
counter clk rst ena = let c = register clk rst (toEnable ena) counterStart (c + 1) in c

-- | Expect a counter starting at 'counterStart' and incrementing by one on each
-- cycle.
expectCounter ::
  KnownDomain dom =>
  Clock dom ->
  Reset dom ->
  Signal dom (Maybe (BitVector 64)) ->
  -- ^ Received data
  Signal dom Bool
  -- ^ Error
expectCounter clk rst = sticky clk rst . mealy clk rst enableGen go counterStart
 where
  go c (Just e) = (c + 1, c /= e)
  go c Nothing  = (c, False)

-- | Worker function for 'transceiversUpTest'. See module documentation for more
-- information.
goTransceiversUpTest ::
  Signal Basic125 FpgaIndex ->
  "SMA_MGT_REFCLK_C" ::: Clock Ext200 ->
  "SYSCLK" ::: Clock Basic125 ->
  "RST_LOCAL" ::: Reset Basic125 ->
  "GTH_RX_NS" ::: TransceiverWires GthRx ->
  "GTH_RX_PS" ::: TransceiverWires GthRx ->
  "MISO" ::: Signal Basic125 Bit ->
  ( "GTH_TX_NS" ::: TransceiverWires GthTx
  , "GTH_TX_PS" ::: TransceiverWires GthTx
  , "allUp" ::: Signal Basic125 Bool
  , "anyErrors" ::: Signal Basic125 Bool
  , "stats" ::: Vec (FpgaCount - 1) (Signal Basic125 ResetManager.Statistics)
  , "spiDone" ::: Signal Basic125 Bool
  , "" :::
      ( "SCLK"      ::: Signal Basic125 Bool
      , "MOSI"      ::: Signal Basic125 Bit
      , "CSB"       ::: Signal Basic125 Bool
      )
  )
goTransceiversUpTest fpgaIndex refClk sysClk rst rxNs rxPs miso =
  ( transceivers.txNs
  , transceivers.txPs
  , allUp
  , expectCounterErrorSys
  , transceivers.stats
  , spiDone
  , spiOut
  )
 where
  allUp = and <$> bundle transceivers.linkUps

  sysRst = orReset rst (unsafeFromActiveLow (fmap not spiErr))

  -- Clock programming
  spiDone = E.dflipflop sysClk $ (==Finished) <$> spiState
  spiErr = E.dflipflop sysClk $ isErr <$> spiState

  isErr (Error _) = True
  isErr _         = False

  (_, _, spiState, spiOut) =
    withClockResetEnable sysClk sysRst enableGen $
      si539xSpi testConfig6_200_on_0a_1ppb (SNat @(Microseconds 10)) (pure Nothing) miso

  -- Transceiver setup
  gthAllReset = unsafeFromActiveLow spiDone

  -- Send counters
  counters =
    zipWith3
      counter
      transceivers.txClocks
      transceivers.txResets
      transceivers.txSamplings

  expectCounterError =
    zipWith3
      expectCounter
      transceivers.rxClocks
      transceivers.rxResets
      transceivers.rxDatas

  expectCounterErrorSys =
      fmap and
    $ bundle
    $ zipWith (.&&.) transceivers.linkUps
    $ zipWith (`xpmCdcSingle` sysClk) transceivers.rxClocks expectCounterError

  transceivers =
    transceiverPrbsN
      @GthTx @GthRx @Ext200 @Basic125 @GthTx @GthRx
      defConfig{debugIla=True, debugFpgaIndex=bitCoerce <$> fpgaIndex}
      Inputs
        { clock = sysClk
        , reset = gthAllReset
        , refClock = refClk
        , channelNames
        , clockPaths
        , rxNs
        , rxPs
        , txDatas = counters
        , txReadys = repeat (pure True)
        , rxReadys = repeat (pure True)
        }

-- | Top entity for this test. See module documentation for more information.
transceiversUpTest ::
  "SMA_MGT_REFCLK_C" ::: DiffClock Ext200 ->
  "SYSCLK_300" ::: DiffClock Ext300 ->
  "SYNC_IN" ::: Signal Basic125 Bool ->
  "GTH_RX_NS" ::: TransceiverWires GthRx ->
  "GTH_RX_PS" ::: TransceiverWires GthRx ->
  "MISO" ::: Signal Basic125 Bit ->
  ( "GTH_TX_NS" ::: TransceiverWires GthTx
  , "GTH_TX_PS" ::: TransceiverWires GthTx
  , "SYNC_OUT" ::: Signal Basic125 Bool
  , "spiDone" ::: Signal Basic125 Bool
  , "" :::
      ( "SCLK"      ::: Signal Basic125 Bool
      , "MOSI"      ::: Signal Basic125 Bit
      , "CSB"       ::: Signal Basic125 Bool
      )
  )
transceiversUpTest refClkDiff sysClkDiff syncIn rxns rxps miso =
  (txns, txps, syncOut, spiDone, spiOut)
 where
  refClk = ibufds_gte3 refClkDiff :: Clock Ext200

  (sysClk, sysRst) = clockWizardDifferential sysClkDiff noReset

  testRst = sysRst `orReset` unsafeFromActiveLow startTest `orReset` syncInRst
  syncOut = startTest
  syncInRst =
      resetGlitchFilter (SNat @1024) sysClk
    $ unsafeFromActiveLow
    $ xpmCdcSingle sysClk sysClk syncIn

  (txns, txps, allUp, anyErrors, _stats, spiDone, spiOut) =
    goTransceiversUpTest fpgaIndex refClk sysClk testRst rxns rxps miso

  failAfterUp = isFalling sysClk testRst enableGen False allUp
  failAfterUpSticky = sticky sysClk testRst failAfterUp

  startTest = isJust <$> maybeFpgaIndex
  fpgaIndex = fromMaybe 0 <$> maybeFpgaIndex

  maybeFpgaIndex :: Signal Basic125 (Maybe FpgaIndex)
  maybeFpgaIndex =
    hitlVio
      0
      sysClk

      -- Consider test done if links have been up consistently for 40 seconds. This
      -- is just below the test timeout of 60 seconds, so transceivers have ~20
      -- seconds to come online reliably. This should be plenty.
      (trueFor (SNat @(Seconds 40)) sysClk testRst allUp .||. failAfterUpSticky .||. anyErrors)

      -- Success?
      (fmap not failAfterUpSticky .&&. fmap not anyErrors)

-- XXX: We use an explicit top entity annotation here, as 'makeTopEntity'
--      generates warnings in combination with 'Vec'.
{-# ANN transceiversUpTest Synthesize
  { t_name = "transceiversUpTest"
  , t_inputs =
    [ (PortProduct "SMA_MGT_REFCLK_C") [PortName "p", PortName "n"]
    , (PortProduct "SYSCLK_300") [PortName "p", PortName "n"]
    , PortName "SYNC_IN"
    , PortName "GTH_RX_NS"
    , PortName "GTH_RX_PS"
    , PortName "MISO"
    ]
  , t_output =
    (PortProduct "")
      [ PortName "GTH_TX_NS"
      , PortName "GTH_TX_PS"
      , PortName "SYNC_OUT"
      , PortName "spiDone"
      , (PortProduct "") [PortName "SCLK", PortName "MOSI", PortName "CSB"]
      ]
  } #-}

tests :: HitlTests FpgaIndex
tests = Map.fromList testsAsList
 where
  fpgaIndices = [0..]
  nTests = 1
  testNames = ["T" <> Text.pack (show n) | n <- [(0::Int)..nTests-1]]
  testsAsList = [(nm, (L.zip fpgaIndices fpgaIndices, NoPostProcData)) | nm <- testNames]
