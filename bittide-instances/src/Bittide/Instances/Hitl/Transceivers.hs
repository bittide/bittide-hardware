-- SPDX-FileCopyrightText: 2023-2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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
import Bittide.Hitl (HitlTests, hitlVioBool, noConfigTest, allFpgas)
import Bittide.Instances.Domains
import Bittide.Transceiver

import Clash.Annotations.TH (makeTopEntity)
import Clash.Cores.Xilinx.GTH
import Clash.Cores.Xilinx.Xpm.Cdc.Single (xpmCdcSingle)
import Clash.Xilinx.ClockGen

import qualified Bittide.Transceiver.ResetManager as ResetManager
import qualified Clash.Explicit.Prelude as E

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
  forall ref rx tx.
  ( HasSynchronousReset ref, HasSynchronousReset rx, HasSynchronousReset tx
  , HasDefinedInitialValues rx, HasDefinedInitialValues tx
  , KnownDomain ref, KnownDomain rx, KnownDomain tx
  ) =>
  Unsigned 3 ->
  String ->
  String ->
  "SMA_MGT_REFCLK_C" ::: Clock ref ->
  "BASIC125CLK" ::: Clock Basic125 ->
  "RST_LOCAL" ::: Reset Basic125 ->
  "GTH_RX_NS" ::: Signal rx (BitVector 1) ->
  "GTH_RX_PS" ::: Signal rx (BitVector 1) ->
  "MISO" ::: Signal Basic125 Bit ->
  ( "GTH_TX_NS" ::: Signal tx (BitVector 1)
  , "GTH_TX_PS" ::: Signal tx (BitVector 1)
  , "allUp" ::: Signal Basic125 Bool
  , "anyErrors" ::: Signal Basic125 Bool
  , "stats" ::: Signal Basic125 ResetManager.Statistics
  , "spiDone" ::: Signal Basic125 Bool
  , "" :::
      ( "SCLK"      ::: Signal Basic125 Bool
      , "MOSI"      ::: Signal Basic125 Bit
      , "CSB"       ::: Signal Basic125 Bool
      )
  )
goTransceiversUpTest
  transceiverIndex channelName clockPath
  refClock sysClk rst rxN rxP miso
  = (txN, txP, linkUp, expectCounterErrorSys, stats, spiDone, spiOut)
 where
  sysRst = orReset rst $ unsafeFromActiveLow $ fmap not spiErr

  -- Clock programming
  spiDone = E.dflipflop sysClk $ (==Finished) <$> spiState
  spiErr = E.dflipflop sysClk $ isErr <$> spiState

  isErr (Error _) = True
  isErr _         = False

  (_, _, spiState, spiOut) =
    withClockResetEnable sysClk sysRst enableGen $
      si539xSpi testConfig6_200_on_0a_1ppb (SNat @(Microseconds 10)) (pure Nothing) miso

  -- Transceiver setup
  Output{..} =
    transceiverPrbs @tx @rx @ref @Basic125 @tx @rx defConfig Input
      { clock = sysClk
      , reset = unsafeFromActiveLow spiDone
      , txData = counter txClock txReset txSampling
      , txReady = pure True
      , rxReady = pure True
      , ..
      }

  expectCounterErrorSys =
    linkUp .&&. xpmCdcSingle rxClock sysClk (expectCounter rxClock rxReset rxData)

-- | Top entity for this test. See module documentation for more information.
transceiversUpTest ::
  "SMA_MGT_REFCLK_C" ::: DiffClock Ext200A ->
  "PCIE_CLK_Q0" ::: DiffClock Ext200B ->
  "SYSCLK_300" ::: DiffClock Ext300 ->
  "GTH_RX_NS_0" ::: Signal GthRxA (BitVector 1) ->
  "GTH_RX_PS_0" ::: Signal GthRxA (BitVector 1) ->
  "GTH_RX_NS_1" ::: Signal GthRxB (BitVector 1) ->
  "GTH_RX_PS_1" ::: Signal GthRxB (BitVector 1) ->
  "MISO_0" ::: Signal Basic125 Bit ->
  "MISO_1" ::: Signal Basic125 Bit ->
  ( "GTH_TX_NS_0" ::: Signal GthTxA (BitVector 1)
  , "GTH_TX_PS_0" ::: Signal GthTxA (BitVector 1)
  , "GTH_TX_NS_1" ::: Signal GthTxB (BitVector 1)
  , "GTH_TX_PS_1" ::: Signal GthTxB (BitVector 1)
  , "spiDone" ::: Signal Basic125 Bool
  , "" :::
      ( "SCLK_0"      ::: Signal Basic125 Bool
      , "MOSI_0"      ::: Signal Basic125 Bit
      , "CSB_0"       ::: Signal Basic125 Bool
      )
  , "" :::
      ( "SCLK_1"      ::: Signal Basic125 Bool
      , "MOSI_1"      ::: Signal Basic125 Bit
      , "CSB_1"       ::: Signal Basic125 Bool
      )
  )
transceiversUpTest
  refClkDiff0 refClkDiff1 sysClkDiff
  rxns0 rxps0
  rxns1 rxps1
  miso0 miso1
  = ( txns0, txps0
    , txns1, txps1
    , spiDone0 .&&. spiDone1
    , spiOut0, spiOut1
    )
 where
  refClk0 = ibufds_gte3 refClkDiff0 :: Clock Ext200A
  refClk1 = ibufds_gte3 refClkDiff1 :: Clock Ext200B

  (sysClk, sysRst) = clockWizardDifferential sysClkDiff noReset

  testRst = sysRst `orReset` unsafeFromActiveLow startTest

  (txns0, txps0, allUp0, anyErrors0, _stats0, spiDone0, spiOut0) =
    goTransceiversUpTest 0 "X0Y10" "clk0" refClk0 sysClk testRst rxns0 rxps0 miso0

  (txns1, txps1, allUp1, anyErrors1, _stats1, spiDone1, spiOut1) =
    goTransceiversUpTest 1 "X0Y9" "clk0-1" refClk1 sysClk testRst rxns1 rxps1 miso1

  failAfterUp = isFalling sysClk testRst enableGen False (allUp0 .&&. allUp1)
  failAfterUpSticky = sticky sysClk testRst failAfterUp

  startTest :: Signal Basic125 Bool
  startTest = hitlVioBool sysClk
    -- Consider test done if links have been up consistently for 40 seconds. This
    -- is just below the test timeout of 60 seconds, so transceivers have ~20
    -- seconds to come online reliably. This should be plenty.
    (    trueFor (SNat @(Seconds 40)) sysClk testRst (allUp0 .&&. allUp1)
    .||. failAfterUpSticky
    .||. anyErrors0
    .||. anyErrors1
    )

    -- Success?
    (fmap not failAfterUpSticky .&&. fmap not (anyErrors0 .||. anyErrors1))

makeTopEntity 'transceiversUpTest

tests :: HitlTests ()
tests = noConfigTest "Transceivers" allFpgas
