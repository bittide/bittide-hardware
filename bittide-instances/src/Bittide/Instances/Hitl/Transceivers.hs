-- SPDX-FileCopyrightText: 2023-2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE NumericUnderscores #-}
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
import Bittide.Instances.Domains
import Bittide.Transceiver

import Clash.Cores.Xilinx.GTH
import Clash.Cores.Xilinx.Xpm.Cdc.Single
import Clash.Hitl (HitlTests, hitlVioBool, allFpgas, noConfigTest)
import Clash.Xilinx.ClockGen

import qualified Clash.Explicit.Prelude as E

c_CHANNEL_NAMES :: Vec 7 String
c_CHANNEL_NAMES =
  "X0Y10":> "X0Y9":> "X0Y16" :> "X0Y17" :> "X0Y18" :> "X0Y19" :> "X0Y11" :> Nil

c_CLOCK_PATHS :: Vec 7 String
c_CLOCK_PATHS =
  "clk0" :> "clk0":> "clk0-2":> "clk0-2":> "clk0-2":> "clk0-2":> "clk0"  :> Nil

-- | Data wires from/to transceivers. No logic should be inserted on these
-- wires. Should be considered asynchronous to one another - even though their
-- domain encodes them as related.
type TransceiverWires dom = Vec 7 (Signal dom (BitVector 1))

-- | Worker function for 'transceiversUpTest'. See module documentation for more
-- information.
goTransceiversUpTest ::
  "SMA_MGT_REFCLK_C" ::: Clock Ext200 ->
  "SYSCLK" ::: Clock Basic125 ->
  "RST_LOCAL" ::: Reset Basic125 ->
  "GTH_RX_NS" ::: TransceiverWires GthRx ->
  "GTH_RX_PS" ::: TransceiverWires GthRx ->
  "MISO" ::: Signal Basic125 Bit ->
  ( "GTH_TX_NS" ::: TransceiverWires GthTx
  , "GTH_TX_PS" ::: TransceiverWires GthTx
  , "allUp" ::: Signal Basic125 Bool
  , "stats" ::: Vec 7 (Signal Basic125 GthResetStats)
  , "spiDone" ::: Signal Basic125 Bool
  , "" :::
      ( "SCLK"      ::: Signal Basic125 Bool
      , "MOSI"      ::: Signal Basic125 Bit
      , "CSB"       ::: Signal Basic125 Bool
      )
  )
goTransceiversUpTest refClk sysClk rst rxns rxps miso =
  (txns, txps, and <$> bundle linkUps, stats, spiDone, spiOut)
 where
  sysRst = orReset rst (unsafeFromActiveLow (fmap not spiErr))

  -- Clock programming
  spiDone = E.dflipflop sysClk $ (==Finished) <$> spiState
  spiErr = E.dflipflop sysClk $ isErr <$> spiState

  isErr (Error _) = True
  isErr _         = False

  (_, _, spiState, spiOut) =
    withClockResetEnable sysClk sysRst enableGen $
      si539xSpi testConfig6_200_on_0a (SNat @(Microseconds 10)) (pure Nothing) miso

  -- Transceiver setup
  gthAllReset = unsafeFromActiveLow spiDone

  (_txClocks, rxClocks, txns, txps, linkUpsRx, stats) = unzip6 $
    transceiverPrbsN
      @GthTx @GthRx @Ext200 @Basic125 @GthTx @GthRx
      refClk sysClk gthAllReset
      c_CHANNEL_NAMES c_CLOCK_PATHS rxns rxps

  syncLink rxClock linkUp = xpmCdcSingle rxClock sysClk linkUp
  linkUps = zipWith syncLink rxClocks linkUpsRx

-- | Returns 'True' if incoming signal has been 'True' for 50 seconds.
trueFor50s ::
  forall dom .
  KnownDomain dom =>
  Clock dom ->
  Reset dom ->
  Signal dom Bool ->
  Signal dom Bool
trueFor50s clk rst =
  moore clk rst enableGen goState goOutput (0 :: IndexMs dom 50_000)
 where
  goState counter  True  = satSucc SatBound counter
  goState _counter False = 0

  goOutput = (== maxBound)

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

  (txns, txps, allUp, _stats, spiDone, spiOut) =
    goTransceiversUpTest refClk sysClk testRst rxns rxps miso

  startTest :: Signal Basic125 Bool
  startTest =
    hitlVioBool
      sysClk

      -- Consider test done if links have been up consistently for 50 seconds. This
      -- is just below the test timeout of 60 seconds, so transceivers have ~10
      -- seconds to come online reliably. This should be plenty.
      (trueFor50s sysClk testRst allUp)

      -- This test either succeeds or times out, so success is set to a static
      -- 'True'. If you want to see statistics, consider setting it to 'False' -
      -- it will make the test TCL print out all probe values.
      (pure True :: Signal Basic125 Bool)

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

tests :: HitlTests ()
tests = noConfigTest "Transceivers" allFpgas
