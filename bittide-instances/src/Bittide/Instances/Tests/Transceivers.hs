-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE NumericUnderscores #-}

-- | Test whether clock boards are configurable and transceiver links come
-- online. This assumes to run on a fully connected mesh of 8 FPGAs. Also see
-- 'c_CHANNEL_NAMES' and 'c_CLOCK_PATHS'.
module Bittide.Instances.Tests.Transceivers where

import Clash.Prelude
import Clash.Explicit.Prelude (noReset, orReset)

import Bittide.Arithmetic.Time
import Bittide.ClockControl.Si5395J
import Bittide.ClockControl.Si539xSpi
import Bittide.Instances.Domains
import Bittide.Transceiver

import Clash.Cores.Xilinx.GTH
import Clash.Cores.Xilinx.VIO (vioProbe)
import Clash.Cores.Xilinx.Xpm.Cdc.Single
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
  "SMA_MGT_REFCLK_C" ::: Clock Basic200 ->
  "SYSCLK" ::: Clock Basic125 ->
  "RST_LOCAL" ::: Reset Basic125 ->
  "GTH_RX_NS" ::: TransceiverWires GthRx ->
  "GTH_RX_PS" ::: TransceiverWires GthRx ->
  "MISO" ::: Signal Basic125 Bit ->
  ( "GTH_TX_NS" ::: TransceiverWires GthTx
  , "GTH_TX_PS" ::: TransceiverWires GthTx
  , "allUp" ::: Signal Basic125 Bool
  , "spiDone" ::: Signal Basic125 Bool
  , "" :::
      ( "SCLK"      ::: Signal Basic125 Bool
      , "MOSI"      ::: Signal Basic125 Bit
      , "CSB"       ::: Signal Basic125 Bool
      )
  )
goTransceiversUpTest refClk sysClk rst rxns rxps miso =
  (txns, txps, and <$> bundle linkUps, spiDone, spiOut)
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
  gthAllReset  = E.holdReset sysClk enableGen d1024 (unsafeFromActiveLow spiDone)
  gthChanReset = E.holdReset sysClk enableGen d1024 gthAllReset
  gthStimReset = E.holdReset sysClk enableGen d1024 gthChanReset

  (_txClocks, rxClocks, txns, txps, linkUpsRx) = unzip5 $
    transceiverPrbsN
      @GthTx @GthRx @Basic200 @Basic125 @GthTx @GthRx
      refClk sysClk gthAllReset gthStimReset gthChanReset
      c_CHANNEL_NAMES c_CLOCK_PATHS rxns rxps

  syncLink rxClock linkUp = xpmCdcSingle rxClock sysClk linkUp
  linkUps = zipWith syncLink rxClocks linkUpsRx

-- | Top entity for this test. See module documentation for more information.
transceiversUpTest ::
  "SMA_MGT_REFCLK_C" ::: DiffClock Basic200 ->
  "SYSCLK_300" ::: DiffClock Basic300 ->
  "GTH_RX_NS" ::: TransceiverWires GthRx ->
  "GTH_RX_PS" ::: TransceiverWires GthRx ->
  "MISO" ::: Signal Basic125 Bit ->
  ( "GTH_TX_NS" ::: TransceiverWires GthTx
  , "GTH_TX_PS" ::: TransceiverWires GthTx
  , "spiDone" ::: Signal Basic125 Bool
  , "" :::
      ( "SCLK"      ::: Signal Basic125 Bool
      , "MOSI"      ::: Signal Basic125 Bit
      , "CSB"       ::: Signal Basic125 Bool
      )
  )
transceiversUpTest refClkDiff sysClkDiff rxns rxps miso =
  (txns, txps, spiDone, spiOut)
 where
  refClk = ibufds_gte3 refClkDiff :: Clock Basic200

  (sysClk, sysLock0) = clockWizardDifferential (SSymbol @"SysClk") sysClkDiff noReset
  sysLock1 = xpmCdcSingle sysClk sysClk sysLock0 -- improvised reset syncer
  sysRst = unsafeFromActiveLow sysLock1
  testRst = orReset sysRst (unsafeFromActiveLow startTest)

  (txns, txps, allUp, spiDone, spiOut) =
    goTransceiversUpTest refClk sysClk testRst rxns rxps miso

  startTest :: Signal Basic125 Bool
  startTest =
    vioProbe
      (  "probe_test_done"
      :> "probe_test_success"
      :> Nil)
      (  "probe_test_start"
      :> Nil)
      False
      sysClk
      allUp
      (pure True :: Signal Basic125 Bool)
-- XXX: We use an explicit top entity annotation here, as 'makeTopEntity'
--      generates warnings in combination with 'Vec'.
{-# ANN transceiversUpTest Synthesize
  { t_name = "transceiversUpTest"
  , t_inputs =
    [ (PortProduct "SMA_MGT_REFCLK_C") [PortName "p", PortName "n"]
    , (PortProduct "SYSCLK_300") [PortName "p", PortName "n"]
    , PortName "GTH_RX_NS"
    , PortName "GTH_RX_PS"
    , PortName "MISO"
    ]
  , t_output =
    (PortProduct "")
      [ PortName "GTH_TX_NS"
      , PortName "GTH_TX_PS"
      , PortName "spiDone"
      , (PortProduct "") [PortName "SCLK", PortName "MOSI", PortName "CSB"]
      ]
  } #-}
