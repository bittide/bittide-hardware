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
import Bittide.Hitl (HitlTests, hitlVioBool, allFpgas, noConfigTest)
import Bittide.Instances.Domains
import Bittide.Transceiver

import Clash.Cores.Xilinx.GTH
import Clash.Cores.Xilinx.Xpm.Cdc.Single
import Clash.Xilinx.ClockGen

import qualified Clash.Explicit.Prelude as E

-- | Data wires from/to transceivers. No logic should be inserted on these
-- wires. Should be considered asynchronous to one another - even though their
-- domain encodes them as related.
type TransceiverWires dom = Vec 2 (Signal dom (BitVector 1))

-- | Worker function for 'transceiversUpTest'. See module documentation for more
-- information.
goTransceiversUpTest ::
  forall ref rx tx.
  ( HasSynchronousReset ref, HasSynchronousReset rx, HasSynchronousReset tx
  , HasDefinedInitialValues rx, HasDefinedInitialValues tx
  , KnownDomain ref, KnownDomain rx, KnownDomain tx
  ) =>
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
  , "stats" ::: Signal Basic125 GthResetStats
  , "spiDone" ::: Signal Basic125 Bool
  , "" :::
      ( "SCLK"      ::: Signal Basic125 Bool
      , "MOSI"      ::: Signal Basic125 Bit
      , "CSB"       ::: Signal Basic125 Bool
      )
  )
goTransceiversUpTest cname cpath refClk sysClk rst rxns rxps miso =
  (txns, txps, linkUp, stats, spiDone, spiOut)
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

  (_txClocks, rxClock, txns, txps, linkUpRx, stats) =
    transceiverPrbs @tx @rx @ref @Basic125 @tx @rx
      refClk sysClk gthAllReset
      cname cpath rxns rxps

  linkUp = xpmCdcSingle rxClock sysClk linkUpRx

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
  "SMA_MGT_REFCLK_C" ::: DiffClock Ext200A ->
  "PCIE_CLK_Q0_C" ::: DiffClock Ext200B ->
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

  (txns0, txps0, allUp0, _stats0, spiDone0, spiOut0) =
    goTransceiversUpTest "X0Y10" "clk0" refClk0 sysClk testRst rxns0 rxps0 miso0

  (txns1, txps1, allUp1, _stats1, spiDone1, spiOut1) =
    goTransceiversUpTest "X0Y9" "clk0-1" refClk1 sysClk testRst rxns1 rxps1 miso1

  startTest :: Signal Basic125 Bool
  startTest =
    hitlVioBool
      sysClk

      -- Consider test done if links have been up consistently for 50 seconds. This
      -- is just below the test timeout of 60 seconds, so transceivers have ~10
      -- seconds to come online reliably. This should be plenty.
      (trueFor50s sysClk testRst (allUp0 .&&. allUp1))

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
    , (PortProduct "PCIE_CLK_Q0") [PortName "p", PortName "n"]
    , (PortProduct "SYSCLK_300") [PortName "p", PortName "n"]
    , PortName "GTH_RX_NS_0"
    , PortName "GTH_RX_PS_0"
    , PortName "GTH_RX_NS_1"
    , PortName "GTH_RX_PS_1"
    , PortName "MISO_0"
    , PortName "MISO_1"
    ]
  , t_output =
    (PortProduct "")
      [ PortName "GTH_TX_NS_0"
      , PortName "GTH_TX_PS_0"
      , PortName "GTH_TX_NS_1"
      , PortName "GTH_TX_PS_1"
      , PortName "spiDone"
      , (PortProduct "") [PortName "SCLK_0", PortName "MOSI_0", PortName "CSB_0"]
      , (PortProduct "") [PortName "SCLK_1", PortName "MOSI_1", PortName "CSB_1"]
      ]
  } #-}

tests :: HitlTests ()
tests = noConfigTest "Transceivers" allFpgas
