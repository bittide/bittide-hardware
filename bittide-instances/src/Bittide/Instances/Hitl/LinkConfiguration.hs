-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=20 #-}

-- | Test whether the transceiver link setup matches with the
-- configuration defined in 'Bittide.Instances.Hitl.Setup.fpgaSetup'.
-- To this end, the test disables all transceivers of a selected node,
-- which then can be observed by all the other nodes and checked
-- against the link mask derived from the table. The check is repeated
-- for each node in the network.
module Bittide.Instances.Hitl.LinkConfiguration
  ( linkConfigurationTest
  , tests
  ) where

import Clash.Prelude (withClockResetEnable)
import Clash.Explicit.Prelude
import qualified Clash.Explicit.Prelude as E

import Data.Graph (buildG)
import Data.Maybe (isJust)
import Data.String (fromString)

import qualified Data.List as List (concat)
import qualified Data.Map.Strict as Map (fromList)

import Bittide.Arithmetic.Time
import Bittide.ClockControl.Si5395J
import Bittide.ClockControl.Si539xSpi (ConfigState(Error, Finished), si539xSpi)
import Bittide.Instances.Domains
import Bittide.Topology (boundGraph)
import Bittide.Transceiver

import Clash.Cores.Xilinx.VIO (vioProbe)
import Bittide.Hitl (HitlTests, NoPostProcData(..) {-, hitlVio-})

import Bittide.Instances.Hitl.Setup

import Clash.Cores.Xilinx.GTH
import Clash.Cores.Xilinx.Xpm.Cdc.Single
import Clash.Xilinx.ClockGen

data TestConfig =
  TestConfig
    { fpgaEnabled :: Bool
    , disabledLinkMask :: BitVector (FpgaCount - 1)
    }
  deriving (Generic, NFDataX, BitPack, Show)

-- | Configures the clock boards, fires up all of the transceivers and
-- observes the incoming links.
transceiversStartAndObserve ::
  "SMA_MGT_REFCLK_C" ::: Clock Ext200 ->
  "SYSCLK" ::: Clock Basic125 ->
  "RST_LOCAL" ::: Reset Basic125 ->
  "GTH_RX_NS" ::: TransceiverWires GthRx ->
  "GTH_RX_PS" ::: TransceiverWires GthRx ->
  "MISO" ::: Signal Basic125 Bit ->
  ( "GTH_TX_NS" ::: TransceiverWires GthTx
  , "GTH_TX_PS" ::: TransceiverWires GthTx
  , "LINK_UPS" ::: Signal Basic125 (BitVector (FpgaCount - 1))
  , "stats" ::: Vec (FpgaCount - 1) (Signal Basic125 GthResetStats)
  , "spiDone" ::: Signal Basic125 Bool
  , "" :::
      ( "SCLK"      ::: Signal Basic125 Bool
      , "MOSI"      ::: Signal Basic125 Bit
      , "CSB"       ::: Signal Basic125 Bool
      )
  )
transceiversStartAndObserve refClk sysClk rst rxns rxps miso =
  (txns, txps, v2bv . fmap boolToBit <$> linkUps, stats, spiDone, spiOut)
 where
  sysRst = rst `orReset` unsafeFromActiveLow (fmap not spiErr)

  -- Clock programming
  spiDone = E.dflipflop sysClk $ (== Finished) <$> spiState
  spiErr = E.dflipflop sysClk $ isErr <$> spiState

  isErr = \case
    Error {} -> True
    _        -> False

  (_, _, spiState, spiOut) =
    withClockResetEnable sysClk sysRst enableGen $
      si539xSpi testConfig6_200_on_0a (SNat @(Microseconds 10)) (pure Nothing) miso

  -- Transceiver setup
  gthAllReset = rst `orReset` unsafeFromActiveLow spiDone

  (_txClocks, rxClocks, txns, txps, linkUpsRx, stats) = unzip6 $
    transceiverPrbsN
      @GthTx @GthRx @Ext200 @Basic125 @GthTx @GthRx
      refClk sysClk gthAllReset
      channelNames clockPaths rxns rxps

  syncLink rxClock linkUp = xpmCdcSingle rxClock sysClk linkUp
  linkUps = bundle $ zipWith syncLink rxClocks linkUpsRx

-- | Top entity for this test. See module documentation for more information.
linkConfigurationTest ::
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
linkConfigurationTest refClkDiff sysClkDiff syncIn rxns rxps miso =
  (txns, txps, syncOut, spiDone, spiOut)
 where
  refClk = ibufds_gte3 refClkDiff :: Clock Ext200
  (sysClk, sysRst) = clockWizardDifferential sysClkDiff noReset

  -- the test starts with a valid configuration coming in
  startTest = isJust <$> testConfig

  -- use some simple SYNC_IN / SYNC_OUT synchronization to
  -- synchronously start the test
  syncInRst =
      resetGlitchFilter (SNat @1024) sysClk
    $ unsafeFromActiveLow
    $ xpmCdcSingle sysClk sysClk syncIn
  syncOut = startTest

  testRst =   sysRst
    `orReset` syncInRst
    `orReset` unsafeFromActiveLow startTest
    `orReset` unsafeFromActiveHigh skip

  -- derive the test settings from the passed configuration
  skip = maybe False (not . fpgaEnabled) <$> testConfig
  mask = maybe 0 disabledLinkMask <$> testConfig

  (txns, txps, linkUps, _stats, spiDone, spiOut) =
    transceiversStartAndObserve refClk sysClk testRst rxns rxps miso

  -- Consider the test done if the enabled links have been up
  -- consistently for a second.
  endSuccess = trueFor (SNat @(Seconds 1)) sysClk testRst linkMaskMatch

  linkMaskMatch = and . fmap bitToBool . bv2v <$> (xor <$> mask <*> linkUps)

  testConfig :: Signal Basic125 (Maybe TestConfig)
  testConfig =
    let
      (start, dat) = unbundle $ setName @"vioHitlt" $ vioProbe
        ("probe_test_done" :> "probe_test_success" :> "linksUps" :>  Nil)
        ("probe_test_start" :> "probe_test_data" :> Nil)
        (False, disabled)
        sysClk
        -- done
        (skip .||. endSuccess)
        -- success
        (skip .||. linkMaskMatch)
        -- other
        linkUps
    in
      mux start (Just <$> dat) (pure Nothing)

-- XXX: We use an explicit top entity annotation here, as 'makeTopEntity'
--      generates warnings in combination with 'Vec'.
{-# ANN linkConfigurationTest Synthesize
  { t_name = "linkConfigurationTest"
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

tests :: HitlTests TestConfig
tests = Map.fromList
  [ ( "disable " <> fromString (show i) <> " [" <> fromString fpgaId <> "]"
    , ( toList $ imap (testData i) $ linkMasks @FpgaCount g
      , NoPostProcData
      )
    )
  | (i, (fpgaId, _)) <- toList $ zip indicesI fpgaSetup
  , let g = boundGraph @FpgaCount $ buildG (0, natToNum @(FpgaCount - 1))
          $ List.concat
              [ [ (i', j), (j, i') ]
              | j <- [0, 1 .. natToNum @(FpgaCount - 1)]
              , let i' = fromEnum i
              , i' /= j
              ]
  ]
 where
  testData i j m = (j, TestConfig (i /= j) m)

disabled :: TestConfig
disabled = TestConfig
  { fpgaEnabled = False
  , disabledLinkMask = 0
  }
