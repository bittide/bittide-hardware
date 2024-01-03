-- SPDX-FileCopyrightText: 2023-2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}

{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=20 #-}

-- | Test whether clock boards are configurable and transceiver links come
-- online. If they do, run clock control and wait for the clocks to stabilize.
-- This assumes to run on a fully connected mesh of 8 FPGAs. Also see
-- 'c_CHANNEL_NAMES' and 'c_CLOCK_PATHS'. It has two tricks up its sleeve:
--
--   1. It uses @SYNC_IN@/@SYNC_OUT@ to make sure each board starts programming
--      its clock boards at the same time.
--
--   2. It keeps track of how many times the GTH's reset manager had to reset
--      the connection and how often it lost connections after establishing
--      them.
--
-- This test will succeed if all clocks have been stable for 5 seconds. Note:
-- this doesn't test reframing yet.
--
module Bittide.Instances.Hitl.FullMeshHwCc
  ( fullMeshHwCcTest
  , clockControlConfig
  , tests
  ) where

import Clash.Prelude (withClockResetEnable)
import Clash.Explicit.Prelude
import qualified Clash.Explicit.Prelude as E

import Data.Maybe (fromMaybe)
import Data.Proxy
import LiftType (liftTypeQ)

import Bittide.Arithmetic.Time
import Bittide.ClockControl
import Bittide.ClockControl.Callisto
import Bittide.ClockControl.Callisto.Util (FINC, FDEC, stickyBits, speedChangeToPins)
import Bittide.ClockControl.Si5395J
import Bittide.ClockControl.Si539xSpi (ConfigState(Error, Finished), si539xSpi)
import Bittide.Counter
import Bittide.ElasticBuffer (sticky)
import Bittide.Hitl (HitlTestsWithPostProcData, hitlVioBool, allFpgas)
import Bittide.Instances.Domains
import Bittide.Instances.Hitl.Setup
import Bittide.Simulate.Config (SimConf(..))
import Bittide.Topology (TopologyType(..))

import Bittide.Instances.Hitl.IlaPlot

import Clash.Annotations.TH (makeTopEntity)
import Clash.Class.Counter
import Clash.Cores.Xilinx.GTH
import Clash.Cores.Xilinx.Ila (IlaConfig(..), Depth(..), ila, ilaConfig)
import Clash.Sized.Extra (unsignedToSigned)
import Clash.Xilinx.ClockGen

import qualified Bittide.Transceiver as Transceiver
import qualified Bittide.Transceiver.ResetManager as ResetManager

import qualified Data.Map as Map (singleton)

clockControlConfig ::
  $(case (instancesClockConfig (Proxy @Basic125)) of { (_ :: t) -> liftTypeQ @t })
clockControlConfig =
  $(lift (instancesClockConfig (Proxy @Basic125)))

-- | Instantiates a hardware implementation of Callisto and exports its results. Can
-- be used to drive FINC/FDEC directly (see @FINC_FDEC@ result) or to tie the
-- results to a RiscV core (see 'fullMeshRiscvCopyTest')
fullMeshHwTest ::
  forall ref rx tx i.
  ( HasSynchronousReset ref, HasSynchronousReset rx, HasSynchronousReset tx
  , HasDefinedInitialValues rx, HasDefinedInitialValues tx
  , KnownDomain ref, KnownDomain rx, KnownDomain tx
  , KnownNat i, 1 <= i
  ) =>
  String ->
  String ->
  SNat i ->
  "SMA_MGT_REFCLK_C" ::: Clock ref ->
  "SYSCLK" ::: Clock Basic125 ->
  "ILA_CTRL" ::: IlaControl Basic125 ->
  "GTH_RX_NS" ::: Signal rx (BitVector 1) ->
  "GTH_RX_PS" ::: Signal rx (BitVector 1) ->
  "MISO" ::: Signal Basic125 Bit ->
  ( "GTH_TX_NS" ::: Signal tx (BitVector 1)
  , "GTH_TX_PS" ::: Signal tx (BitVector 1)
  , "FINC_FDEC" ::: Signal Basic125 (FINC, FDEC)

  , "CALLISTO_RESULT" ::: Signal Basic125 (CallistoResult (FpgaCount - 1))
  , "CALLISTO_RESET" ::: Reset Basic125
  , "DATA_COUNTER" ::: Signal Basic125 (DataCount 32)
  , "stats" ::: Signal Basic125 ResetManager.Statistics
  , "spiDone" ::: Signal Basic125 Bool
  , "" :::
      ( "SCLK" ::: Signal Basic125 Bool
      , "MOSI" ::: Signal Basic125 Bit
      , "CSB"  ::: Signal Basic125 Bool
      )
  , "transceiversFailedAfterUp" ::: Signal Basic125 Bool
  , "ALL_READY" ::: Signal Basic125 Bool
  , "ALL_STABLE" ::: Signal Basic125 Bool
  )
fullMeshHwTest
  channelName clockPath i
  refClock sysClk IlaControl{syncRst = rst, ..} rxN rxP miso
  = fincFdecIla `hwSeqX`
  ( txN
  , txP
  , frequencyAdjustments
  , callistoResult
  , clockControlReset
  , domainDiff
  , stats
  , spiDone
  , spiOut
  , transceiversFailedAfterUp
  , allUp
  , allStable0
  )
 where
  syncRst = rst `orReset` (unsafeFromActiveLow (fmap not spiErr))

  -- Clock programming
  spiDone = E.dflipflop sysClk $ (==Finished) <$> spiState
  spiErr = E.dflipflop sysClk $ isErr <$> spiState

  isErr (Error _) = True
  isErr _         = False

  (_, _, spiState, spiOut) =
    withClockResetEnable sysClk syncRst enableGen $
      si539xSpi testConfig6_200_on_0a_10ppb (SNat @(Microseconds 10)) (pure Nothing) miso

  -- Transceiver setup
  Transceiver.Output{..} =
    Transceiver.transceiverPrbs
      @tx @rx @ref @Basic125 @tx @rx Transceiver.defConfig Transceiver.Input
        { clock = sysClk
        , reset = unsafeFromActiveLow spiDone
        , transceiverIndex = natToNum @i
        , txData = pure 0
        , txReady = pure False
        , rxReady = pure True
        , ..
        }

  allUp = trueFor (SNat @(Milliseconds 500)) sysClk syncRst linkUp
  transceiversFailedAfterUp =
    sticky sysClk syncRst (isFalling sysClk syncRst enableGen False allUp)

  timeSucc = countSucc @(Unsigned 16, Index (PeriodToCycles Basic125 (Milliseconds 1)))
  timer = register sysClk syncRst enableGen (0, 0) (timeSucc <$> timer)
  milliseconds1 = fst <$> timer

  -- Clock control
  clockControlReset =
      orReset (unsafeFromActiveLow allUp)
    $ orReset (unsafeFromActiveHigh transceiversFailedAfterUp)
              (unsafeFromActiveLow syncStart)

  availableLinkMask = pure maxBound

  (clockMod, _stabilities, allStable0, _allCentered) = unbundle $
    fmap
      (\CallistoResult{..} -> (maybeSpeedChange, stability, allStable, allSettled))
      callistoResult

  callistoResult =
    callistoClockControlWithIla @(FpgaCount - 1) @CccBufferSize i
      txClock sysClk clockControlReset clockControlConfig
      IlaControl{..} availableLinkMask (singleton (resize <$> domainDiff))

  -- Capture every 100 microseconds - this should give us a window of about 5
  -- seconds. Or: when we're in reset. If we don't do the latter, the VCDs get
  -- very confusing.
  capture = (captureFlag .&&. allUp) .||. unsafeToActiveHigh syncRst

  fincFdecIla :: Signal Basic125 ()
  fincFdecIla = setName @"fincFdecIla" $ ila
    (ilaConfig $
         "trigger_0"
      :> "capture_0"
      :> "probe_milliseconds"
      :> "probe_allStable0"
      :> "probe_transceiversFailedAfterUp"
      :> "probe_nFincs"
      :> "probe_nFdecs"
      :> "probe_net_nFincs"
      :> Nil
    ){depth = D16384}
    sysClk

    -- Trigger as soon as we come out of reset
    (unsafeToActiveLow syncRst)

    capture

    -- Debug probes
    milliseconds1
    allStable0
    transceiversFailedAfterUp
    nFincs
    nFdecs
    (fmap unsignedToSigned nFincs - fmap unsignedToSigned nFdecs)

  captureFlag = riseEvery sysClk syncRst enableGen
    (SNat @(PeriodToCycles Basic125 (Milliseconds 1)))

  nFincs = regEn sysClk clockControlReset enableGen
    (0 :: Unsigned 32)
    ((== Just SpeedUp) <$> clockMod)
    (satSucc SatBound <$> nFincs)

  nFdecs = regEn sysClk clockControlReset enableGen
    (0 :: Unsigned 32)
    ((== Just SlowDown) <$> clockMod)
    (satSucc SatBound <$> nFdecs)

  frequencyAdjustments :: Signal Basic125 (FINC, FDEC)
  frequencyAdjustments =
    E.delay sysClk enableGen minBound {- glitch filter -} $
      withClockResetEnable sysClk clockControlReset enableGen $
        stickyBits @Basic125 d20 (speedChangeToPins . fromMaybe NoChange <$> clockMod)

  domainDiff = domainDiffCounterExt sysClk clockControlReset rxClock txClock

-- | Top entity for this test. See module documentation for more information.
fullMeshHwCcTest ::
  "PCIE_CLK_Q0" ::: DiffClock Ext200A ->
  "SMA_MGT_REFCLK_C" ::: DiffClock Ext200B ->
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
  , "" :::
      ( "FINC_0"      ::: Signal Basic125 Bool
      , "FDEC_0"      ::: Signal Basic125 Bool
      )
  , "" :::
      ( "FINC_1"      ::: Signal Basic125 Bool
      , "FDEC_1"      ::: Signal Basic125 Bool
      )
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
fullMeshHwCcTest
  refClkDiff0 refClkDiff1 sysClkDiff
  rxns0 rxps0
  rxns1 rxps1
  miso0 miso1
  = ( txns0, txps0
    , txns1, txps1
    , unbundle hwFincFdecs0
    , unbundle hwFincFdecs1
    , spiDone0 .&&. spiDone1
    , spiOut0
    , spiOut1
    )
 where
  refClk0 = ibufds_gte3 refClkDiff0 :: Clock Ext200A
  refClk1 = ibufds_gte3 refClkDiff1 :: Clock Ext200B

  (sysClk, sysRst) = clockWizardDifferential sysClkDiff noReset
  ilaControl0 = ilaPlotSetup IlaPlotSetup {allReady = allReady0, ..}
  ilaControl1 = ilaPlotSetup IlaPlotSetup {allReady = allReady1, ..}
  syncIn = syncOut ilaControl0

  (   txns0, txps0, hwFincFdecs0, _callistoResult0, _callistoReset0
    , _dataCounts0, _stats0, spiDone0, spiOut0, transceiversFailedAfterUp0, allReady0
    , allStable0 ) = fullMeshHwTest @Ext200A @GthRxA @GthTxA
                       "X0Y10" "clk0" (SNat @1)
                       refClk0 sysClk ilaControl0 rxns0 rxps0 miso0

  (   txns1, txps1, hwFincFdecs1, _callistoResult1, _callistoReset1
    , _dataCounts1, _stats1, spiDone1, spiOut1, transceiversFailedAfterUp1, allReady1
    , allStable1 ) = fullMeshHwTest @Ext200B @GthRxB @GthTxB
                       "X0Y9" "clk0-1" (SNat @2)
                       refClk1 sysClk ilaControl1 rxns1 rxps1 miso1

  -- check that tests are not synchronously start before all
  -- transceivers are up
  startBeforeAllReady0 = sticky sysClk (syncRst ilaControl0)
    (syncStart ilaControl0 .&&. ((not <$> allReady0) .||. transceiversFailedAfterUp0))

  startBeforeAllReady1 = sticky sysClk (syncRst ilaControl1)
    (syncStart ilaControl1 .&&. ((not <$> allReady1) .||. transceiversFailedAfterUp1))

  endSuccess :: Signal Basic125 Bool
  endSuccess = trueFor (SNat @(Seconds 5)) sysClk (syncRst ilaControl0) allStable0
          .&&. trueFor (SNat @(Seconds 5)) sysClk (syncRst ilaControl1) allStable1

  startTest :: Signal Basic125 Bool
  startTest =
    hitlVioBool
      sysClk

      -- done
      (    endSuccess      .||. transceiversFailedAfterUp0
      .||. startBeforeAllReady0
      .||. transceiversFailedAfterUp1
      .||. startBeforeAllReady1
      )

      -- success
      (not <$>
         (    transceiversFailedAfterUp0
         .||. startBeforeAllReady0
         .||. transceiversFailedAfterUp1
         .||. startBeforeAllReady1
         )
      )

makeTopEntity 'fullMeshHwCcTest

tests :: HitlTestsWithPostProcData () SimConf
tests = Map.singleton "CC" $
  ( allFpgas ()
  , def { mTopologyType      = Just $ Complete (natToInteger @FpgaCount)
        , samples            = 1000
        , duration           = natToNum @(PeriodToCycles Basic125 (Seconds 60))
        , stabilityMargin    = snatToNum cccStabilityCheckerMargin
        , stabilityFrameSize = snatToNum cccStabilityCheckerFramesize
        , reframe            = cccEnableReframing
        , rusty              = cccEnableRustySimulation
        , waitTime           = fromEnum cccReframingWaitTime
        , clockOffsets       = toList $ repeat @FpgaCount 0
        , startupDelays      = toList $ repeat @FpgaCount 0
        }
  )
 where
  ClockControlConfig{..} = clockControlConfig
