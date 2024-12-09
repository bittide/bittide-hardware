-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=20 #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

{- | Tests whether the transceiver link setup matches with the
configuration defined in 'Bittide.Instances.Hitl.Setup.fpgaSetup'.
To this end, each node sends its own index to all of it's
neighbours, which then verify that the result matches with the
definition.
-}
module Bittide.Instances.Hitl.LinkConfiguration (
  linkConfigurationTest,
  tests,
) where

import Clash.Explicit.Prelude
import qualified Clash.Explicit.Prelude as E
import Clash.Prelude (withClockResetEnable)

import qualified Data.Map.Strict as Map (fromList)

import Bittide.Arithmetic.Time
import Bittide.ClockControl.Si5395J
import Bittide.ClockControl.Si539xSpi (ConfigState (Error, Finished), si539xSpi)
import Bittide.ElasticBuffer (sticky)
import Bittide.Instances.Domains
import Bittide.Transceiver

import Bittide.Hitl

import Bittide.Instances.Hitl.Setup

import Clash.Annotations.TH (makeTopEntity)
import Clash.Cores.Xilinx.GTH
import Clash.Cores.Xilinx.Xpm.Cdc.Handshake.Extra
import Clash.Cores.Xilinx.Xpm.Cdc.Single
import Clash.Xilinx.ClockGen
import Data.Maybe (fromMaybe, isJust)

import qualified Bittide.Transceiver as Transceiver
import qualified Bittide.Transceiver.ResetManager as ResetManager

{- | Checks whether the received index matches with the corresponding
entry in 'Bittide.Instances.Hitl.Setup.fpgaSetup' and sychronizes
to the right clock domain accordingly.
-}
checkData ::
  forall n src dst.
  (KnownDomain src, KnownDomain dst, KnownNat n, 1 <= n, BitSize (Index n) <= 64) =>
  Clock dst ->
  Clock src ->
  Signal dst Bool ->
  Signal src (Maybe (BitVector 64)) ->
  Signal src (Maybe (Index n)) ->
  Signal dst Bool
checkData dstClk srcClk ready rx setup =
  ready .&&. xpmCdcSingle srcClk dstClk (match <$> rx <*> setup)
 where
  match ma mb = fromMaybe False (ma .==. (zExtend . pack <$> mb))
  zExtend = zeroExtend @_ @(BitSize (Index n)) @(64 - BitSize (Index n))

{- | Extracts the corresponding target FPGA index from
'Bittide.Instances.Hitl.Setup.fpgaSetup' according to the given
link index and synchronizes it to the provided clock domain.
-}
expectedTargetIndex ::
  (KnownDomain src, KnownDomain dst) =>
  Clock src ->
  Signal src (Index FpgaCount) ->
  Clock dst ->
  Reset dst ->
  Index LinkCount ->
  Signal dst (Maybe (Index FpgaCount))
expectedTargetIndex srcClk myIndex dstClk dstRst link =
  fmap ((!! link) . snd . (fpgaSetup !!))
    <$> xpmCdcStable srcClk myIndex dstClk dstRst

{- | Synchronizes the fixed FPGA index from some given source domain
to the given target domain. Lossy synchronization is sufficient
here, as the index is considered to be stable stable once the test
has been started.
-}
xpmCdcStable ::
  ( KnownDomain src
  , KnownDomain dst
  , BitPack a
  , NFDataX a
  , 1 <= BitSize a
  , BitSize a <= 1024
  ) =>
  Clock src ->
  Signal src a ->
  Clock dst ->
  Reset dst ->
  Signal dst (Maybe a)
xpmCdcStable srcClk idx dstClk dstRst = mIdx
 where
  mIdx =
    register dstClk dstRst enableGen Nothing
      $ (<|>)
      <$> xpmCdcMaybeLossy srcClk dstClk (pure <$> idx)
      <*> mIdx

{- | Configures the clock boards, fires up all of the transceivers and
observes the incoming links.
-}
transceiversStartAndObserve ::
  "SMA_MGT_REFCLK_C" ::: Clock Ext200 ->
  "SYSCLK" ::: Clock Basic125 ->
  "RST_LOCAL" ::: Reset Basic125 ->
  "MY_INDEX" ::: Signal Basic125 (Index FpgaCount) ->
  "GTH_RX_NS" ::: TransceiverWires GthRxS LinkCount ->
  "GTH_RX_PS" ::: TransceiverWires GthRxS LinkCount ->
  "MISO" ::: Signal Basic125 Bit ->
  ( "GTH_TX_NS" ::: TransceiverWires GthTxS LinkCount
  , "GTH_TX_PS" ::: TransceiverWires GthTxS LinkCount
  , "allReady" ::: Signal Basic125 Bool
  , "success" ::: Signal Basic125 Bool
  , "stats" ::: Vec LinkCount (Signal Basic125 ResetManager.Statistics)
  , "spiDone" ::: Signal Basic125 Bool
  , ""
      ::: ( "SCLK" ::: Signal Basic125 Bool
          , "MOSI" ::: Signal Basic125 Bit
          , "CSB" ::: Signal Basic125 Bool
          )
  )
transceiversStartAndObserve refClk sysClk rst myIndex rxNs rxPs miso =
  ( transceivers.txNs
  , transceivers.txPs
  , allReady
  , success
  , transceivers.stats
  , spiDone
  , spiOut
  )
 where
  allReady = and <$> bundle transceivers.linkReadys
  sysRst = rst `orReset` unsafeFromActiveLow (fmap not spiErr)

  -- Clock programming
  spiDone = E.dflipflop sysClk $ (== Finished) <$> spiState
  spiErr = E.dflipflop sysClk $ isErr <$> spiState

  isErr = \case
    Error{} -> True
    _ -> False

  (_, _, spiState, spiOut) =
    withClockResetEnable sysClk sysRst enableGen
      $ si539xSpi
        testConfig6_200_on_0a_1ppb
        (SNat @(Microseconds 10))
        (pure Nothing)
        miso

  -- Transceiver setup
  transceivers =
    transceiverPrbsN
      @GthTx
      @GthRx
      @Ext200
      @Basic125
      @GthTxS
      @GthRxS
      Transceiver.defConfig
      Transceiver.Inputs
        { clock = sysClk
        , reset = rst `orReset` unsafeFromActiveLow spiDone
        , refClock = refClk
        , channelNames
        , clockPaths
        , rxNs
        , rxPs
        , txDatas = myIndexTxN
        , rxReadys = repeat $ pure True
        , txReadys = repeat $ pure True
        }

  -- synchronizes the FPGA's stable index to the individual TX clock
  -- domains of the transceivers
  myIndexTxN =
    fmap (zeroExtend . pack . fromMaybe 0)
      <$> map
        (xpmCdcStable sysClk myIndex transceivers.txClock)
        transceivers.txResets

  -- check that all the received data matches with our expectations
  success =
    fmap and
      $ bundle
      $ zipWith4
        (checkData @FpgaCount sysClk)
        transceivers.rxClocks
        transceivers.linkReadys
        transceivers.rxDatas
      $ zipWith3
        (expectedTargetIndex sysClk myIndex)
        transceivers.rxClocks
        transceivers.rxResets
        indicesI

-- | Top entity for this test. See module documentation for more information.
linkConfigurationTest ::
  "SMA_MGT_REFCLK_C" ::: DiffClock Ext200 ->
  "SYSCLK_125" ::: DiffClock Ext125 ->
  "SYNC_IN" ::: Signal Basic125 Bool ->
  "GTH_RX_NS" ::: TransceiverWires GthRxS LinkCount ->
  "GTH_RX_PS" ::: TransceiverWires GthRxS LinkCount ->
  "MISO" ::: Signal Basic125 Bit ->
  ( "GTH_TX_NS" ::: TransceiverWires GthTxS LinkCount
  , "GTH_TX_PS" ::: TransceiverWires GthTxS LinkCount
  , "SYNC_OUT" ::: Signal Basic125 Bool
  , "spiDone" ::: Signal Basic125 Bool
  , ""
      ::: ( "SCLK" ::: Signal Basic125 Bool
          , "MOSI" ::: Signal Basic125 Bit
          , "CSB" ::: Signal Basic125 Bool
          )
  )
linkConfigurationTest refClkDiff sysClkDiff syncIn rxns rxps miso =
  (txns, txps, syncOut, spiDone, spiOut)
 where
  refClk = ibufds_gte3 refClkDiff :: Clock Ext200
  (sysClk, sysRst) = clockWizardDifferential sysClkDiff noReset

  -- the test starts with a valid configuration coming in
  startTest = isJust <$> testConfig
  -- derive the test settings from the passed configuration
  myIndex = fromMaybe 0 <$> testConfig

  -- use some simple SYNC_IN / SYNC_OUT synchronization to
  -- synchronously start the test
  syncInRst =
    resetGlitchFilter (SNat @1024) sysClk
      $ unsafeFromActiveLow
      $ xpmCdcSingle sysClk sysClk syncIn
  syncOut = startTest

  testRst =
    sysRst
      `orReset` syncInRst
      `orReset` unsafeFromActiveLow startTest

  (txns, txps, allReady, success, _stats, spiDone, spiOut) =
    transceiversStartAndObserve refClk sysClk testRst myIndex rxns rxps miso

  failAfterUp = isFalling sysClk testRst enableGen False allReady
  failAfterUpSticky = sticky sysClk testRst failAfterUp

  -- Consider the test done if links have been up consistently for 40
  -- seconds. This is just below the test timeout of 60 seconds, so
  -- transceivers have ~20 seconds to come online reliably. This
  -- should be plenty. The test will stop immediately on success,
  -- i.e., if all neighbours have transmitted the expected ids
  -- alltogether at least once.
  done =
    success
      .||. failAfterUpSticky
      .||. trueFor (SNat @(Seconds 40)) sysClk testRst allReady

  testConfig :: Signal Basic125 (Maybe (Index FpgaCount))
  testConfig = hitlVio 0 sysClk done (success .&&. (not <$> failAfterUpSticky))

makeTopEntity 'linkConfigurationTest

tests :: HitlTestGroup
tests =
  HitlTestGroup
    { topEntity = 'linkConfigurationTest
    , extraXdcFiles = []
    , externalHdl = []
    , testCases =
        [ HitlTestCase
            { name = "LinkConfiguration"
            , parameters =
                Map.fromList
                  [ (HwTargetByIndex (fromIntegral i), i)
                  | i <- [0 ..] :: [Index FpgaCount]
                  ]
            , postProcData = ()
            }
        ]
    , mDriverProc = Nothing
    , mPostProc = Nothing
    }
