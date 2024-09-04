-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=20 #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

{- | Test whether clock boards are configurable and transceiver links come
online. If they do, run clock control in software and wait for the clocks to
stabilize. This assumes to run on a fully connected mesh of 8 FPGAs. Also see
'Bittide.Instances.Hitl.Setup'. It has two tricks up its sleeve:

  1. It uses @SYNC_IN@/@SYNC_OUT@ to make sure each board starts programming
     its clock boards at the same time.

  2. It keeps track of how many times the GTH's reset manager had to reset
     the connection and how often it lost connections after establishing
     them.

This test will succeed if all clocks have been stable for 5 seconds. Note:
this doesn't test reframing yet.
-}
module Bittide.Instances.Hitl.FullMeshSwCc (
  fullMeshSwCcTest,
  clockControlConfig,
  tests,
) where

import Clash.Explicit.Prelude hiding (PeriodToCycles)
import qualified Clash.Explicit.Prelude as E
import Clash.Prelude (withClockResetEnable)
import qualified Prelude as P

import Data.Maybe (fromMaybe)
import Data.Proxy
import Language.Haskell.TH (runIO)
import LiftType (liftTypeQ)
import System.FilePath

import Bittide.Arithmetic.Time
import Bittide.ClockControl
import Bittide.ClockControl.Callisto
import Bittide.ClockControl.Callisto.Util (FDEC, FINC, speedChangeToPins, stickyBits)
import Bittide.ClockControl.Registers (clockControlWb)
import Bittide.ClockControl.Si539xSpi (ConfigState (Error, Finished), si539xSpi)
import Bittide.Counter
import Bittide.DoubleBufferedRam (ContentType (Blob), InitialContent (Reloadable))
import Bittide.ElasticBuffer (Overflow, Underflow, resettableXilinxElasticBuffer, sticky)
import Bittide.Hitl (HitlTestsWithPostProcData, allFpgas, hitlVioBool)
import Bittide.Instances.Domains
import Bittide.ProcessingElement (PeConfig (..), processingElement)
import Bittide.ProcessingElement.Util (memBlobsFromElf)
import Bittide.SharedTypes (ByteOrder (BigEndian))
import Bittide.Simulate.Config (CcConf (..))
import Bittide.Topology (TopologyType (..))
import Bittide.Transceiver (transceiverPrbsN)

import Bittide.Instances.Hitl.HwCcTopologies (commonSpiConfig)
import Bittide.Instances.Hitl.IlaPlot
import Bittide.Instances.Hitl.Setup hiding (FpgaCount, LinkCount)
import Project.FilePath

import Clash.Annotations.TH (makeTopEntity)
import Clash.Class.Counter
import Clash.Cores.Xilinx.GTH
import Clash.Cores.Xilinx.Ila (Depth (..), IlaConfig (..), ila, ilaConfig)
import Clash.Cores.Xilinx.Xpm.Cdc (xpmCdcSingle)
import Clash.Cores.Xilinx.Xpm.Cdc.Handshake.Extra (xpmCdcMaybeLossy)
import Clash.Sized.Extra (unsignedToSigned)
import Clash.Sized.Vector.ToTuple (vecToTuple)
import Clash.Xilinx.ClockGen

import Protocols
import VexRiscv

import qualified Bittide.Transceiver as Transceiver
import qualified Bittide.Transceiver.ResetManager as ResetManager
import qualified Data.Map as Map
import Data.String (fromString)

type FpgaCount = 8
type LinkCount = FpgaCount - 1

clockControlConfig ::
  $(case (instancesClockConfig (Proxy @Basic125)) of (_ :: t) -> liftTypeQ @t)
clockControlConfig =
  $(lift (instancesClockConfig (Proxy @Basic125)))

-- | Instantiates a RiscV core
fullMeshRiscvTest ::
  forall dom.
  (KnownDomain dom) =>
  Clock dom ->
  Reset dom ->
  Vec LinkCount (Signal dom (RelDataCount 32)) ->
  -- Freq increase / freq decrease request to clock board
  ( "FINC" ::: Signal dom Bool
  , "FDEC" ::: Signal dom Bool
  )
fullMeshRiscvTest clk rst dataCounts = unbundle fIncDec
 where
  (_, fIncDec) =
    toSignals
      ( circuit $ \jtag -> do
          [wbB] <- withClockResetEnable clk rst enableGen $ processingElement @dom peConfig -< jtag
          (fIncDec, _allStable) <-
            withClockResetEnable clk rst enableGen
              $ clockControlWb margin framesize (pure $ complement 0) dataCounts
              -< wbB
          idC -< fIncDec
      )
      (pure $ JtagIn low low low, pure ())

  margin = d2

  framesize = SNat @(PeriodToCycles dom (Seconds 1))

  (iMem, dMem) =
    $( do
        root <- runIO $ findParentContaining "cabal.project"
        let
          elfDir = root </> firmwareBinariesDir "riscv32imc-unknown-none-elf" Release
          elfPath = elfDir </> "clock-control"
          iSize = 64 * 1024 -- 64 KB
          dSize = 64 * 1024 -- 64 KB
        memBlobsFromElf BigEndian (Just iSize, Just dSize) elfPath Nothing
     )

  {-
    0b10xxxxx_xxxxxxxx 0b10 0x8x instruction memory
    0b01xxxxx_xxxxxxxx 0b01 0x4x data memory
    0b11xxxxx_xxxxxxxx 0b11 0xCx memory mapped hardware clock control
  -}
  peConfig =
    PeConfig
      (0b10 :> 0b01 :> 0b11 :> Nil)
      (Reloadable $ Blob iMem)
      (Reloadable $ Blob dMem)

type FifoSize = 5 -- = 2^5 = 32

-- | Instantiates a hardware implementation of Callisto and exports its results.
fullMeshHwTest ::
  "SMA_MGT_REFCLK_C" ::: Clock Ext200 ->
  "SYSCLK" ::: Clock Basic125 ->
  "ILA_CTRL" ::: IlaControl Basic125 ->
  "GTH_RX_NS" ::: TransceiverWires GthRxS LinkCount ->
  "GTH_RX_PS" ::: TransceiverWires GthRxS LinkCount ->
  "MISO" ::: Signal Basic125 Bit ->
  ( "GTH_TX_NS" ::: TransceiverWires GthTxS LinkCount
  , "GTH_TX_PS" ::: TransceiverWires GthTxS LinkCount
  , "FINC_FDEC" ::: Signal Basic125 (FINC, FDEC)
  , "CALLISTO_RESULT" ::: Signal Basic125 (CallistoResult LinkCount)
  , "CALLISTO_RESET" ::: Reset Basic125
  , "DATA_COUNTERS" ::: Vec LinkCount (Signal Basic125 (RelDataCount 32))
  , "stats" ::: Vec LinkCount (Signal Basic125 ResetManager.Statistics)
  , "spiDone" ::: Signal Basic125 Bool
  , ""
      ::: ( "SCLK" ::: Signal Basic125 Bool
          , "MOSI" ::: Signal Basic125 Bit
          , "CSB" ::: Signal Basic125 Bool
          )
  , "transceiversFailedAfterUp" ::: Signal Basic125 Bool
  , "ALL_UP" ::: Signal Basic125 Bool
  , "ALL_STABLE" ::: Signal Basic125 Bool
  , "ugnsStable" ::: Vec LinkCount (Signal Basic125 Bool)
  )
fullMeshHwTest refClk sysClk IlaControl{syncRst = rst, ..} rxNs rxPs miso =
  fincFdecIla
    `hwSeqX` ( transceivers.txNs
             , transceivers.txPs
             , frequencyAdjustments
             , callistoResult
             , clockControlReset
             , domainDiffs
             , transceivers.stats
             , spiDone
             , spiOut
             , transceiversFailedAfterUp
             , allReady
             , allStable1
             , map (fmap (\(_, _, x, _) -> x)) freeUgnDatas
             )
 where
  syncRst = rst `orReset` (unsafeFromActiveLow (fmap not spiErr))

  -- Clock programming
  spiDone = E.dflipflop sysClk $ (== Finished) <$> spiState
  spiErr = E.dflipflop sysClk $ isErr <$> spiState

  isErr (Error _) = True
  isErr _ = False

  (_, _, spiState, spiOut) =
    withClockResetEnable sysClk syncRst enableGen
      $ si539xSpi commonSpiConfig (SNat @(Microseconds 10)) (pure Nothing) miso

  -- Transceiver setup
  gthAllReset = unsafeFromActiveLow spiDone

  transceivers =
    transceiverPrbsN
      @GthTx
      @GthRx
      @Ext200
      @Basic125
      @GthTxS
      @GthRxS
      @LinkCount
      Transceiver.defConfig
      Transceiver.Inputs
        { clock = sysClk
        , reset = gthAllReset
        , refClock = refClk
        , channelNames = takeI channelNames
        , clockPaths = takeI clockPaths
        , rxNs
        , rxPs
        , txDatas = txCounters
        , txReadys = txAllStables
        , rxReadys = repeat (pure True)
        }
  txAllStables = zipWith (xpmCdcSingle sysClk) transceivers.txClocks (repeat allStable1)
  allStable1 = sticky sysClk syncRst allStable0
  txResets2 =
    zipWith
      orReset
      transceivers.txResets
      (map unsafeFromActiveLow txAllStables)

  txCounters = zipWith txCounter transceivers.txClocks txResets2
  txCounter txClk txRst = result
   where
    result = register txClk txRst enableGen (0xaabbccddeeff1234 :: BitVector 64) (result + 1)
  -- see NOTE [magic start values]

  -- rxFifos :: Vec LinkCount (_, _, _, _, _Signal GthRx (Maybe (BitVector 64)))
  rxFifos =
    zipWith4
      go
      transceivers.txClocks
      transceivers.rxClocks
      txResets2
      transceivers.rxDatas
   where
    go rClk wClk rRst =
      resettableXilinxElasticBuffer @FifoSize @_ @_ @(Maybe (BitVector 64)) rClk wClk rRst

  (fillLvls, fifoUnderflowsTx, fifoOverflowsTx, _ebMode, rxCntrs) = unzip5 rxFifos

  fifoOverflowsFree :: Vec LinkCount (Signal Basic125 Overflow)
  fifoOverflowsFree = zipWith (flip xpmCdcSingle sysClk) transceivers.txClocks fifoOverflowsTx
  fifoUnderflowsFree :: Vec LinkCount (Signal Basic125 Underflow)
  fifoUnderflowsFree = zipWith (flip xpmCdcSingle sysClk) transceivers.txClocks fifoUnderflowsTx

  ugns :: Vec LinkCount (Signal GthTx (BitVector 64))
  ugns =
    zipWith
      (-)
      txCounters
      (map (fmap (fromMaybe 0x1122334411223344)) rxCntrs)
  -- see NOTE [magic start values]

  -- NOTE [magic start values]
  -- These values could be anything, but are chosen to be recognisable and help debugging.
  --   0xaabbccddeeff1234 - 0x1122334411223344 = 0x99999999dddcdef0
  -- If you ever see the ugn being a constant 0x99999999dddcdef0
  -- then you know the your counter isn't running and you're receiving 'Nothing',
  -- If you see 0x99999999.......... and it's counting up, then you're receiving Nothing,
  -- but your counter is running.

  ugnStable1sec = zipWith3 (stableForMs (SNat @1000)) transceivers.txClocks transceivers.txResets ugns

  freeUgnDatas = zipWith5 go transceivers.txClocks (repeat sysClk) ugns fillLvls ugnStable1sec
   where
    go clkIn clkOut ugn fillLvl stable =
      regMaybe
        clkOut
        noReset
        enableGen
        (0, 0, False, unpack 0)
        (xpmCdcMaybeLossy clkIn clkOut inp)
     where
      fillStat = fillStats clkIn noReset fillLvl
      inp = (fmap Just $ bundle (ugn, fillLvl, stable, fillStat))

  ( ugnD0
    , ugnD1
    , ugnD2
    , ugnD3
    , ugnD4
    , ugnD5
    , ugnD6
    ) = vecToTuple freeUgnDatas
  (ugn0, fill0, ugnStable0, fillStats0) = unbundle ugnD0
  (ugn1, fill1, ugnStable1, fillStats1) = unbundle ugnD1
  (ugn2, fill2, ugnStable2, fillStats2) = unbundle ugnD2
  (ugn3, fill3, ugnStable3, fillStats3) = unbundle ugnD3
  (ugn4, fill4, ugnStable4, fillStats4) = unbundle ugnD4
  (ugn5, fill5, ugnStable5, fillStats5) = unbundle ugnD5
  (ugn6, fill6, ugnStable6, fillStats6) = unbundle ugnD6

  FillStats fillMin0 fillMax0 = unbundle fillStats0
  FillStats fillMin1 fillMax1 = unbundle fillStats1
  FillStats fillMin2 fillMax2 = unbundle fillStats2
  FillStats fillMin3 fillMax3 = unbundle fillStats3
  FillStats fillMin4 fillMax4 = unbundle fillStats4
  FillStats fillMin5 fillMax5 = unbundle fillStats5
  FillStats fillMin6 fillMax6 = unbundle fillStats6

  allReady =
    trueFor (SNat @(Milliseconds 500)) sysClk syncRst (and <$> bundle transceivers.linkReadys)
  transceiversFailedAfterUp =
    sticky sysClk syncRst (isFalling sysClk syncRst enableGen False allReady)

  timeSucc = countSucc @(Unsigned 16, Index (PeriodToCycles Basic125 (Milliseconds 1)))
  timer = register sysClk syncRst enableGen (0, 0) (timeSucc <$> timer)
  milliseconds1 = fst <$> timer

  -- Clock control
  clockControlReset =
    orReset (unsafeFromActiveLow allReady)
      $ orReset
        (unsafeFromActiveHigh transceiversFailedAfterUp)
        (unsafeFromActiveLow syncStart)

  availableLinkMask = pure maxBound

  (clockMod, stabilities, allStable0, _allCentered) =
    unbundle
      $ fmap
        (\CallistoResult{..} -> (maybeSpeedChange, stability, allStable, allSettled))
        callistoResult
  ( stability0
    , stability1
    , stability2
    , stability3
    , stability4
    , stability5
    , stability6
    ) = vecToTuple $ unbundle stabilities

  callistoResult =
    callistoClockControlWithIla @LinkCount @CccBufferSize
      (head transceivers.txClocks)
      sysClk
      clockControlReset
      clockControlConfig
      IlaControl{..}
      availableLinkMask
      (fmap (fmap resize) domainDiffs)

  -- Capture every 100 microseconds - this should give us a window of about 5
  -- seconds. Or: when we're in reset. If we don't do the latter, the VCDs get
  -- very confusing.
  capture = (captureFlag .&&. allReady) .||. unsafeToActiveHigh syncRst

  fincFdecIla :: Signal Basic125 ()
  fincFdecIla =
    setName @"fincFdecIla"
      $ ila
        ( ilaConfig
            $ "trigger_0"
            :> "capture_0"
            :> "probe_milliseconds"
            :> "probe_allStable0"
            :> "probe_allStable1"
            :> "probe_transceiversFailedAfterUp"
            :> "probe_nFincs"
            :> "probe_nFdecs"
            :> "probe_net_nFincs"
            :> "probe_ugn0"
            :> "probe_ugn1"
            :> "probe_ugn2"
            :> "probe_ugn3"
            :> "probe_ugn4"
            :> "probe_ugn5"
            :> "probe_ugn6"
            :> "probe_fill0"
            :> "probe_fill2"
            :> "probe_fill1"
            :> "probe_fill3"
            :> "probe_fill4"
            :> "probe_fill5"
            :> "probe_fill6"
            :> "probe_fillMin0"
            :> "probe_fillMin2"
            :> "probe_fillMin1"
            :> "probe_fillMin3"
            :> "probe_fillMin4"
            :> "probe_fillMin5"
            :> "probe_fillMin6"
            :> "probe_fillMax0"
            :> "probe_fillMax2"
            :> "probe_fillMax1"
            :> "probe_fillMax3"
            :> "probe_fillMax4"
            :> "probe_fillMax5"
            :> "probe_fillMax6"
            :> "stability0"
            :> "stability2"
            :> "stability1"
            :> "stability3"
            :> "stability4"
            :> "stability5"
            :> "stability6"
            :> "ugnStable0"
            :> "ugnStable1"
            :> "ugnStable2"
            :> "ugnStable3"
            :> "ugnStable4"
            :> "ugnStable5"
            :> "ugnStable6"
            :> "probe_linkReadys"
            :> "probe_linkUps"
            :> "fifoUnderflows"
            :> "fifoOverflows"
            :> Nil
        )
          { depth = D16384
          }
        sysClk
        -- Trigger as soon as we come out of reset
        (unsafeToActiveLow syncRst)
        capture
        -- Debug probes
        milliseconds1
        allStable0
        allStable1
        transceiversFailedAfterUp
        nFincs
        nFdecs
        (fmap unsignedToSigned nFincs - fmap unsignedToSigned nFdecs)
        ugn0
        ugn1
        ugn2
        ugn3
        ugn4
        ugn5
        ugn6
        fill0
        fill1
        fill2
        fill3
        fill4
        fill5
        fill6
        fillMin0
        fillMin1
        fillMin2
        fillMin3
        fillMin4
        fillMin5
        fillMin6
        fillMax0
        fillMax1
        fillMax2
        fillMax3
        fillMax4
        fillMax5
        fillMax6
        stability0
        stability1
        stability2
        stability3
        stability4
        stability5
        stability6
        ugnStable0
        ugnStable1
        ugnStable2
        ugnStable3
        ugnStable4
        ugnStable5
        ugnStable6
        (bundle transceivers.linkReadys)
        (bundle transceivers.linkUps)
        ((pack . reverse) <$> bundle fifoUnderflowsFree)
        ((pack . reverse) <$> bundle fifoOverflowsFree)

  captureFlag =
    riseEvery
      sysClk
      syncRst
      enableGen
      (SNat @(PeriodToCycles Basic125 (Milliseconds 1)))

  nFincs =
    regEn
      sysClk
      clockControlReset
      enableGen
      (0 :: Unsigned 32)
      ((== Just SpeedUp) <$> clockMod)
      (satSucc SatBound <$> nFincs)

  nFdecs =
    regEn
      sysClk
      clockControlReset
      enableGen
      (0 :: Unsigned 32)
      ((== Just SlowDown) <$> clockMod)
      (satSucc SatBound <$> nFdecs)

  frequencyAdjustments :: Signal Basic125 (FINC, FDEC)
  frequencyAdjustments =
    E.delay sysClk enableGen minBound {- glitch filter -}
      $ withClockResetEnable sysClk clockControlReset enableGen
      $ stickyBits @Basic125 d20 (speedChangeToPins . fromMaybe NoChange <$> clockMod)

  domainDiffs =
    domainDiffCounterExt sysClk clockControlReset
      <$> transceivers.rxClocks
      <*> transceivers.txClocks

{- | Tracks the min/max values of the input during the last milliseconds

Updates once per millisecond.
-}
fillStats ::
  forall dom a.
  (KnownDomain dom, Ord a, Num a, Bounded a, NFDataX a) =>
  Clock dom ->
  Reset dom ->
  Signal dom a ->
  Signal dom (FillStats a)
fillStats clk rst = moore clk rst enableGen go (\(_, _, x) -> x) (maxBound, mempty, mempty)
 where
  go ::
    (IndexMs dom 1, FillStats a, FillStats a) ->
    a ->
    (IndexMs dom 1, FillStats a, FillStats a)
  go (cntr, prevStats, out) inp
    | cntr == 0 = (maxBound, mempty, new)
    | otherwise = (cntr - 1, new, out)
   where
    new = mappend prevStats (FillStats inp inp)

data FillStats a = FillStats {fillMin :: a, fillMax :: a}
  deriving (Generic, NFDataX, BitPack)

instance Bundle (FillStats a) where
  type Unbundled dom (FillStats a) = FillStats (Signal dom a)
  bundle (FillStats sigMin sigMax) = liftA2 FillStats sigMin sigMax
  unbundle x = FillStats{fillMin = fmap fillMin x, fillMax = fmap fillMax x}

instance (Ord a) => Semigroup (FillStats a) where
  a <> b =
    FillStats
      { fillMin = min (fillMin a) (fillMin b)
      , fillMax = max (fillMax a) (fillMax b)
      }

instance (Bounded a, Ord a) => Monoid (FillStats a) where
  mempty = FillStats{fillMin = maxBound, fillMax = minBound}

{- | Counts how many cycles the input signal has been stable

Stable means equal to its previous value according to the 'Eq' instance.
The 'BitPack' instance is only used as a convenient way of intialization,
it resets to a previous value of @unpack 0@.
-}
stableFor ::
  forall n dom a.
  (KnownNat n, KnownDomain dom, Eq a, BitPack a, NFDataX a) =>
  Clock dom ->
  Reset dom ->
  Signal dom a ->
  Signal dom (Unsigned n)
stableFor clk rst = moore clk rst enableGen go snd (unpack 0, 0)
 where
  go :: (a, Unsigned n) -> a -> (a, Unsigned n)
  go (prev, cntr) inp
    | inp == prev = (prev, satSucc SatBound cntr)
    | otherwise = (inp, 0)

-- | Wrapper around 'stableFor' that checks the input has been stable for atleast @ms@ milliseconds
stableForMs ::
  forall ms dom a.
  (KnownNat ms, KnownDomain dom, Eq a, BitPack a, NFDataX a) =>
  SNat ms ->
  Clock dom ->
  Reset dom ->
  Signal dom a ->
  Signal dom Bool
stableForMs SNat clk rst inp =
  liftA2 (>=) stable (snatToNum (SNat @(PeriodToCycles dom (Milliseconds ms))))
 where
  stable = stableFor @(CLog 2 (PeriodToCycles dom (Milliseconds ms))) clk rst inp

-- | Top entity for this test. See module documentation for more information.
fullMeshSwCcTest ::
  "SMA_MGT_REFCLK_C" ::: DiffClock Ext200 ->
  "SYSCLK_300" ::: DiffClock Ext300 ->
  "SYNC_IN" ::: Signal Basic125 Bool ->
  "GTH_RX_NS" ::: TransceiverWires GthRxS LinkCount ->
  "GTH_RX_PS" ::: TransceiverWires GthRxS LinkCount ->
  "MISO" ::: Signal Basic125 Bit ->
  ( "GTH_TX_NS" ::: TransceiverWires GthTxS LinkCount
  , "GTH_TX_PS" ::: TransceiverWires GthTxS LinkCount
  , ""
      ::: ( "FINC" ::: Signal Basic125 Bool
          , "FDEC" ::: Signal Basic125 Bool
          )
  , "SYNC_OUT" ::: Signal Basic125 Bool
  , "spiDone" ::: Signal Basic125 Bool
  , ""
      ::: ( "SCLK" ::: Signal Basic125 Bool
          , "MOSI" ::: Signal Basic125 Bit
          , "CSB" ::: Signal Basic125 Bool
          )
  )
fullMeshSwCcTest refClkDiff sysClkDiff syncIn rxns rxps miso =
  (txns, txps, (riscvFinc, riscvFdec), syncOut, spiDone, spiOut)
 where
  refClk = ibufds_gte3 refClkDiff :: Clock Ext200
  (sysClk, sysRst) = clockWizardDifferential sysClkDiff noReset
  ilaControl@IlaControl{syncRst, syncOut, syncStart} = ilaPlotSetup IlaPlotSetup{..}

  ( txns
    , txps
    , _hwFincFdecs
    , _callistoResult
    , callistoReset
    , dataCounts
    , _stats
    , spiDone
    , spiOut
    , transceiversFailedAfterUp
    , allReady
    , allStable
    , ugnsStable
    ) = fullMeshHwTest refClk sysClk ilaControl rxns rxps miso

  (riscvFinc, riscvFdec) =
    fullMeshRiscvTest sysClk callistoReset dataCounts

  allUgnsStable = fmap and $ bundle ugnsStable

  -- checks that tests are not synchronously start before all
  -- transceivers are up
  startBeforeAllReady =
    sticky
      sysClk
      syncRst
      (syncStart .&&. ((not <$> allReady) .||. transceiversFailedAfterUp))

  endSuccess :: Signal Basic125 Bool
  endSuccess = trueFor (SNat @(Seconds 5)) sysClk syncRst $ allStable .&&. allUgnsStable

  startTest :: Signal Basic125 Bool
  startTest =
    hitlVioBool
      sysClk
      -- done
      (endSuccess .||. transceiversFailedAfterUp .||. startBeforeAllReady)
      -- success
      (allUgnsStable .&&. not <$> (transceiversFailedAfterUp .||. startBeforeAllReady))

makeTopEntity 'fullMeshSwCcTest

testsToRun :: Int
testsToRun = 1

tests :: HitlTestsWithPostProcData () CcConf
tests =
  Map.fromList
    $ P.zip ["CC" <> fromString (show n) | n <- [0 .. testsToRun - 1]]
    $ P.repeat
      ( allFpgas ()
      , def
          { ccTopologyType = Complete (natToInteger @FpgaCount)
          , samples = 1000
          , duration = natToNum @(PeriodToCycles Basic125 (Seconds 60))
          , stabilityMargin = snatToNum cccStabilityCheckerMargin
          , stabilityFrameSize = snatToNum cccStabilityCheckerFramesize
          , reframe = cccEnableReframing
          , waitTime = fromEnum cccReframingWaitTime
          , clockOffsets = Nothing
          , startupDelays = toList $ repeat @FpgaCount 0
          }
      )
 where
  ClockControlConfig{..} = clockControlConfig
