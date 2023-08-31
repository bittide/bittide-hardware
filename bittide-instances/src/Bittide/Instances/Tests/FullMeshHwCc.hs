-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
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
module Bittide.Instances.Tests.FullMeshHwCc where

import Clash.Prelude (withClockResetEnable, withClock)
import Clash.Explicit.Prelude
import Language.Haskell.TH

import Bittide.Arithmetic.Time
import Bittide.ClockControl
import Bittide.ClockControl.Callisto
import Bittide.ClockControl.Si5395J
import Bittide.ClockControl.Si539xSpi
import Bittide.ProcessingElement
import Bittide.ProcessingElement.Util
import Bittide.Counter
import Bittide.ElasticBuffer (sticky)
import Bittide.Instances.Domains
import Bittide.Transceiver
import Bittide.Wishbone
import Bittide.SharedTypes
import Project.FilePath
import System.FilePath
import Bittide.ClockControl.Registers
import Bittide.DoubleBufferedRam
import Bittide.Extra.Maybe

import Bittide.Instances.MVPs (stickyBits, speedChangeToPins, FINC, FDEC)

import Clash.Class.Counter
import Clash.Cores.Xilinx.GTH
import Clash.Cores.Xilinx.VIO (vioProbe)
import Clash.Cores.Xilinx.Xpm.Cdc.Handshake.Extra
import Clash.Cores.Xilinx.Xpm.Cdc.Single
import Clash.Cores.Xilinx.Xpm.Cdc.Gray
import Clash.Cores.Xilinx.Ila
import Clash.Explicit.Reset.Extra
import Clash.Sized.Extra (unsignedToSigned)
import Clash.Xilinx.ClockGen
import Protocols.Internal
import Data.Proxy

import qualified Clash.Explicit.Prelude as E
unitCS :: CSignal dom ()
unitCS = CSignal (pure ())


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

-- | Worker function for 'fullMeshHwCcTest'. See module documentation for more
-- information.
goFullMeshHwCcTest ::
  "SMA_MGT_REFCLK_C" ::: Clock Basic200 ->
  "SYSCLK" ::: Clock Basic125 ->
  "RST_LOCAL" ::: Reset Basic125 ->
  "GTH_RX_NS" ::: TransceiverWires GthRx ->
  "GTH_RX_PS" ::: TransceiverWires GthRx ->
  "MISO" ::: Signal Basic125 Bit ->
  ( "GTH_TX_NS" ::: TransceiverWires GthTx
  , "GTH_TX_PS" ::: TransceiverWires GthTx
  , "FINC_FDEC" ::: Signal GthTx (FINC, FDEC)
  , "FINC_FDEC" ::: Signal GthTx (FINC, FDEC)
  , "CALLISTO_CLOCK" ::: Clock GthTx
  , "CALLISTO_RESULT" ::: Signal GthTx (CallistoResult 7)
  , "CALLISTO_RESET" ::: Reset GthTx
  , "DATA_COUNTERS" ::: Vec 7 (Signal GthTx (DataCount 32))
  , "stats" ::: Vec 7 (Signal Basic125 GthResetStats)
  , "spiDone" ::: Signal Basic125 Bool
  , "" :::
      ( "SCLK" ::: Signal Basic125 Bool
      , "MOSI" ::: Signal Basic125 Bit
      , "CSB"  ::: Signal Basic125 Bool
      )
  , "transceiversFailedAfterUp" ::: Signal Basic125 Bool
  , "ALL_STABLE"   ::: Signal Basic125 Bool
  , "linkUps" ::: Vec 7 (Signal Basic125 Bool)
  )
goFullMeshHwCcTest refClk sysClk rst rxns rxps miso =
  fincFdecIla `hwSeqX`
  ( txns
  , txps
  , hardFrequencyAdjustments
  , softFrequencyAdjustments
  , txClock
  , callistoResult
  , clockControlReset
  , domainDiffs
  , stats
  , spiDone
  , spiOut
  , transceiversFailedAfterUp
  , fincFdecIla `hwSeqX` allStable1
  , linkUps
  )
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

  (head -> (txClock :: Clock GthTx), rxClocks, txns, txps, linkUpsRx, stats) = unzip6 $
    transceiverPrbsN
      @GthTx @GthRx @Basic200 @Basic125 @GthTx @GthRx
      refClk sysClk gthAllReset
      c_CHANNEL_NAMES c_CLOCK_PATHS rxns rxps

  syncLink rxClock linkUp = xpmCdcSingle rxClock sysClk linkUp
  linkUps = zipWith syncLink rxClocks linkUpsRx
  allUp = trueFor500ms sysClk sysRst (and <$> bundle linkUps)
  transceiversFailedAfterUp =
    sticky sysClk sysRst (isFalling sysClk sysRst enableGen False allUp)

  timeSucc = countSucc @(Unsigned 16, Index (PeriodToCycles Basic125 (Milliseconds 1)))
  timer = register sysClk rst enableGen (0, 0) (timeSucc <$> timer)
  milliseconds1 = fst <$> timer

  -- Callisto Clock control
  clockControlReset =
    xpmResetSynchronizer Asserted sysClk txClock $
      orReset (unsafeFromActiveLow allUp) (unsafeFromActiveHigh transceiversFailedAfterUp)
  clockConfig = $(lift (instancesClockConfig (Proxy @GthTx)))
  availableLinkMask = pure maxBound

  (speedChange1, _stabilities, allStable0, _allCentered) = unbundle $
    fmap (\CallistoResult{..} -> (speedChange, stability, allStable, allSettled)) callistoResult

  callistoResult =
    callistoClockControl
      @7 @25 @GthTx txClock clockControlReset enableGen
      clockConfig availableLinkMask $ fmap (fmap resize) domainDiffs

  allStable1 = xpmCdcSingle txClock sysClk allStable0

  dataCount0 :>
    dataCount1 :>
    dataCount2 :>
    dataCount3 :>
    dataCount4 :>
    dataCount5 :>
    dataCount6 :>
    Nil = (fmap truncateDataCount . dataCountCdc) <$> domainDiffs

  -- Capture every 100 microseconds - this should give us a window of about 5
  -- seconds. Or: when we're in reset. If we don't do the latter, the VCDs get
  -- very confusing.
  capture = captureFlag .||. unsafeToActiveHigh sysRst

  hardFrequencyAdjustments :: Signal GthTx (FINC, FDEC)
  hardFrequencyAdjustments =
    E.delay txClock enableGen minBound {- glitch filter -} $
      withClockResetEnable txClock clockControlReset enableGen $
        stickyBits @GthTx d20 (speedChangeToPins <$> speedChange1)

  -- Vex risc clock control
  (_, CSignal softFrequencyAdjustments) = toSignals
    ( circuit $ \ unit -> do
      [wbA, wbB] <- (withClockResetEnable txClock clockControlReset enableGen $ processingElement @GthTx peConfig) -< unit
      fIncDecCallisto <| withClock txClock ilaWb -< wbA
      fIncDec <- withClockResetEnable txClock clockControlReset enableGen $
        clockControlWb (cccStabilityCheckerMargin clockConfig) (cccStabilityCheckerFramesize clockConfig) (pure $ complement 0) domainDiffs -< wbB
      idC -< fIncDec
    ) ((), unitCS)

  fIncDecCallisto = Circuit goFIncDecCallisto
   where
    goFIncDecCallisto (wbM2S, _) = (wbS2M, ())
     where
      (_, wbS2M) = withClockResetEnable txClock clockControlReset enableGen $
        registerWb
          CircuitPriority
          (0 :: Signed 32, 0 :: Bytes 4)
          wbM2S
          (fmap (Just . (,0)) fincfdecCount)
      fincfdecCount = countfIncfDecs txClock clockControlReset enableGen $ unbundle $ speedChangeToPins . speedChange <$> callistoResult

  ( (_iStart, _iSize, iMem)
    , (_dStart, _dSize, dMem)) = $(do
      root <- runIO $ findParentContaining "cabal.project"
      let
        elfDir = root </> firmwareBinariesDir "riscv32imc-unknown-none-elf" True
        elfPath = elfDir </> "clock-control"
      memBlobsFromElf BigEndian elfPath Nothing)

  {-
    0b10xxxxx_xxxxxxxx 0b10 0x8x instruction memory
    0b01xxxxx_xxxxxxxx 0b01 0x4x data memory
    0b00xxxxx_xxxxxxxx 0b00 0x0x FINC/FDEC register
    0b11xxxxx_xxxxxxxx 0b11 0xCx memory mapped hardware clock control
  -}
  peConfig =
    PeConfig
      (0b10 :> 0b01 :> 0b00 :> 0b11 :> Nil)
      (Reloadable $ Blob iMem)
      (Reloadable $ Blob dMem)

  -- Data count processing
  truncateDataCount :: Signed 32 -> Signed 16
  truncateDataCount = truncateB

  dataCountCdc =
      regMaybe sysClk sysRst enableGen 0
    . xpmCdcMaybeLossy txClock sysClk
    . fmap Just

  (domainDiffs, _domainActives) =
    unzip $ fmap unbundle $ rxDiffCounter <$> rxClocks <*> linkUpsRx
  rxDiffCounter rxClk linkUp =
    domainDiffCounter rxClk (unsafeFromActiveLow linkUp) txClock clockControlReset

  -- Data capturing
  fincFdecIla :: Signal Basic125 ()
  fincFdecIla = ila
    (ilaConfig $
         "trigger"
      :> "capture"
      :> "probe_milliseconds"
      :> "probe_allStable1"
      :> "probe_transceiversFailedAfterUp"
      :> "probe_nFincs"
      :> "probe_nFdecs"
      :> "probe_net_nFincs"
      :> "probe_dataCount_0"
      :> "probe_dataCount_1"
      :> "probe_dataCount_2"
      :> "probe_dataCount_3"
      :> "probe_dataCount_4"
      :> "probe_dataCount_5"
      :> "probe_dataCount_6"
      :> Nil
    ){depth = D65536}
    sysClk

    -- Trigger as soon as we come out of reset
    (unsafeToActiveLow sysRst)

    capture

    -- Debug probes
    milliseconds1
    allStable1
    transceiversFailedAfterUp
    nFincsSynced
    nFdecsSynced
    (fmap unsignedToSigned nFincsSynced - fmap unsignedToSigned nFdecsSynced)
    dataCount0
    dataCount1
    dataCount2
    dataCount3
    dataCount4
    dataCount5
    dataCount6

  nFincsSynced = xpmCdcGray txClock sysClk nFincs
  nFdecsSynced = xpmCdcGray txClock sysClk nFdecs

  captureFlag = captureCounter .==. pure (maxBound :: Index (PeriodToCycles Basic125 (Milliseconds 1)))
  captureCounter = register sysClk sysRst enableGen 0 (satSucc SatWrap <$> captureCounter)

  isFinc = speedChange1 .==. pure SpeedUp
  nFincs = register txClock clockControlReset (toEnable isFinc) (0 :: Unsigned 32) (nFincs + 1)

  isFdec = speedChange1 .==. pure SlowDown
  nFdecs = register txClock clockControlReset (toEnable isFdec) (0 :: Unsigned 32) (nFdecs + 1)

-- | Returns 'True' if incoming signal has been 'True' for 500 ms
--
-- XXX: Type checker doesn't like it when we try to generalize this over periods
trueFor500ms ::
  forall dom .
  KnownDomain dom =>
  Clock dom ->
  Reset dom ->
  Signal dom Bool ->
  Signal dom Bool
trueFor500ms clk rst =
  moore clk rst enableGen goState goOutput (0 :: IndexMs dom 500)
 where
  goState counter  True  = satSucc SatBound counter
  goState _counter False = 0

  goOutput = (== maxBound)

-- | Returns 'True' if incoming signal has been 'True' for 5 s
--
-- XXX: Type checker doesn't like it when we try to generalize this over periods
trueFor5s ::
  forall dom .
  KnownDomain dom =>
  Clock dom ->
  Reset dom ->
  Signal dom Bool ->
  Signal dom Bool
trueFor5s clk rst =
  moore clk rst enableGen goState goOutput (0 :: IndexMs dom 5_000)
 where
  goState counter  True  = satSucc SatBound counter
  goState _counter False = 0

  goOutput = (== maxBound)

-- | Top entity for this test. See module documentation for more information.
fullMeshHwCcTest ::
  "SMA_MGT_REFCLK_C" ::: DiffClock Basic200 ->
  "SYSCLK_300" ::: DiffClock Basic300 ->
  "SYNC_IN" ::: Signal Basic125 Bool ->
  "GTH_RX_NS" ::: TransceiverWires GthRx ->
  "GTH_RX_PS" ::: TransceiverWires GthRx ->
  "MISO" ::: Signal Basic125 Bit ->
  ( "GTH_TX_NS" ::: TransceiverWires GthTx
  , "GTH_TX_PS" ::: TransceiverWires GthTx
  , "" :::
      ( "FINC"      ::: Signal GthTx Bool
      , "FDEC"      ::: Signal GthTx Bool
      )
  , "SYNC_OUT" ::: Signal Basic125 Bool
  , "spiDone" ::: Signal Basic125 Bool
  , "" :::
      ( "SCLK"      ::: Signal Basic125 Bool
      , "MOSI"      ::: Signal Basic125 Bit
      , "CSB"       ::: Signal Basic125 Bool
      )
  , "linkUps" ::: Vec 7 (Signal Basic125 Bool)
  )
fullMeshHwCcTest refClkDiff sysClkDiff syncIn rxns rxps miso =
  (txns, txps, frequencyAdjustments, syncOut, spiDone, spiOut, linkUps)
 where
  refClk = ibufds_gte3 refClkDiff :: Clock Basic200

  (sysClk, sysLock0) = clockWizardDifferential (SSymbol @"SysClk") sysClkDiff noReset
  sysLock1 = xpmCdcSingle sysClk sysClk sysLock0 -- improvised reset syncer
  sysRst = unsafeFromActiveLow sysLock1
  testRst = sysRst `orReset` unsafeFromActiveLow syncOut `orReset` syncInRst
  syncOut =  or <$> startTests
  syncInRst =
      resetGlitchFilter (SNat @1024) sysClk
    $ unsafeFromActiveLow
    $ xpmCdcSingle sysClk sysClk syncIn

  (txns, txps, hardFincFdecs, softFincFdecs, callistoClock, _, _, _, stats, spiDone, spiOut, transceiversFailedAfterUp, allStable, linkUps) =
    goFullMeshHwCcTest refClk sysClk testRst rxns rxps miso

  frequencyAdjustments = unbundle $ dflipflop callistoClock $
    fromMaybesL (False, False) <$>
    (zipWith orNothing <$> startTestsTx <*> bundle testIncDecs)
  testIncDecs = pure (False, False) :> hardFincFdecs :> softFincFdecs :> Nil

  testDone = trueFor5s sysClk testRst $ mux (head <$> startTests) (or <$> bundle linkUps) allStable
  stats0 :> stats1 :> stats2 :> stats3 :> stats4 :> stats5 :> stats6 :> Nil = stats

  done = testDone .||. transceiversFailedAfterUp
  success = fmap not transceiversFailedAfterUp

  startTestsTx = bundle $ fmap (xpmCdcSingle sysClk callistoClock) (unbundle startTests)
  startTests :: Signal Basic125 (Vec 3 Bool)
  startTests =
    setName @"vioHitlt" $
    vioProbe
      (  "probe_test_done"
      :> "probe_test_success"

      -- Debug probes
      :> "stats0_txRetries"
      :> "stats0_rxRetries"
      :> "stats0_rxFullRetries"
      :> "stats0_failAfterUps"
      :> "stats1_txRetries"
      :> "stats1_rxRetries"
      :> "stats1_rxFullRetries"
      :> "stats1_failAfterUps"
      :> "stats2_txRetries"
      :> "stats2_rxRetries"
      :> "stats2_rxFullRetries"
      :> "stats2_failAfterUps"
      :> "stats3_txRetries"
      :> "stats3_rxRetries"
      :> "stats3_rxFullRetries"
      :> "stats3_failAfterUps"
      :> "stats4_txRetries"
      :> "stats4_rxRetries"
      :> "stats4_rxFullRetries"
      :> "stats4_failAfterUps"
      :> "stats5_txRetries"
      :> "stats5_rxRetries"
      :> "stats5_rxFullRetries"
      :> "stats5_failAfterUps"
      :> "stats6_txRetries"
      :> "stats6_rxRetries"
      :> "stats6_rxFullRetries"
      :> "stats6_failAfterUps"
      :> "linkUps"
      :> Nil)

      (  "probe_test_start_no_control"
      :> "probe_test_start_hard_control"
      :> "probe_test_start_soft_control"
      :> Nil)

      (repeat False)
      sysClk

      done
      success

      -- Debug probes
      (txRetries     <$> stats0)
      (rxRetries     <$> stats0)
      (rxFullRetries <$> stats0)
      (failAfterUps  <$> stats0)
      (txRetries     <$> stats1)
      (rxRetries     <$> stats1)
      (rxFullRetries <$> stats1)
      (failAfterUps  <$> stats1)
      (txRetries     <$> stats2)
      (rxRetries     <$> stats2)
      (rxFullRetries <$> stats2)
      (failAfterUps  <$> stats2)
      (txRetries     <$> stats3)
      (rxRetries     <$> stats3)
      (rxFullRetries <$> stats3)
      (failAfterUps  <$> stats3)
      (txRetries     <$> stats4)
      (rxRetries     <$> stats4)
      (rxFullRetries <$> stats4)
      (failAfterUps  <$> stats4)
      (txRetries     <$> stats5)
      (rxRetries     <$> stats5)
      (rxFullRetries <$> stats5)
      (failAfterUps  <$> stats5)
      (txRetries     <$> stats6)
      (rxRetries     <$> stats6)
      (rxFullRetries <$> stats6)
      (failAfterUps  <$> stats6)
      (bundle linkUps)

-- | Count the number of FINC / FDEC Pulses, FINC => +1 , FDEC => -1.
countfIncfDecs ::
  (KnownDomain dom, KnownNat n) =>
  Clock dom ->
  Reset dom ->
  Enable dom ->
  (Signal dom Bool, Signal dom Bool) ->
  Signal dom (Signed n)
countfIncfDecs clk rst ena (fInc, fDec) = cnt
 where
  cntCond = isRising clk rst ena False fInc .||. isRising clk rst ena False fDec
  cnt = regEn clk rst ena 0 cntCond cnt'
  cnt' = satAdd SatError <$> cnt <*> mux fInc 1 (-1)


-- XXX: We use an explicit top entity annotation here, as 'makeTopEntity'
--      generates warnings in combination with 'Vec'.
{-# ANN fullMeshHwCcTest Synthesize
  { t_name = "fullMeshHwCcTest"
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
      , PortProduct "" [PortName "FINC", PortName "FDEC"]
      , PortName "SYNC_OUT"
      , PortName "spiDone"
      , (PortProduct "") [PortName "SCLK", PortName "MOSI", PortName "CSB"]
      , PortName "linkUps"
      ]
  } #-}
