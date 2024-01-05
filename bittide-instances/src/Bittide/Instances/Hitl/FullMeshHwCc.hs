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
module Bittide.Instances.Hitl.FullMeshHwCc
  ( fullMeshHwCcWithRiscvTest
  , fullMeshHwCcTest
  , NodeCount
  , DataCountSize
  , clockControlConfig
  ) where

import Clash.Prelude (withClockResetEnable)
import Clash.Explicit.Prelude
import qualified Clash.Explicit.Prelude as E

import Control.Arrow ((***), second)
import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import Data.Proxy
import Language.Haskell.TH (runIO)
import LiftType (liftTypeQ)
import System.Directory
import System.FilePath

import Bittide.Arithmetic.Time
import Bittide.ClockControl
import Bittide.ClockControl.Callisto
import Bittide.ClockControl.Registers (clockControlWb)
import Bittide.ClockControl.Si5395J
import Bittide.ClockControl.Si539xSpi (ConfigState(Error, Finished), si539xSpi)
import Bittide.DoubleBufferedRam
  ( InitialContent(Reloadable), ContentType(Blob), RegisterWritePriority(CircuitPriority)
  , registerWb
  )
import Bittide.Counter
import Bittide.ElasticBuffer (sticky)
import Bittide.Instances.Domains
import Bittide.ProcessingElement (PeConfig(..), processingElement)
import Bittide.ProcessingElement.Util (memBlobsFromElf)
import Bittide.SharedTypes (Bytes, ByteOrder(BigEndian))
import Bittide.Transceiver

import Bittide.Instances.Hitl.FullMeshHwCc.IlaPlot
import Bittide.Instances.Pnr.MVPs (stickyBits, speedChangeToPins, FINC, FDEC)

import Clash.Class.Counter
import Clash.Cores.Xilinx.GTH
import Clash.Cores.Xilinx.VIO (vioProbe)
import Clash.Cores.Xilinx.Xpm.Cdc.Single
import Clash.Cores.Xilinx.Xpm.Cdc.Gray
import Clash.Cores.Xilinx.Ila (IlaConfig(..), Depth(..), ila, ilaConfig)
import Clash.Explicit.Reset.Extra
import Clash.Explicit.Signal.Extra
import Clash.Sized.Extra (unsignedToSigned)
import Clash.Xilinx.ClockGen

import Protocols
import Protocols.Wishbone
import Protocols.Internal


type NodeCount = 8 :: Nat
type DataCountSize = 25 :: Nat

clockControlConfig ::
  $(case (instancesClockConfig (Proxy @GthTx)) of { (_ :: t) -> liftTypeQ @t })
clockControlConfig =
  $(lift (instancesClockConfig (Proxy @GthTx)))

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

unitCS :: CSignal dom ()
unitCS = CSignal (pure ())

-- | Instantiates a RiscV core that copies instructions coming from a hardware
-- implementation of Callisto (see 'fullMeshHwTest') and copies it to a register
-- tied to FINC/FDEC.
fullMeshRiscvCopyTest ::
  forall dom .
  KnownDomain dom =>
  Clock dom ->
  Reset dom ->
  Signal dom (CallistoResult 7) ->
  Vec 7 (Signal dom (DataCount 32)) ->
  -- Freq increase / freq decrease request to clock board
  ( "FINC" ::: Signal dom Bool
  , "FDEC" ::: Signal dom Bool
  )
fullMeshRiscvCopyTest clk rst callistoResult dataCounts = unbundle fIncDec
 where
  (_, CSignal fIncDec) = toSignals
    ( circuit $ \unit -> do
      [wbA, wbB] <- withClockResetEnable clk rst enableGen $ processingElement @dom peConfig -< unit
      fIncDecCallisto -< wbA
      (fIncDec, _allStable) <- withClockResetEnable clk rst enableGen $
        clockControlWb margin framesize (pure $ complement 0) dataCounts -< wbB
      idC -< fIncDec
    ) ((), unitCS)

  fIncDecCallisto ::
    forall aw nBytes .
    (KnownNat aw, 2 <= aw, nBytes ~ 4) =>
    Circuit
      (Wishbone dom 'Standard aw (Bytes nBytes))
      ()
  fIncDecCallisto = Circuit goFIncDecCallisto
   where
    goFIncDecCallisto (wbM2S, _) = (wbS2M, ())
     where
      (_, wbS2M) = withClockResetEnable clk rst enableGen $
        registerWb
          CircuitPriority
          (0 :: Bytes nBytes, 0 :: Bytes nBytes)
          wbM2S
          (fmap (fmap ((,0) . extend . pack)) fincfdec)

      fincfdec :: Signal dom (Maybe SpeedChange)
      fincfdec =
        clearOnAck
          <$> fmap acknowledge wbS2M
          <*> fmap maybeSpeedChange callistoResult

      -- Clear register if register is read from (or written to, but we assume
      -- this doesn't happen). This makes sure the RiscV doesn't read the same
      -- result from the hardware clock control twice.
      clearOnAck :: ("ACK" ::: Bool) -> Maybe SpeedChange -> Maybe SpeedChange
      clearOnAck False maybeSpeedChange   = maybeSpeedChange
      clearOnAck True  (Just speedChange) = Just speedChange
      clearOnAck True  Nothing            = Just NoChange

  margin = d2

  framesize = SNat @(PeriodToCycles dom (Seconds 1))

  (   (_iStart, _iSize, iMem)
    , (_dStart, _dSize, dMem)) = $(do

    let
      findProjectRoot :: IO FilePath
      findProjectRoot = goUp =<< getCurrentDirectory
        where
          goUp :: FilePath -> IO FilePath
          goUp path
            | isDrive path = error "Could not find 'cabal.project'"
            | otherwise = do
                exists <- doesFileExist (path </> projectFilename)
                if exists then
                  return path
                else
                  goUp (takeDirectory path)

          projectFilename = "cabal.project"

    root <- runIO findProjectRoot

    let elfPath = root </> "_build/cargo/firmware-binaries/riscv32imc-unknown-none-elf/release/clock-control-reg-cpy"

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

-- | Instantiates a hardware implementation of Callisto and exports its results. Can
-- be used to drive FINC/FDEC directly (see @FINC_FDEC@ result) or to tie the
-- results to a RiscV core (see 'fullMeshRiscvCopyTest')
fullMeshHwTest ::
  "SMA_MGT_REFCLK_C" ::: Clock Ext200 ->
  "SYSCLK" ::: Clock Basic125 ->
  "RST_LOCAL" ::: Reset Basic125 ->
  "ILA_CTRL" ::: IlaControl Basic125 ->
  "GTH_RX_NS" ::: TransceiverWires GthRx ->
  "GTH_RX_PS" ::: TransceiverWires GthRx ->
  "MISO" ::: Signal Basic125 Bit ->
  ( "GTH_TX_NS" ::: TransceiverWires GthTx
  , "GTH_TX_PS" ::: TransceiverWires GthTx
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
  , "ALL_UP" ::: Signal Basic125 Bool
  , "ALL_STABLE"   ::: Signal Basic125 Bool
  )
fullMeshHwTest refClk sysClk testRst IlaControl{..} rxns rxps miso =
  fincFdecIla `hwSeqX`
  ( txns
  , txps
  , frequencyAdjustments
  , txClock
  , callistoResult
  , clockControlReset
  , domainDiffs
  , stats
  , spiDone
  , spiOut
  , transceiversFailedAfterUp
  , allUp
  , allStable1
  )
 where
  sysRst = orReset testRst (unsafeFromActiveLow (fmap not spiErr))

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
      @GthTx @GthRx @Ext200 @Basic125 @GthTx @GthRx
      refClk sysClk gthAllReset
      c_CHANNEL_NAMES c_CLOCK_PATHS rxns rxps

  syncLink rxClock linkUp = xpmCdcSingle rxClock sysClk linkUp
  linkUps = zipWith syncLink rxClocks linkUpsRx
  allUp = trueFor500ms sysClk sysRst (and <$> bundle linkUps)
  transceiversFailedAfterUp =
    sticky sysClk sysRst (isFalling sysClk sysRst enableGen False allUp)

  timeSucc = countSucc @(Unsigned 16, Index (PeriodToCycles Basic125 (Milliseconds 1)))
  timer = register sysClk testRst enableGen (0, 0) (timeSucc <$> timer)
  milliseconds1 = fst <$> timer

  -- Clock control
  clockControlReset =
    xpmResetSynchronizer Asserted sysClk txClock
      $ orReset (unsafeFromActiveLow allUp)
      $ orReset (unsafeFromActiveHigh transceiversFailedAfterUp)
                (unsafeFromActiveLow syncStart)

  availableLinkMask = pure maxBound

  (maybeSpeedChange1, _stabilities, allStable0, _allCentered) = unbundle $
    fmap
      (\CallistoResult{..} -> (maybeSpeedChange, stability, allStable, allSettled))
      callistoResult

  callistoResult =
    callistoClockControlWithIla @(NodeCount - 1) @DataCountSize @GthTx
      sysClk txClock clockControlReset enableGen
      clockControlConfig IlaControl{..} availableLinkMask
      (fmap (fmap resize) domainDiffs)

  speedChange2 =
    mux
      (xpmCdcSingle sysClk txClock calibrate)
      (pure Nothing)
      maybeSpeedChange1

  allStable1 = xpmCdcSingle txClock sysClk allStable0

  -- Capture every 100 microseconds - this should give us a window of about 5
  -- seconds. Or: when we're in reset. If we don't do the latter, the VCDs get
  -- very confusing.
  capture = (captureFlag .&&. allUp) .||. unsafeToActiveHigh sysRst

  fincFdecIla :: Signal Basic125 ()
  fincFdecIla = ila
    (ilaConfig $
         "trigger_0"
      :> "capture_0"
      :> "probe_milliseconds"
      :> "probe_allStable1"
      :> "probe_transceiversFailedAfterUp"
      :> "probe_nFincs"
      :> "probe_nFdecs"
      :> "probe_net_nFincs"
      :> Nil
    ){depth = D16384}
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

  nFincsSynced = xpmCdcGray txClock sysClk nFincs
  nFdecsSynced = xpmCdcGray txClock sysClk nFdecs

  captureFlag = captureCounter .==. pure (maxBound :: Index (PeriodToCycles Basic125 (Milliseconds 1)))
  captureCounter = register sysClk sysRst enableGen 0 (satSucc SatWrap <$> captureCounter)

  isFinc = speedChange2 .==. pure (Just SpeedUp)
  nFincs = register txClock clockControlReset (toEnable isFinc) (0 :: Unsigned 32) (nFincs + 1)

  isFdec = speedChange2 .==. pure (Just SlowDown)
  nFdecs = register txClock clockControlReset (toEnable isFdec) (0 :: Unsigned 32) (nFdecs + 1)

  frequencyAdjustments :: Signal GthTx (FINC, FDEC)
  frequencyAdjustments =
    E.delay txClock enableGen minBound {- glitch filter -} $
      withClockResetEnable txClock clockControlReset enableGen $
        stickyBits @GthTx d20 (speedChangeToPins . fromMaybe NoChange <$> speedChange2)

  (domainDiffs, _domainActives) =
    unzip $ fmap unbundle $ rxDiffCounter <$> rxClocks <*> linkUpsRx
  rxDiffCounter rxClk linkUp =
    domainDiffCounter rxClk (unsafeFromActiveLow linkUp) txClock clockControlReset


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
fullMeshHwCcWithRiscvTest ::
  "SMA_MGT_REFCLK_C" ::: DiffClock Ext200 ->
  "SYSCLK_300" ::: DiffClock Ext300 ->
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
  )
fullMeshHwCcWithRiscvTest refClkDiff sysClkDiff syncIn rxns rxps miso =
  (txns, txps, (riscvFinc, riscvFdec), syncOut, spiDone, spiOut)
 where
  refClk = ibufds_gte3 refClkDiff :: Clock Ext200

  (sysClk, sysRst0) = clockWizardDifferential sysClkDiff noReset
  sysRst = sysRst0 `orReset` unsafeFromActiveLow startTest

  --
  -- 'syncOutGenerator' is used to drive the 'SYNC_OUT' signal, which
  -- is only connected for the last node in the network and wired back
  -- to 'SYNC_IN' of all nodes from there.
  --
  -- Note that all nodes are in reset before their local 'startTest' VIO
  -- signal gets asserted, as 'startTest' is directly driving 'sysRst'.
  -- Thus, for the other nodes to capture the 'SYNC_OUT' signal correctly,
  -- the node receiving the `startTest` rising edge last must be the one
  -- with it's 'SYNC_OUT' physically connected to the 'SYNC_IN' of all
  -- nodes in the network. This assumption is tested by
  -- 'Bittide.Instances.Hitl.SyncInSyncOut'.
  syncOut =
      dflipflop sysClk
    $ syncOutGenerator sysClk startTest
    $ trueFor5s sysClk testRst allUp

  -- first synchronize SYNC_IN to the local clock and filter from
  -- potential glitches
  syncIn1 =
      unsafeToActiveLow
    $ resetGlitchFilter (SNat @1024) sysClk
    $ unsafeFromActiveLow
    $ xpmCdcSingle sysClk sysClk syncIn

  -- generate a pulse on every change of SYNC_IN
  syncInChangepoints =
    changepoints sysClk sysRst enableGen syncIn1

  -- recover the activity and readiness states from SYNC_IN
  (syncActive, syncStart) = unbundle $ syncInRecover sysClk sysRst syncIn1

  -- tests are reset with on `sysRst` or if not synchronously active
  testRst = sysRst `orReset` unsafeFromActiveLow syncActive

  -- checks that tests are not synchronously start before all
  -- transceivers are up
  startBeforeAllUp = sticky sysClk testRst
    (syncStart .&&. ((not <$> allUp) .||. transceiversFailedAfterUp))

  -- generate the global timestamp from the synchronous rising and
  -- falling edges of SYNC_IN
  globalTimestamp :: Signal Basic125 GlobalTimestamp
  globalTimestamp = register sysClk testRst enableGen (0,0) $
    mux syncInChangepoints
      (((+1) *** const 0) <$> globalTimestamp)
      (second (+1) <$> globalTimestamp)

  -- calibrate over the first 200 sync pulses
  calibrate =
    moore sysClk testRst enableGen
      (\s -> bool s $ satSucc SatBound s)
      (/= maxBound)
      (minBound :: Index 200)
      syncInChangepoints

  calibrationDone =
    isFalling sysClk testRst enableGen False calibrate

  syncEnd = isFalling sysClk testRst enableGen False syncActive

  (   txns, txps, _hwFincFdecs, callistoClock, callistoResult, callistoReset
    , dataCounts, stats, spiDone, spiOut, transceiversFailedAfterUp, allUp
    , allStable ) =
    fullMeshHwTest refClk sysClk testRst IlaControl{..} rxns rxps miso

  (riscvFinc, riscvFdec) =
    fullMeshRiscvCopyTest callistoClock callistoReset callistoResult dataCounts

  stats0 :> stats1 :> stats2 :> stats3 :> stats4 :> stats5 :> stats6 :> Nil = stats

  endSuccess :: Signal Basic125 Bool
  endSuccess = trueFor5s sysClk testRst allStable

  startTest :: Signal Basic125 Bool
  startTest =
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
      :> Nil)
      (  "probe_test_start"
      :> Nil)
      False
      sysClk

      -- done
      (endSuccess .||. transceiversFailedAfterUp .||. startBeforeAllUp)

      -- success
      (not <$> (transceiversFailedAfterUp .||. startBeforeAllUp))

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
-- XXX: We use an explicit top entity annotation here, as 'makeTopEntity'
--      generates warnings in combination with 'Vec'.
{-# ANN fullMeshHwCcWithRiscvTest Synthesize
  { t_name = "fullMeshHwCcWithRiscvTest"
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
      ]
  } #-}

-- | Top entity for this test. See module documentation for more information.
fullMeshHwCcTest ::
  "SMA_MGT_REFCLK_C" ::: DiffClock Ext200 ->
  "SYSCLK_300" ::: DiffClock Ext300 ->
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
  )
fullMeshHwCcTest refClkDiff sysClkDiff syncIn rxns rxps miso =
  (txns, txps, unbundle hwFincFdecs, syncOut, spiDone, spiOut)
 where
  refClk = ibufds_gte3 refClkDiff :: Clock Ext200

  (sysClk, sysRst0) = clockWizardDifferential sysClkDiff noReset
  sysRst = sysRst0 `orReset` unsafeFromActiveLow startTest

  --
  -- 'syncOutGenerator' is used to drive the 'SYNC_OUT' signal, which
  -- is only connected for the last node in the network and wired back
  -- to 'SYNC_IN' of all nodes from there.
  --
  -- Note that all nodes are in reset before their local 'startTest' VIO
  -- signal gets asserted, as 'startTest' is directly driving 'sysRst'.
  -- Thus, for the other nodes to capture the 'SYNC_OUT' signal correctly,
  -- the node receiving the `startTest` rising edge last must be the one
  -- with it's 'SYNC_OUT' physically connected to the 'SYNC_IN' of all
  -- nodes in the network. This assumption is tested by
  -- 'Bittide.Instances.Hitl.SyncInSyncOut'.
  syncOut =
      dflipflop sysClk
    $ syncOutGenerator sysClk startTest
    $ trueFor5s sysClk testRst allUp

  -- first synchronize SYNC_IN to the local clock and filter from
  -- potential glitches
  syncIn1 =
      unsafeToActiveLow
    $ resetGlitchFilter (SNat @1024) sysClk
    $ unsafeFromActiveLow
    $ xpmCdcSingle sysClk sysClk syncIn

  -- generate a pulse on every change of SYNC_IN
  syncInChangepoints =
    changepoints sysClk sysRst enableGen syncIn1

  -- recover the activity and readiness states from SYNC_IN
  (syncActive, syncStart) = unbundle $ syncInRecover sysClk sysRst syncIn1

  -- tests are reset with on `sysRst` or if not synchronously active
  testRst = sysRst `orReset` unsafeFromActiveLow syncActive

  -- checks that tests are not synchronously start before all
  -- transceivers are up
  startBeforeAllUp = sticky sysClk testRst
    (syncStart .&&. ((not <$> allUp) .||. transceiversFailedAfterUp))

  -- generate the global timestamp from the synchronous rising and
  -- falling edges of SYNC_IN
  globalTimestamp :: Signal Basic125 GlobalTimestamp
  globalTimestamp = register sysClk testRst enableGen (0,0) $
    mux syncInChangepoints
      (((+1) *** const 0) <$> globalTimestamp)
      (second (+1) <$> globalTimestamp)

  -- calibrate over the first 200 sync pulses
  calibrate =
    moore sysClk testRst enableGen
      (\s -> bool s $ satSucc SatBound s)
      (/= maxBound)
      (minBound :: Index 200)
      syncInChangepoints

  calibrationDone =
    isFalling sysClk testRst enableGen False calibrate

  syncEnd = isFalling sysClk testRst enableGen False syncActive

  (   txns, txps, hwFincFdecs, _callistoClock, _callistoResult, _callistoReset
    , _dataCounts, stats, spiDone, spiOut, transceiversFailedAfterUp, allUp
    , allStable ) =
    fullMeshHwTest refClk sysClk testRst IlaControl{..} rxns rxps miso

  stats0 :> stats1 :> stats2 :> stats3 :> stats4 :> stats5 :> stats6 :> Nil = stats

  endSuccess :: Signal Basic125 Bool
  endSuccess = trueFor5s sysClk testRst allStable

  startTest :: Signal Basic125 Bool
  startTest =
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
      :> Nil)
      (  "probe_test_start"
      :> Nil)
      False
      sysClk

      -- done
      (endSuccess .||. transceiversFailedAfterUp .||. startBeforeAllUp)

      -- success
      (not <$> (transceiversFailedAfterUp .||. startBeforeAllUp))

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
      ]
  } #-}
