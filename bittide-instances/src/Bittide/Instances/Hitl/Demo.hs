-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans -fconstraint-solver-iterations=10 #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

module Bittide.Instances.Hitl.Demo (dut, demoTest, tests) where

import Clash.Explicit.Prelude hiding (PeriodToCycles)
import Clash.Prelude (HiddenClockResetEnable, withClockResetEnable)

import Bittide.Arithmetic.Time (PeriodToCycles, trueFor)
import Bittide.ClockControl hiding (speedChangeToFincFdec)
import Bittide.ClockControl.Callisto.Types (CallistoCResult)
import Bittide.ClockControl.CallistoSw (
  SwControlCConfig (SwControlCConfig),
  callistoSwClockControlC,
 )
import Bittide.ClockControl.Si5395J
import Bittide.ClockControl.Si539xSpi (ConfigState (Error, Finished), si539xSpi)
import Bittide.Counter
import Bittide.DoubleBufferedRam
import Bittide.ElasticBuffer (
  EbMode,
  Overflow,
  Underflow,
  resettableXilinxElasticBuffer,
  sticky,
 )
import Bittide.Hitl
import Bittide.Instances.Domains
import Bittide.Instances.Hitl.IlaPlot (syncInRecover, syncOutGenerator)
import Bittide.Instances.Hitl.Setup
import Bittide.ProcessingElement
import Bittide.SharedTypes
import Bittide.Simulate.Config (CcConf (..))
import Bittide.Topology (Topology, TopologyType (Complete), complete)
import Bittide.Transceiver (transceiverPrbsN)
import Bittide.Wishbone

import Clash.Annotations.TH (makeTopEntity)
import Clash.Cores.Xilinx.GTH
import Clash.Cores.Xilinx.Unisim.DnaPortE2 (simDna2)
import Clash.Cores.Xilinx.VIO
import Clash.Cores.Xilinx.Xpm.Cdc (xpmCdcSingle)
import Clash.Functor.Extra
import Clash.Signal.TH.Extra (deriveSignalHasFields)
import Clash.Xilinx.ClockGen (clockWizardDifferential)
import Data.Maybe (fromMaybe, isJust)
import Data.Proxy
import LiftType (liftTypeQ)
import Protocols
import Protocols.Idle
import Protocols.Wishbone
import System.FilePath ((</>))
import VexRiscv

import qualified Bittide.Instances.Hitl.Driver.Demo as D
import qualified Bittide.Transceiver as Transceiver
import qualified Data.Map.Strict as Map (fromList)

data TestConfig = TestConfig
  { fpgaEnabled :: Bool
  -- ^ Enables or disables an FPGA depending on the selected
  -- topology. Disabled FPGAs immediediatly succeed after the test
  -- start.
  --
  -- Also note that the flag only disables clock control, while
  -- other functionality, as for example SYNC_IN/SYNC_OUT time
  -- synchronization, needs to stay alive.
  , mask :: BitVector LinkCount
  -- ^ The link mask depending on the selected topology.
  }
  deriving (Generic, NFDataX, BitPack, Show)

deriveSignalHasFields ''TestConfig

disabled :: TestConfig
disabled =
  TestConfig
    { fpgaEnabled = False
    , mask = 0
    }

clockControlConfig ::
  $(case (instancesClockConfig (Proxy @Basic125)) of (_ :: t) -> liftTypeQ @t)
clockControlConfig =
  $(lift (instancesClockConfig (Proxy @Basic125)))

txCounterStartUgn :: BitVector 64
txCounterStartUgn = 0xaabb_ccdd_eeff_1234

rxCounterStartUgn :: BitVector 64
rxCounterStartUgn = 0x1122_3344_1122_3344

type AllStablePeriod = Seconds 5

type FifoSize = 5 -- = 2^5 = 32

{- | The number of FINCs (if positive) or FDECs (if negative) applied
prior to the test start leading to some desired initial clock
offset.
-}
type FincFdecCount = Signed 32

data SyncCfg dom = SyncCfg
  { refClk :: Clock dom
  , refRst :: Reset dom
  , allReady :: Signal dom Bool
  , startTest :: Signal dom Bool
  , syncIn :: Signal dom Bool
  }

data SyncCtl dom = SyncCtl
  { syncRst :: Reset dom
  , syncOut :: Signal dom Bool
  , syncStart :: Signal dom Bool
  }

syncCtlSetup ::
  forall dom.
  (HasDefinedInitialValues dom, HasSynchronousReset dom) =>
  SyncCfg dom ->
  SyncCtl dom
syncCtlSetup SyncCfg{..} = SyncCtl{..}
 where
  syncOut =
    dflipflop refClk
      $ syncOutGenerator refClk startTest
      $ trueFor (SNat @(Seconds 5)) refClk syncRst allReady

  syncInFiltered =
    unsafeToActiveLow
      $ resetGlitchFilter (SNat @128) refClk
      $ unsafeFromActiveLow
      $ xpmCdcSingle refClk refClk syncIn

  (syncActive, syncStart) =
    unbundle $ syncInRecover refClk refRst startTest syncInFiltered

  syncRst = refRst `orReset` unsafeFromActiveLow syncActive

{- Internal busses:
    - Instruction memory
    - Data memory
    - `timeWb`
-}
type NmuInternalBusses = 3
type NmuRemBusWidth nodeBusses = 30 - CLog 2 (nodeBusses + NmuInternalBusses)

data SimpleManagementConfig nodeBusses where
  SimpleManagementConfig ::
    (KnownNat nodeBusses) =>
    PeConfig (nodeBusses + NmuInternalBusses) ->
    DumpVcd ->
    SimpleManagementConfig nodeBusses

simpleManagementUnitC ::
  forall bitDom nodeBusses.
  ( KnownDomain bitDom
  , HiddenClockResetEnable bitDom
  , 1 <= DomainPeriod bitDom
  , KnownNat nodeBusses
  , CLog 2 (nodeBusses + NmuInternalBusses) <= 30
  ) =>
  SimpleManagementConfig nodeBusses ->
  Circuit
    (Jtag bitDom, CSignal bitDom (BitVector 64))
    (Vec nodeBusses (Wishbone bitDom 'Standard (NmuRemBusWidth nodeBusses) (Bytes 4)))
simpleManagementUnitC (SimpleManagementConfig peConfig dumpVcd) =
  circuit $ \(jtag, _linkIn) -> do
    peWbs <- processingElement dumpVcd peConfig -< jtag
    ([timeWbBus], nmuWbs) <- splitAtC d1 -< peWbs
    timeWb -< timeWbBus
    idC -< nmuWbs

jtagChain ::
  forall dom n.
  (KnownDomain dom, KnownNat n) =>
  Circuit (Jtag dom) (Vec (n + 1) (Jtag dom))
jtagChain = Circuit go
 where
  go ::
    (Fwd (Jtag dom), Bwd (Vec (n + 1) (Jtag dom))) ->
    (Bwd (Jtag dom), Fwd (Vec (n + 1) (Jtag dom)))
  go (fwd, bwds) = (bwd, fwds)
   where
    tcks = repeat $ testClock <$> fwd
    tmss = repeat $ testModeSelect <$> fwd
    tdis = (testDataIn <$> fwd) :> (testDataOut <<$>> init bwds)
    fwds = zipWith3 (liftA3 JtagIn) tcks tmss tdis
    bwd = (head bwds)

unsafeJtagSynchronizer ::
  forall dom1 dom2.
  (KnownDomain dom1, KnownDomain dom2) =>
  Clock dom1 ->
  Clock dom2 ->
  Circuit (Jtag dom1) (Jtag dom2)
unsafeJtagSynchronizer clk1 clk2 = Circuit go
 where
  go ::
    (Fwd (Jtag dom1), Bwd (Jtag dom2)) ->
    (Bwd (Jtag dom1), Fwd (Jtag dom2))
  go (jtagIn1, jtagOut2) = (jtagOut1, jtagIn2)
   where
    jtagOut1 = unsafeSynchronizer clk2 clk1 jtagOut2
    jtagIn2 = unsafeSynchronizer clk1 clk2 jtagIn1

-- | Availabe step size configurations.
data StepSizeSelect
  = PPB_1
  | PPB_10
  | PPB_100
  | PPB_500
  | PPM_1
  deriving (Generic, NFDataX, BitPack, Eq, Enum, Bounded, Show)

{- | The step size, as it is used by all tests. Note that changing the
step size for individual tests requires recalibration of the clock
offsets, which is why we fix it to a single and common value here.
-}
commonStepSizeSelect :: StepSizeSelect
commonStepSizeSelect =
  -- Don't forget to update the value of f_step this value in "Callisto.hs" and
  -- "callisto.rs".
  PPB_100

commonSpiConfig :: TestConfig6_200_on_0a_RegisterMap
commonSpiConfig = case commonStepSizeSelect of
  PPB_1 -> testConfig6_200_on_0a_1ppb
  PPB_10 -> testConfig6_200_on_0a_10ppb
  PPB_100 -> testConfig6_200_on_0a_100ppb
  PPB_500 -> testConfig6_200_on_0a_500ppb
  PPM_1 -> testConfig6_200_on_0a_1ppm

foldWithBitMask ::
  forall n a.
  (KnownNat n) =>
  (a -> a -> a) ->
  a ->
  Vec (n + 1) a ->
  BitVector (n + 1) ->
  a
foldWithBitMask foldFn dflt foldVec mask = output
 where
  maskVec :: Vec (n + 1) Bit
  maskVec = bv2v mask
  go :: a -> Bit -> a
  go val b = if b == high then val else dflt
  defaultMasked :: Vec (n + 1) a
  defaultMasked = zipWith go foldVec maskVec
  output :: a
  output = fold foldFn defaultMasked

dut ::
  "REFCLK" ::: Clock Basic125 ->
  "SKYCLK" ::: Clock Ext200 ->
  "TEST_CFG" ::: Signal Basic125 TestConfig ->
  "SYNC_CFG" ::: SyncCtl Basic125 ->
  "GTH_RX_NS" ::: TransceiverWires GthRxS LinkCount ->
  "GTH_RX_PS" ::: TransceiverWires GthRxS LinkCount ->
  "MISO" ::: Signal Basic125 Bit ->
  "JTAG_IN" ::: Signal Basic125 JtagIn ->
  ( "GTH_TX_NS" ::: TransceiverWires GthTxS LinkCount
  , "GTH_TX_PS" ::: TransceiverWires GthTxS LinkCount
  , "handshakesDone" ::: Signal Basic125 Bool
  , "FINC_FDEC" ::: Signal Basic125 (FINC, FDEC)
  , "spiDone" ::: Signal Basic125 Bool
  , ""
      ::: ( "SCLK" ::: Signal Basic125 Bool
          , "MOSI" ::: Signal Basic125 Bit
          , "CSB" ::: Signal Basic125 Bool
          )
  , "JTAG_OUT" ::: Signal Basic125 JtagOut
  , "transceiversFailedAfterUp" ::: Signal Basic125 Bool
  , "ALL_READY" ::: Signal Basic125 Bool
  , "ALL_STABLE" ::: Signal Basic125 Bool
  , "noFifoOverflows" ::: Signal Basic125 Bool
  , "noFifoUnderflows" ::: Signal Basic125 Bool
  )
dut refClk skyClk cfg SyncCtl{..} rxNs rxPs miso jtagIn =
  ( transceivers.txNs
  , transceivers.txPs
  , allHandshakesComplete
  , frequencyAdjustments
  , spiDone
  , spiOut
  , jtagOut
  , transceiversFailedAfterUp
  , allReady
  , allStable0
  , noFifoOverflows
  , noFifoUnderflows
  )
 where
  spiState :: Signal Basic125 (ConfigState Basic125 TestConfig6_200_on_0a_TotalRegs)
  spiOut :: (Signal Basic125 Bool, Signal Basic125 Bit, Signal Basic125 Bool)
  (_, _, spiState, spiOut) =
    withClockResetEnable refClk spiRst enableGen
      $ si539xSpi commonSpiConfig (SNat @(Microseconds 10)) (pure Nothing) miso

  spiDone :: Signal Basic125 Bool
  spiDone = dflipflop refClk $ (== Finished) <$> spiState
  spiErr :: Signal Basic125 Bool
  spiErr = dflipflop refClk $ isErr <$> spiState
  gthAllReset :: Reset Basic125
  gthAllReset = unsafeFromActiveLow spiDone

  spiRst :: Reset Basic125
  spiRst = syncRst `orReset` unsafeFromActiveHigh spiErr

  isErr :: ConfigState dom n -> Bool
  isErr (Error _) = True
  isErr _ = False

  transceivers :: Transceiver.Outputs LinkCount GthTx GthRx GthTxS Basic125
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
        { clock = refClk
        , reset = gthAllReset
        , refClock = skyClk
        , channelNames
        , clockPaths
        , rxNs
        , rxPs
        , -- controlled by management unit, should come from combination of ??
          txDatas = repeat txCounter
        , txStarts = repeat (pure True)
        , rxReadys = repeat (pure True)
        }

  bittideClk :: Clock GthTx
  bittideClk = transceivers.txClock

  allHandshakesComplete :: Signal Basic125 Bool
  allHandshakesComplete =
    foldWithBitMask
      (&&)
      True
      <$> (bundle transceivers.handshakesCompleteFree)
      <*> cfg.mask
  handshakeRst :: Reset Basic125
  handshakeRst = unsafeFromActiveLow allHandshakesComplete
  handshakeRstTx :: Reset GthTx
  handshakeRstTx =
    unsafeFromActiveLow
      $ foldWithBitMask
        (&&)
        True
      <$> (bundle transceivers.handshakesCompleteTx)
      <*> (unsafeSynchronizer refClk bittideClk cfg.mask)

  allReady :: Signal Basic125 Bool
  allReady =
    trueFor (SNat @(Milliseconds 500)) refClk spiRst (and <$> bundle transceivers.linkReadys)
  transceiversFailedAfterUp :: Signal Basic125 Bool
  transceiversFailedAfterUp =
    sticky refClk syncRst (isFalling refClk spiRst enableGen False allReady)

  othersNotInStartReset :: Vec LinkCount (Signal GthRx Bool)
  othersNotInStartReset = maybe False (\val -> msb val == high) <<$>> transceivers.rxDatas
  othersNotInStartResetRefC :: Vec LinkCount (Signal Basic125 Bool)
  othersNotInStartResetRefC =
    zipWith
      (\s rxClk -> xpmCdcSingle rxClk refClk s)
      othersNotInStartReset
      transceivers.rxClocks

  startRst :: Reset Basic125
  startRst =
    orReset spiRst
      $ orReset (unsafeFromActiveLow allReady)
      $ orReset (unsafeFromActiveLow syncStart)
      $ orReset
        (unsafeFromActiveHigh transceiversFailedAfterUp)
        (unsafeFromActiveLow $ and <$> bundle othersNotInStartResetRefC)
  startEna :: Enable Basic125
  startEna = toEnable $ unsafeToActiveLow startRst
  startEnaTx :: Enable GthTx
  startEnaTx = toEnable $ xpmCdcSingle refClk bittideClk $ fromEnable startEna

  notInStartReset :: Signal Basic125 Bool
  notInStartReset = unsafeToActiveLow startRst

  muConfig :: SimpleManagementConfig 1
  muConfig =
    SimpleManagementConfig
      PeConfig
        { memMapConfig = 0b100 :> 0b010 :> 0b110 :> 0b101 :> Nil
        , initI = Undefined @(Div (64 * 1024) 4)
        , initD = Undefined @(Div (64 * 1024) 4)
        , iBusTimeout = d0
        , dBusTimeout = d0
        , includeIlaWb = False
        }
      NoDumpVcd

  ccConfig ::
    SwControlCConfig CccStabilityCheckerMargin (CccStabilityCheckerFramesize Basic125) 0
  ccConfig =
    SwControlCConfig
      SNat
      SNat
      PeConfig
        { memMapConfig = 0b100 :> 0b010 :> 0b110 :> 0b101 :> Nil
        , initI = Undefined @(Div (64 * 1024) 4)
        , initD = Undefined @(Div (64 * 1024) 4)
        , iBusTimeout = d0
        , dBusTimeout = d0
        , includeIlaWb = True
        }

  circuitFn ::
    ( Fwd
        ( Jtag Basic125
        , CSignal GthTx (BitVector 64)
        , CSignal Basic125 Bool
        , CSignal Basic125 (BitVector LinkCount)
        , Vec LinkCount (CSignal Basic125 (RelDataCount CccBufferSize))
        )
    , Bwd (CSignal Basic125 (CallistoCResult LinkCount))
    ) ->
    ( Bwd
        ( Jtag Basic125
        , CSignal GthTx (BitVector 64)
        , CSignal Basic125 Bool
        , CSignal Basic125 (BitVector LinkCount)
        , Vec LinkCount (CSignal Basic125 (RelDataCount CccBufferSize))
        )
    , Fwd (CSignal Basic125 (CallistoCResult LinkCount))
    )
  Circuit circuitFn = circuit $ \(jtag, linkIn, reframe, mask, dc) -> do
    [muJtag0, ccJtag] <- jtagChain -< jtag
    muJtag1 <- unsafeJtagSynchronizer refClk bittideClk -< muJtag0

    [muTimeWb] <-
      withClockResetEnable
        bittideClk
        handshakeRstTx
        startEnaTx
        (simpleManagementUnitC muConfig)
        -< (muJtag1, linkIn)
    withClockResetEnable bittideClk handshakeRstTx startEnaTx (readDnaPortE2Wb simDna2)
      -< muTimeWb

    (swCcOut, ccM2S) <-
      withClockResetEnable
        refClk
        handshakeRst
        startEna
        (callistoSwClockControlC @LinkCount @CccBufferSize NoDumpVcd ccConfig)
        -< (ccJtag, reframe, mask, dc)
    idleSink -< ccM2S

    idC -< swCcOut

  ((jtagOut, _, _, _, _), callistoResult) =
    circuitFn ((jtagIn, pure 0, pure False, cfg.mask, (resize <<$>> domainDiffs)), pure ())

  allStable0 :: Signal Basic125 Bool
  allStable0 = callistoResult.allStableC
  allStable1 :: Signal Basic125 Bool
  allStable1 = sticky refClk spiRst allStable0

  frequencyAdjustments :: Signal Basic125 (FINC, FDEC)
  frequencyAdjustments =
    delay refClk enableGen minBound
      $ speedChangeToStickyPins
        refClk
        startRst
        enableGen
        (SNat @Si539xHoldTime)
        callistoResult.maybeSpeedChangeC

  domainDiffs :: Vec LinkCount (Signal Basic125 FincFdecCount)
  domainDiffs =
    zipWith3
      (domainDiffCounterExt refClk)
      (repeat startRst)
      transceivers.rxClocks
      (repeat transceivers.txClock)

  txAllStable :: Signal GthTx Bool
  txAllStable = xpmCdcSingle refClk bittideClk allStable1
  txReset2 :: Reset GthTx
  txReset2 = orReset transceivers.txReset (unsafeFromActiveLow txAllStable)

  txNotInStartReset :: Signal GthTx Bool
  txNotInStartReset = unsafeSynchronizer refClk bittideClk notInStartReset

  txCounter :: Signal GthTx (BitVector 64)
  txCounter =
    regEn
      bittideClk
      txReset2
      enableGen
      txCounterStartUgn
      txNotInStartReset
      (txCounter + 1)

  rxFifos ::
    Vec
      LinkCount
      ( Signal GthTx (RelDataCount FifoSize)
      , Signal GthTx Underflow
      , Signal GthTx Overflow
      , Signal GthTx EbMode
      , Signal GthTx (Maybe (BitVector 64))
      )
  rxFifos = zipWith go transceivers.rxClocks transceivers.rxDatas
   where
    go ::
      Clock GthRx ->
      Signal GthRx (Maybe (BitVector 64)) ->
      ( Signal GthTx (RelDataCount FifoSize)
      , Signal GthTx Underflow
      , Signal GthTx Overflow
      , Signal GthTx EbMode
      , Signal GthTx (Maybe (BitVector 64))
      )
    go rxClk rxData =
      resettableXilinxElasticBuffer
        @FifoSize
        @_
        @_
        @(Maybe (BitVector 64))
        bittideClk
        rxClk
        transceivers.txReset
        rxData

  fifoUnderflowsTx :: Vec LinkCount (Signal GthTx Underflow)
  fifoOverflowsTx :: Vec LinkCount (Signal GthTx Overflow)
  mRxCntrs :: Vec LinkCount (Signal GthTx (Maybe (BitVector 64)))
  (_, fifoUnderflowsTx, fifoOverflowsTx, _, mRxCntrs) = unzip5 rxFifos
  _rxCntrs :: Vec LinkCount (Signal GthTx (BitVector 64))
  _rxCntrs =
    (regMaybe bittideClk (unsafeFromActiveLow txNotInStartReset) enableGen rxCounterStartUgn)
      <$> mRxCntrs

  fifoOverflowsFree :: Vec LinkCount (Signal Basic125 Overflow)
  fifoOverflowsFree = (xpmCdcSingle bittideClk refClk) <$> fifoOverflowsTx
  fifoUnderflowsFree :: Vec LinkCount (Signal Basic125 Underflow)
  fifoUnderflowsFree = (xpmCdcSingle bittideClk refClk) <$> fifoUnderflowsTx

  maskWithCfg ::
    Bool ->
    Vec LinkCount (Signal Basic125 Bool) ->
    Signal Basic125 (Vec LinkCount Bool)
  maskWithCfg dflt = liftA2 go1 (mask <$> cfg) . bundle
   where
    go1 m = zipWith go2 (bitCoerce m)
    go2 m val = if m then val else dflt

  findFifoError :: Vec LinkCount (Signal Basic125 Bool) -> Signal Basic125 Bool
  findFifoError bits = result
   where
    masked = maskWithCfg False bits
    observingError = or <$> masked
    observedError = sticky refClk startRst observingError
    result = not <$> observedError

  noFifoOverflows :: Signal Basic125 Bool
  noFifoOverflows = findFifoError fifoOverflowsFree
  noFifoUnderflows :: Signal Basic125 Bool
  noFifoUnderflows = findFifoError fifoUnderflowsFree

demoTest ::
  "SMA_MGT_REFCLK_C" ::: DiffClock Ext200 ->
  "SYSCLK_125" ::: DiffClock Ext125 ->
  "SYNC_IN" ::: Signal Basic125 Bool ->
  "GTH_RX_NS" ::: TransceiverWires GthRxS LinkCount ->
  "GTH_RX_PS" ::: TransceiverWires GthRxS LinkCount ->
  "MISO" ::: Signal Basic125 Bit ->
  "JTAG" ::: Signal Basic125 JtagIn ->
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
  , "JTAG" ::: Signal Basic125 JtagOut
  )
demoTest boardClkDiff refClkDiff syncIn rxns rxps miso jtagIn =
  (txns, txps, unbundle swFincFdecs, syncOut, spiDone, spiOut, jtagOut)
 where
  boardClk :: Clock Ext200
  boardClk = ibufds_gte3 boardClkDiff
  refClk :: Clock Basic125
  refRst :: Reset Basic125
  (refClk, refRst) = clockWizardDifferential refClkDiff noReset

  testStart :: Signal Basic125 Bool
  testConfig0 :: Signal Basic125 TestConfig
  syncEn :: Signal Basic125 Bool
  (unbundle -> (testStart, testConfig0, syncEn)) =
    setName @"vioHitlt"
      $ vioProbe
        ("probe_test_done" :> "probe_test_success" :> "probe_handshakes_done" :> Nil)
        ("probe_test_start" :> "probe_test_data" :> "probe_sync_enable" :> Nil)
        (False, disabled, False)
        refClk
        testDone
        testSuccess
        handshakesDone

  SyncCtl{syncOut = unmaskedSyncOut, ..} = syncCtlSetup SyncCfg{..}
  syncOut :: Signal Basic125 Bool
  syncOut = unmaskedSyncOut .&&. syncEn

  testConfig :: Signal Basic125 (Maybe TestConfig)
  testConfig = mux testStart (Just <$> testConfig0) (pure Nothing)

  startTest :: Signal Basic125 Bool
  startTest = isJust <$> testConfig
  cfg :: Signal Basic125 TestConfig
  cfg = fromMaybe disabled <$> testConfig

  ( txns :: TransceiverWires GthTxS LinkCount
    , txps :: TransceiverWires GthTxS LinkCount
    , handshakesDone :: Signal Basic125 Bool
    , swFincFdecs :: Signal Basic125 (Bool, Bool)
    , spiDone :: Signal Basic125 Bool
    , spiOut :: (Signal Basic125 Bool, Signal Basic125 Bit, Signal Basic125 Bool)
    , jtagOut :: Signal Basic125 JtagOut
    , transceiversFailedAfterUp :: Signal Basic125 Bool
    , allReady :: Signal Basic125 Bool
    , allStable :: Signal Basic125 Bool
    , noFifoOverflows :: Signal Basic125 Bool
    , noFifoUnderflows :: Signal Basic125 Bool
    ) =
      dut
        refClk
        boardClk
        cfg
        SyncCtl{..}
        rxns
        rxps
        miso
        jtagIn

  skip :: Signal Basic125 Bool
  skip = maybe False (not . fpgaEnabled) <$> testConfig

  startBeforeAllReady :: Signal Basic125 Bool
  startBeforeAllReady =
    sticky
      refClk
      syncRst
      (syncStart .&&. ((not <$> allReady) .||. transceiversFailedAfterUp))

  fifoSuccess :: Signal Basic125 Bool
  fifoSuccess = noFifoUnderflows .&&. noFifoOverflows

  endSuccess :: Signal Basic125 Bool
  endSuccess =
    trueFor (SNat @(Seconds 5)) refClk syncRst
      $ allStable
      .&&. fifoSuccess

  testDone :: Signal Basic125 Bool
  testDone =
    startTest
      .&&. ( skip
              .||. endSuccess
              .||. transceiversFailedAfterUp
              .||. startBeforeAllReady
              .||. (not <$> fifoSuccess)
           )

  testSuccess :: Signal Basic125 Bool
  testSuccess =
    skip
      .||. ( allStable
              .&&. fifoSuccess
              .&&. (not <$> (transceiversFailedAfterUp .||. startBeforeAllReady))
           )
{-# OPAQUE demoTest #-}

makeTopEntity 'demoTest

type DemoTopologySize = 3

tests :: [HitlTestGroup]
tests = [testGroup]
 where
  ClockControlConfig{..} = clockControlConfig

  defSimCfg :: CcConf
  defSimCfg =
    def
      { samples = 1000
      , duration = natToNum @(PeriodToCycles Basic125 (Seconds 60))
      , stabilityMargin = snatToNum cccStabilityCheckerMargin
      , stabilityFrameSize = snatToNum cccStabilityCheckerFramesize
      , waitTime = fromEnum cccReframingWaitTime
      , stopAfterStable =
          Just
            $ natToNum @(PeriodToCycles Basic125 AllStablePeriod)
      }

  demoTopology :: Topology DemoTopologySize
  demoTopology = complete SNat

  testData ::
    forall n.
    (KnownNat n, n <= FpgaCount) =>
    Index n ->
    BitVector LinkCount ->
    (HwTargetRef, TestConfig)
  testData i mask =
    ( HwTargetByIndex (fromIntegral i)
    , TestConfig{fpgaEnabled = True, ..}
    )

  testCase :: HitlTestCase HwTargetRef TestConfig CcConf
  testCase =
    HitlTestCase
      { name = "Bittide Demo DUT"
      , parameters =
          Map.fromList
            $ toList (zipWith testData indicesI (linkMasks demoTopology))
            <> [ (HwTargetByIndex (fromInteger i), disabled)
               | let n = topologySize demoTopology
               , i <- [n, n + 1 .. natToNum @LinkCount]
               ]
      , postProcData =
          defSimCfg
            { ccTopologyType = Complete $ natToInteger @DemoTopologySize
            , clockOffsets = Nothing
            , startupDelays = toList $ repeat @DemoTopologySize 0
            }
      }

  topologySize ::
    forall n.
    (KnownNat n) =>
    Topology n ->
    Integer
  topologySize _ = natToNum @n

  testGroup :: HitlTestGroup
  testGroup =
    HitlTestGroup
      { topEntity = 'demoTest
      , extraXdcFiles = ["jtag" </> "config.xdc", "jtag" </> "pmod1.xdc"]
      , externalHdl = []
      , testCases = [testCase]
      , mDriverProc = Just D.driverFunc
      , mPostProc = Nothing
      }
