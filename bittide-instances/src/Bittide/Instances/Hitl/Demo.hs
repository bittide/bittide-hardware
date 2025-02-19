-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

module Bittide.Instances.Hitl.Demo (dut, demoTest, tests) where

import Clash.Explicit.Prelude
import Clash.Prelude (HiddenClockResetEnable, withClockResetEnable)

import Bittide.Arithmetic.Time (trueFor)
import Bittide.Calendar (CalendarConfig (..), ValidEntry (..))
import Bittide.CaptureUgn (captureUgn)
import Bittide.ClockControl hiding (speedChangeToFincFdec)
import Bittide.ClockControl.Callisto.Types (CallistoCResult)
import Bittide.ClockControl.CallistoSw (
  SwControlCConfig (SwControlCConfig),
  callistoSwClockControlC,
 )
import Bittide.ClockControl.Si539xSpi (ConfigState (Error, Finished), si539xSpi)
import Bittide.Counter
import Bittide.DoubleBufferedRam
import Bittide.ElasticBuffer (
  EbMode (Pass),
  Overflow,
  Underflow,
  resettableXilinxElasticBuffer,
  sticky,
 )
import Bittide.Hitl (
  HitlTestCase (..),
  HitlTestGroup (..),
  paramForHwTargets,
 )
import Bittide.Instances.Domains
import Bittide.Instances.Hitl.Setup (
  LinkCount,
  TransceiverWires,
  allHwTargets,
  channelNames,
  clockPaths,
 )
import Bittide.Instances.Hitl.SwCcTopologies (FifoSize, FincFdecCount, commonSpiConfig)
import Bittide.Jtag (jtagChain, unsafeJtagSynchronizer)
import Bittide.ProcessingElement (PeConfig (..), processingElement, splitAtC)
import Bittide.SharedTypes (Bytes)
import Bittide.Switch (switchC)
import Bittide.SwitchDemoProcessingElement (switchDemoPeWb)
import Bittide.Transceiver (transceiverPrbsN)
import Bittide.Wishbone (readDnaPortE2Wb, timeWb)

import Clash.Annotations.TH (makeTopEntity)
import Clash.Cores.Xilinx.GTH (ibufds_gte3)
import Clash.Cores.Xilinx.Unisim.DnaPortE2 (simDna2)
import Clash.Cores.Xilinx.VIO (vioProbe)
import Clash.Cores.Xilinx.Xpm.Cdc (xpmCdcArraySingle, xpmCdcSingle)
import Clash.Functor.Extra ((<<$>>))
import Clash.Xilinx.ClockGen (clockWizardDifferential)
import Protocols
import Protocols.Idle
import Protocols.Wishbone
import System.FilePath ((</>))
import VexRiscv (DumpVcd (..), Jtag, JtagIn (..), JtagOut (..))

import qualified Bittide.Instances.Hitl.Driver.Demo as D
import qualified Bittide.Transceiver as Transceiver

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

{- FOURMOLU_DISABLE -} -- Fourmolu doesn't do well with tabular code
calendarConfig :: CalendarConfig 4 26 (Vec 8 (Index 9))
calendarConfig =
  CalendarConfig
    (SNat @LinkCount)

    -- Active calendar. It will broadcast the PE (node 1) data to all links. Other
    -- than that we cycle through the other nodes.
    (      ValidEntry (2 :> repeat 1) repetitions
        :> ValidEntry (3 :> repeat 1) repetitions
        :> ValidEntry (4 :> repeat 1) repetitions
        :> ValidEntry (5 :> repeat 1) repetitions
        :> ValidEntry (6 :> repeat 1) repetitions
        :> ValidEntry (7 :> repeat 1) repetitions
        :> ValidEntry (8 :> repeat 1) repetitions
        :> Nil
    )

    -- Don't care about inactive calendar:
    (ValidEntry (repeat 0) 1 :> Nil)
  where
  -- We want enough time to read _number of FPGAs_ triplets
  repetitions = natToNum @((LinkCount + 1) * 3) @(Unsigned 8)
{- FOURMOLU_ENABLE -}

muConfig :: SimpleManagementConfig 10
muConfig =
  SimpleManagementConfig
    PeConfig
      { memMapConfig = 0b100 :> 0b010 :> 0b110 :> 0b101 :> repeat 0
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

{- | Reset logic:

HW:

  1. Wait for SPI
  2. Wait for transceivers handshakes (=> all domains are up after this)
  3. Send local counter for one cycle, connect to switch after (=> in parallel
     with 4+, just wait until the other side says they're ready to receive)
  4a. Deassert CC CPU reset
  4b. Deassert Bittide domain reset (=> MU CPU, PE)
  5. Wait for stable buffers
  6. Wait for elastic buffer initialization (=> signal we're ready to receive data)

SW (MU):

  1. Wait for all UGNs to be captured
-}
dut ::
  "REFCLK" ::: Clock Basic125 ->
  "TEST_RST" ::: Reset Basic125 ->
  "SKYCLK" ::: Clock Ext200 ->
  "GTH_RX_NS" ::: TransceiverWires GthRxS LinkCount ->
  "GTH_RX_PS" ::: TransceiverWires GthRxS LinkCount ->
  "allProgrammed" ::: Signal Basic125 Bool ->
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
  , "ALL_STABLE" ::: Signal Basic125 Bool
  , "fifoOverflowsSticky" ::: Signal Basic125 Bool
  , "fifoUnderflowsSticky" ::: Signal Basic125 Bool
  )
dut refClk refRst skyClk rxNs rxPs allProgrammed miso jtagIn =
  ( transceivers.txNs
  , transceivers.txPs
  , handshakesCompleteFree
  , frequencyAdjustments
  , spiDone
  , spiOut
  , jtagOut
  , transceiversFailedAfterUp
  , allStable
  , fifoOverflowsSticky
  , fifoUnderflowsSticky
  )
 where
  -- Step 1, wait for SPI:
  (_, _, spiState, spiOut) =
    withClockResetEnable refClk spiRst enableGen
      $ si539xSpi @Basic125 commonSpiConfig (SNat @(Microseconds 10)) (pure Nothing) miso

  spiDone :: Signal Basic125 Bool
  spiDone = dflipflop refClk $ (== Finished) <$> spiState

  spiErr :: Signal Basic125 Bool
  spiErr = dflipflop refClk $ isErr <$> spiState

  gthAllReset :: Reset Basic125
  gthAllReset = unsafeFromActiveLow spiDone

  spiRst :: Reset Basic125
  spiRst = refRst `orReset` unsafeFromActiveHigh spiErr

  isErr :: ConfigState dom n -> Bool
  isErr (Error _) = True
  isErr _ = False

  -- Step 2, wait for transceiver handshakes:
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
        , txDatas = txDatas
        , txStarts = txStarts
        , rxReadys = ebReadysRx
        }

  bittideClk :: Clock GthTx
  bittideClk = transceivers.txClock

  handshakesCompleteFree :: Signal Basic125 Bool
  handshakesCompleteFree = and <$> bundle transceivers.handshakesCompleteFree

  handshakesCompleteTx :: Signal GthTx Bool
  handshakesCompleteTx = and <$> bundle transceivers.handshakesCompleteTx

  -- Step 3, send local counter for one cycle, connect to switch after:
  txSamplingsDelayed :: Vec 7 (Signal GthTx Bool)
  txSamplingsDelayed =
    register bittideClk handshakeRstTx enableGen False <$> transceivers.txSamplings

  txDatas :: Vec 7 (Signal GthTx (BitVector 64))
  txDatas = mux <$> txSamplingsDelayed <*> switchDataOut <*> repeat localCounter

  switchDataOut :: Vec 7 (Signal GthTx (BitVector 64))
  switchDataOut = repeat (pure 0) -- TBD: connect switch
  txStarts :: Vec 7 (Signal GthTx Bool)
  txStarts = repeat handshakesCompleteTx

  -- Step 4, deassert CC CPU reset, deassert Bittide domain reset:
  handshakeRstFree :: Reset Basic125
  handshakeRstFree = unsafeFromActiveLow handshakesCompleteFree

  handshakeRstTx :: Reset GthTx
  handshakeRstTx = unsafeFromActiveLow handshakesCompleteTx

  localCounter :: Signal GthTx (BitVector 64)
  localCounter = register bittideClk handshakeRstTx enableGen 0 (localCounter + 1)

  -- Step 5, wait for stable buffers:
  allStable :: Signal Basic125 Bool
  allStable =
    -- Note: We only consider the stable signal after all CC CPUs have been
    --       programmed. This makes sure we don't accidentally start the
    --       elastic buffers while either unprogrammed nodes just happen to be
    --       at the convergence frequency or when a single node is not
    --       synchronizing yet, pulling all the others to it (which _would_ make
    --       the system stable, but you know... not what we want!).
    callistoResult.allStableC .&&. allProgrammed

  allStableSticky :: Signal Basic125 Bool
  allStableSticky = sticky refClk handshakeRstFree allStable

  allStableStickyTx :: Signal GthTx Bool
  allStableStickyTx = xpmCdcSingle refClk bittideClk allStableSticky

  -- Step 6, wait for elastic buffer initialization
  --         (=> signal we're ready to receive data):
  ebReset :: Reset GthTx
  ebReset = unsafeFromActiveLow allStableStickyTx

  ebReadys :: Vec 7 (Signal GthTx Bool)
  ebReadys = map (.==. pure Pass) ebModes

  ebReadysRx :: Vec 7 (Signal GthRx Bool)
  ebReadysRx = xpmCdcArraySingle bittideClk <$> transceivers.rxClocks <*> ebReadys

  -- Connect everything together:
  transceiversFailedAfterUp :: Signal Basic125 Bool
  transceiversFailedAfterUp =
    sticky refClk refRst (isFalling refClk spiRst enableGen False handshakesCompleteFree)

  circuitFn ::
    ( ( Signal Basic125 JtagIn
      , Signal GthTx (BitVector 64)
      , Signal Basic125 Bool
      , Signal Basic125 (BitVector LinkCount)
      , Vec LinkCount (Signal Basic125 (RelDataCount CccBufferSize))
      , Vec LinkCount (Signal GthTx (Maybe (BitVector 64)))
      )
    , ( Signal Basic125 ()
      , Vec LinkCount (Signal GthTx ())
      )
    ) ->
    ( ( Signal Basic125 JtagOut
      , Signal GthTx ()
      , Signal Basic125 ()
      , Signal Basic125 ()
      , Vec LinkCount (Signal Basic125 ())
      , Vec LinkCount (Signal GthTx ())
      )
    , ( Signal Basic125 (CallistoCResult LinkCount)
      , Vec LinkCount (Signal GthTx (BitVector 64))
      )
    )
  Circuit circuitFn = circuit $ \(jtag, linkIn, reframe, mask, dc, [rx0, rx1, rx2, rx3, rx4, rx5, rx6]) -> do
    [muJtagFree, ccJtag] <- jtagChain -< jtag
    muJtagTx <- unsafeJtagSynchronizer refClk bittideClk -< muJtagFree

    [peWb, switchWb, dnaWb, ugn0, ugn1, ugn2, ugn3, ugn4, ugn5, ugn6] <-
      withClockResetEnable
        bittideClk
        handshakeRstTx
        enableGen
        (simpleManagementUnitC muConfig)
        -< (muJtagTx, linkIn)

    [urx0, urx1, urx2, urx3, urx4, urx5, urx6] <-
      withClockResetEnable
        bittideClk
        handshakeRstTx
        enableGen
        (repeatC (captureUgn (unpack <$> localCounter)))
        -< [ (ugn0, rx0)
           , (ugn1, rx1)
           , (ugn2, rx2)
           , (ugn3, rx3)
           , (ugn4, rx4)
           , (ugn5, rx5)
           , (ugn6, rx6)
           ]

    [peIn, tx0, tx1, tx2, tx3, tx4, tx5, tx6] <-
      withClockResetEnable
        bittideClk
        handshakeRstTx
        enableGen
        switchC
        calendarConfig
        -< ([peOut, urx0, urx1, urx2, urx3, urx4, urx5, urx6], switchWb)

    peOut <-
      withClockResetEnable
        bittideClk
        handshakeRstTx
        enableGen
        (switchDemoPeWb (SNat @LinkCount) (unpack <$> localCounter))
        -< (peWb, dna, peIn)

    dna <-
      withClockResetEnable
        bittideClk
        handshakeRstTx
        enableGen
        (readDnaPortE2Wb simDna2)
        -< dnaWb

    (swCcOut, ccM2S) <-
      withClockResetEnable
        refClk
        handshakeRstFree
        enableGen
        (callistoSwClockControlC @LinkCount @CccBufferSize NoDumpVcd ccConfig)
        -< (ccJtag, reframe, mask, dc)

    idleSink -< ccM2S

    idC -< (swCcOut, [tx0, tx1, tx2, tx3, tx4, tx5, tx6])

  ((jtagOut, _linkInBwd, _reframingBwd, _maskBwd, _diffsBwd, _insBwd), (callistoResult, _)) =
    circuitFn
      (
        ( jtagIn
        , pure 0 -- link in
        , pure False -- enable reframing
        , pure maxBound -- enable mask
        , resize <<$>> domainDiffs
        , rxDatasEbs
        )
      ,
        ( pure ()
        , repeat (pure ())
        )
      )

  frequencyAdjustments :: Signal Basic125 (FINC, FDEC)
  frequencyAdjustments =
    delay refClk enableGen minBound
      $ speedChangeToStickyPins
        refClk
        handshakeRstFree
        enableGen
        (SNat @Si539xHoldTime)
        callistoResult.maybeSpeedChangeC

  domainDiffs :: Vec LinkCount (Signal Basic125 FincFdecCount)
  domainDiffs =
    zipWith3
      (domainDiffCounterExt refClk)
      (repeat handshakeRstFree)
      transceivers.rxClocks
      (repeat transceivers.txClock)

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
    go rxClk rxData = resettableXilinxElasticBuffer bittideClk rxClk ebReset rxData

  fifoUnderflowsTx :: Vec LinkCount (Signal GthTx Underflow)
  fifoOverflowsTx :: Vec LinkCount (Signal GthTx Overflow)
  rxDatasEbs :: Vec LinkCount (Signal GthTx (Maybe (BitVector 64)))
  ebModes :: Vec 7 (Signal GthTx EbMode)
  (_, fifoUnderflowsTx, fifoOverflowsTx, ebModes, rxDatasEbs) = unzip5 rxFifos

  fifoOverflowsFree :: Signal Basic125 Overflow
  fifoOverflowsFree = and <$> xpmCdcArraySingle bittideClk refClk (bundle fifoOverflowsTx)

  fifoUnderflowsFree :: Signal Basic125 Underflow
  fifoUnderflowsFree = and <$> xpmCdcArraySingle bittideClk refClk (bundle fifoUnderflowsTx)

  fifoOverflowsSticky :: Signal Basic125 Bool
  fifoOverflowsSticky = sticky refClk handshakeRstFree fifoOverflowsFree

  fifoUnderflowsSticky :: Signal Basic125 Bool
  fifoUnderflowsSticky = sticky refClk handshakeRstFree fifoUnderflowsFree

demoTest ::
  "SMA_MGT_REFCLK_C" ::: DiffClock Ext200 ->
  "SYSCLK_125" ::: DiffClock Ext125 ->
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
  , "spiDone" ::: Signal Basic125 Bool
  , ""
      ::: ( "SCLK" ::: Signal Basic125 Bool
          , "MOSI" ::: Signal Basic125 Bit
          , "CSB" ::: Signal Basic125 Bool
          )
  , "JTAG" ::: Signal Basic125 JtagOut
  )
demoTest boardClkDiff refClkDiff rxns rxps miso jtagIn =
  (txns, txps, unbundle swFincFdecs, spiDone, spiOut, jtagOut)
 where
  boardClk :: Clock Ext200
  boardClk = ibufds_gte3 boardClkDiff

  refClk :: Clock Basic125
  refRst :: Reset Basic125
  (refClk, refRst) = clockWizardDifferential refClkDiff noReset

  ( testStart :: Signal Basic125 Bool
    , allProgrammed :: Signal Basic125 Bool
    ) =
      unbundle
        $ setName @"vioHitlt"
        $ vioProbe
          ("probe_test_done" :> "probe_test_success" :> Nil)
          ("probe_test_start" :> "probe_all_programmed" :> Nil)
          (False, False)
          refClk
          (testStart .&&. testDone) -- done
          (testStart .&&. testSuccess) -- success
  testReset :: Reset Basic125
  testReset = unsafeFromActiveLow testStart `orReset` refRst

  ( txns :: TransceiverWires GthTxS LinkCount
    , txps :: TransceiverWires GthTxS LinkCount
    , _handshakesDone :: Signal Basic125 Bool
    , swFincFdecs :: Signal Basic125 (Bool, Bool)
    , spiDone :: Signal Basic125 Bool
    , spiOut :: (Signal Basic125 Bool, Signal Basic125 Bit, Signal Basic125 Bool)
    , jtagOut :: Signal Basic125 JtagOut
    , transceiversFailedAfterUp :: Signal Basic125 Bool
    , allStable :: Signal Basic125 Bool
    , noFifoOverflows :: Signal Basic125 Bool
    , noFifoUnderflows :: Signal Basic125 Bool
    ) = dut refClk testReset boardClk rxns rxps allProgrammed miso jtagIn

  fifoSuccess :: Signal Basic125 Bool
  fifoSuccess = noFifoUnderflows .&&. noFifoOverflows

  endSuccess :: Signal Basic125 Bool
  endSuccess = trueFor (SNat @(Seconds 5)) refClk testReset (allStable .&&. fifoSuccess)

  testDone :: Signal Basic125 Bool
  testDone = endSuccess .||. transceiversFailedAfterUp .||. fmap not fifoSuccess

  testSuccess :: Signal Basic125 Bool
  testSuccess = allStable .&&. fifoSuccess .&&. fmap not transceiversFailedAfterUp
{-# OPAQUE demoTest #-}
makeTopEntity 'demoTest

tests :: HitlTestGroup
tests =
  HitlTestGroup
    { topEntity = 'demoTest
    , extraXdcFiles = ["jtag" </> "config.xdc", "jtag" </> "pmod1.xdc"]
    , externalHdl = []
    , testCases =
        [ HitlTestCase
            { name = "Bittide Demo DUT"
            , parameters = paramForHwTargets allHwTargets ()
            , postProcData = ()
            }
        ]
    , mDriverProc = Just D.driverFunc
    , mPostProc = Nothing
    }
