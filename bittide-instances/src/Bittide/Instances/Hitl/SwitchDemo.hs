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

module Bittide.Instances.Hitl.SwitchDemo (dut, switchDemoTest, tests) where

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
  FpgaCount,
  LinkCount,
  allHwTargets,
  channelNames,
  clockPaths,
 )
import Bittide.Instances.Hitl.SwCcTopologies (FifoSize, FincFdecCount, commonSpiConfig)
import Bittide.Jtag (jtagChain, unsafeJtagSynchronizer)
import Bittide.ProcessingElement (PeConfig (..), processingElement, splitAtC)
import Bittide.SharedTypes (Bytes)
import Bittide.Switch (switchC)
import Bittide.SwitchDemoProcessingElement (SimplePeState (Idle), switchDemoPeWb)
import Bittide.Transceiver (transceiverPrbsN)
import Bittide.Wishbone (readDnaPortE2Wb, timeWb, wbStallUntil, whoAmIC)

import Clash.Annotations.TH (makeTopEntity)
import Clash.Cores.Xilinx.Ila (Depth (..), IlaConfig (..), ila, ilaConfig)
import Clash.Cores.Xilinx.Unisim.DnaPortE2 (simDna2)
import Clash.Cores.Xilinx.VIO (vioProbe)
import Clash.Cores.Xilinx.Xpm.Cdc (xpmCdcArraySingle, xpmCdcSingle)
import Clash.Functor.Extra ((<<$>>))
import Clash.Sized.Extra (unsignedToSigned)
import Clash.Sized.Vector.ToTuple (vecToTuple)
import Clash.Xilinx.ClockGen (clockWizardDifferential)
import Protocols
import Protocols.Extra (fanoutC)
import Protocols.Wishbone
import System.FilePath ((</>))
import VexRiscv (DumpVcd (..), Jtag, JtagIn (..), JtagOut (..))

import qualified Bittide.Instances.Hitl.Driver.SwitchDemo as D
import qualified Bittide.Transceiver as Transceiver
import qualified Clash.Cores.Xilinx.GTH as Gth

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
    ( CSignal bitDom (Unsigned 64)
    , Vec nodeBusses (Wishbone bitDom 'Standard (NmuRemBusWidth nodeBusses) (Bytes 4))
    )
simpleManagementUnitC (SimpleManagementConfig peConfig dumpVcd) =
  circuit $ \(jtag, _linkIn) -> do
    peWbs <- processingElement dumpVcd peConfig -< jtag
    ([timeWbBus], nmuWbs) <- splitAtC d1 -< peWbs
    localCounter <- timeWb -< timeWbBus
    idC -< (localCounter, nmuWbs)

{- FOURMOLU_DISABLE -} -- Fourmolu doesn't do well with tabular code
calendarConfig :: CalendarConfig 4 26 (Vec 8 (Index 9))
calendarConfig =
  CalendarConfig
    (SNat @LinkCount)

    -- Active calendar. It will broadcast the PE (node 1) data to all links. Other
    -- than that we cycle through the other nodes.
    (      ValidEntry (2 :> repeat 1) nRepetitions
        :> ValidEntry (3 :> repeat 1) nRepetitions
        :> ValidEntry (4 :> repeat 1) nRepetitions
        :> ValidEntry (5 :> repeat 1) nRepetitions
        :> ValidEntry (6 :> repeat 1) nRepetitions
        :> ValidEntry (7 :> repeat 1) nRepetitions
        :> ValidEntry (8 :> repeat 1) nRepetitions
        :> Nil
    )

    -- Don't care about inactive calendar:
    (ValidEntry (repeat 0) 0 :> Nil)
  where
  -- We want enough time to read _number of FPGAs_ triplets
  nRepetitions = bitCoerce (maxBound :: Index (FpgaCount * 3))
{- FOURMOLU_ENABLE -}

muConfig :: SimpleManagementConfig 11
muConfig =
  SimpleManagementConfig
    PeConfig
      { memMapConfig =
          0b1000 -- IMEM
            :> 0b1100 -- DMEM
            :> 0b1101 -- timeWb
            :> 0b1110 -- whoAmI
            :> 0b1001 -- peWb
            :> 0b1010 -- switchWb
            :> 0b1011 -- dnaWb
            :> 0b0001 -- UGN0
            :> 0b0010 -- UGN1
            :> 0b0011 -- UGN2
            :> 0b0100 -- UGN3
            :> 0b0101 -- UGN4
            :> 0b0110 -- UGN5
            :> 0b0111 -- UGN6
            :> Nil
      , initI = Undefined @(Div (64 * 1024) 4)
      , initD = Undefined @(Div (64 * 1024) 4)
      , iBusTimeout = d0
      , dBusTimeout = d0
      , includeIlaWb = False
      }
    NoDumpVcd

ccConfig ::
  SwControlCConfig CccStabilityCheckerMargin (CccStabilityCheckerFramesize Basic125) 2
ccConfig =
  SwControlCConfig
    SNat
    SNat
    PeConfig
      { memMapConfig = 0b100 :> 0b010 :> 0b110 :> 0b101 :> 0b111 :> 0b011 :> Nil
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
  "GTH_RX_S" ::: Gth.SimWires GthRx LinkCount ->
  "GTH_RX_NS" ::: Gth.Wires GthRxS LinkCount ->
  "GTH_RX_PS" ::: Gth.Wires GthRxS LinkCount ->
  "allProgrammed" ::: Signal Basic125 Bool ->
  "MISO" ::: Signal Basic125 Bit ->
  "JTAG_IN" ::: Signal Basic125 JtagIn ->
  ( "GTH_TX_S" ::: Gth.SimWires GthTx LinkCount
  , "GTH_TX_NS" ::: Gth.Wires GthTxS LinkCount
  , "GTH_TX_PS" ::: Gth.Wires GthTxS LinkCount
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
dut refClk refRst skyClk rxs rxNs rxPs allProgrammed miso jtagIn =
  hwSeqX
    (bundle (debugIla, bittidePeIla))
    ( transceivers.txSims
    , transceivers.txNs
    , transceivers.txPs
    , handshakesDoneFree
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
  debugIla :: Signal Basic125 ()
  debugIla =
    setName @"demoDebugIla"
      ila
      ( ilaConfig
          $ "trigger_fdi_dd"
          :> "capture_fdi_dd"
          -- Important step 1 signals
          :> "dd_spiDone"
          :> "dd_spiErr"
          -- Important step 2 signals
          :> "dd_handshakesDoneFree"
          -- Important step 3 signals
          :> "dd_txStarts"
          -- Important step 4 signals
          -- Important step 5 signals
          :> "dd_allStable"
          -- Important step 6 signals
          :> "dd_ebReadys"
          -- Other
          :> "dd_transceiversFailedAfterUp"
          :> "dd_nFincs"
          :> "dd_nFdecs"
          :> "dd_netFincs"
          :> "dd_dDiff0"
          :> "dd_dDiff1"
          :> "dd_dDiff2"
          :> "dd_dDiff3"
          :> "dd_dDiff4"
          :> "dd_dDiff5"
          :> "dd_dDiff6"
          :> Nil
      )
        { depth = D32768
        }
      refClk
      (unsafeToActiveLow handshakeRstFree)
      captureFlag
      spiDone
      spiErr
      (bundle transceivers.handshakesDoneFree)
      (bundle $ xpmCdcArraySingle bittideClk refClk <$> txStarts)
      allStable
      (bundle $ xpmCdcArraySingle bittideClk refClk <$> ebReadys)
      transceiversFailedAfterUp
      nFincs
      nFdecs
      (fmap unsignedToSigned nFincs - fmap unsignedToSigned nFdecs)
      dDiff0
      dDiff1
      dDiff2
      dDiff3
      dDiff4
      dDiff5
      dDiff6

  captureFlag =
    riseEvery
      refClk
      spiRst
      enableGen
      (SNat @(PeriodToCycles Basic125 (Milliseconds 2)))

  nFincs :: Signal Basic125 (Unsigned 32)
  nFincs =
    regEn
      refClk
      refRst
      enableGen
      (0 :: Unsigned 32)
      ( isFalling
          refClk
          refRst
          enableGen
          False
          ((== Just SpeedUp) <$> callistoResult.maybeSpeedChangeC)
      )
      (satSucc SatBound <$> nFincs)

  nFdecs :: Signal Basic125 (Unsigned 32)
  nFdecs =
    regEn
      refClk
      refRst
      enableGen
      (0 :: Unsigned 32)
      ( isFalling
          refClk
          refRst
          enableGen
          False
          ((== Just SlowDown) <$> callistoResult.maybeSpeedChangeC)
      )
      (satSucc SatBound <$> nFdecs)

  ( dDiff0
    , dDiff1
    , dDiff2
    , dDiff3
    , dDiff4
    , dDiff5
    , dDiff6
    ) = vecToTuple domainDiffs

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
        , rxSims = rxs
        , rxNs
        , rxPs
        , txDatas = txDatas
        , txStarts = txStarts
        , rxReadys = ebReadysRx
        }

  bittideClk :: Clock GthTx
  bittideClk = transceivers.txClock

  handshakesDoneFree :: Signal Basic125 Bool
  handshakesDoneFree = and <$> bundle transceivers.handshakesDoneFree

  handshakesDoneTx :: Signal GthTx Bool
  handshakesDoneTx = and <$> bundle transceivers.handshakesDoneTx

  -- Step 3, send local counter for one cycle, connect to switch after:
  txSamplingsDelayed :: Vec 7 (Signal GthTx Bool)
  txSamplingsDelayed =
    register bittideClk handshakeRstTx enableGen False <$> transceivers.txSamplings

  txDatas :: Vec 7 (Signal GthTx (BitVector 64))
  txDatas = mux <$> txSamplingsDelayed <*> switchDataOut <*> repeat (pack <$> localCounter)

  txStarts :: Vec 7 (Signal GthTx Bool)
  txStarts = repeat allStableStickyTx

  -- Step 4, deassert CC CPU reset, deassert Bittide domain reset:
  handshakeRstFree :: Reset Basic125
  handshakeRstFree = unsafeFromActiveLow handshakesDoneFree

  handshakeRstTx :: Reset GthTx
  handshakeRstTx = unsafeFromActiveLow handshakesDoneTx

  -- localCounter :: Signal GthTx (BitVector 64)
  -- localCounter = register bittideClk handshakeRstTx enableGen 0 (localCounter + 1)

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
    sticky refClk refRst (isFalling refClk spiRst enableGen False handshakesDoneFree)

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
      , Signal GthTx ()
      , Signal GthTx ()
      , Signal GthTx ()
      , Signal GthTx ()
      , Signal GthTx ()
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
      , Signal GthTx (Unsigned 64)
      , Signal GthTx (SimplePeState FpgaCount)
      , Signal GthTx (BitVector 64)
      , Signal GthTx (BitVector 64)
      , Signal GthTx (Vec (LinkCount + 1) (Index (LinkCount + 2)))
      )
    )
  Circuit circuitFn = circuit $ \(jtag, linkIn, reframe, mask, dc, [rx0, rx1, rx2, rx3, rx4, rx5, rx6]) -> do
    [muJtagFree, ccJtag] <- jtagChain -< jtag
    muJtagTx <- unsafeJtagSynchronizer refClk bittideClk -< muJtagFree

    (lc, [muWhoAmI, peWb, switchWb, dnaWb, ugn0, ugn1, ugn2, ugn3, ugn4, ugn5, ugn6]) <-
      withClockResetEnable
        bittideClk
        handshakeRstTx
        enableGen
        (simpleManagementUnitC muConfig)
        -< (muJtagTx, linkIn)

    [lc0, lc1, lc2, lc3, lc4, lc5, lc6, lc7, lc8] <- fanoutC -< lc

    [urx0, urx1, urx2, urx3, urx4, urx5, urx6] <-
      withClockResetEnable
        bittideClk
        handshakeRstTx
        enableGen
        (repeatC captureUgn)
        -< [ (lc0, ugn0, rx0)
           , (lc1, ugn1, rx1)
           , (lc2, ugn2, rx2)
           , (lc3, ugn3, rx3)
           , (lc4, ugn4, rx4)
           , (lc5, ugn5, rx5)
           , (lc6, ugn6, rx6)
           ]

    ([peIn, tx0, tx1, tx2, tx3, tx4, tx5, tx6], ce) <-
      withClockResetEnable
        bittideClk
        handshakeRstTx
        enableGen
        switchC
        calendarConfig
        -< ([peOut0, urx0, urx1, urx2, urx3, urx4, urx5, urx6], switchWb)
    [peIn0, peIn1] <- fanoutC -< peIn

    (peOut, ps) <-
      withClockResetEnable
        bittideClk
        handshakeRstTx
        enableGen
        (switchDemoPeWb (SNat @FpgaCount))
        -< (lc7, peWb, dna, peIn0)
    [peOut0, peOut1] <- fanoutC -< peOut

    dna <-
      withClockResetEnable
        bittideClk
        handshakeRstTx
        enableGen
        (readDnaPortE2Wb simDna2)
        -< dnaWb

    withClockResetEnable
      bittideClk
      handshakeRstTx
      enableGen
      (whoAmIC 0x746d_676d)
      -< muWhoAmI

    (swCcOut, [ccWhoAmI, wbStall]) <-
      withClockResetEnable
        refClk
        handshakeRstFree
        enableGen
        (callistoSwClockControlC @LinkCount @CccBufferSize NoDumpVcd ccConfig)
        -< (ccJtag, reframe, mask, dc)

    withClockResetEnable
      refClk
      handshakeRstFree
      enableGen
      (whoAmIC 0x6363_7773)
      -< ccWhoAmI

    withClockResetEnable
      refClk
      handshakeRstFree
      enableGen
      wbStallUntil
      -< (wbStall, Fwd (pure True))

    idC -< (swCcOut, [tx0, tx1, tx2, tx3, tx4, tx5, tx6], lc8, ps, peIn1, peOut1, ce)

  ( (jtagOut, _linkInBwd, _reframingBwd, _maskBwd, _diffsBwd, _insBwd)
    , (callistoResult, switchDataOut, localCounter, peState, peInput, peOutput, calEntry)
    ) =
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
          , pure ()
          , pure ()
          , pure ()
          , pure ()
          , pure ()
          )
        )

  peNotIdle :: Signal GthTx Bool
  peNotIdle = (/= Idle) <$> peState
  peNotIdleSticky :: Signal GthTx Bool
  peNotIdleSticky = sticky bittideClk handshakeRstTx peNotIdle
  peNotIdleStickyFree :: Signal Basic125 Bool
  peNotIdleStickyFree = xpmCdcSingle bittideClk refClk peNotIdleSticky

  bittidePeIla :: Signal Basic125 ()
  bittidePeIla =
    setName @"bittidePeIla"
      ila
      ( ilaConfig
          $ "trigger_fdi_pe"
          :> "capture_fdi_pe"
          :> "pe_input"
          :> "pe_state"
          :> "pe_output"
          :> "pe_local_counter"
          :> "pe_active_cal_entry"
          :> "pe_rx_0"
          :> "pe_rx_1"
          :> "pe_rx_2"
          :> "pe_rx_3"
          :> "pe_rx_4"
          :> "pe_rx_5"
          :> "pe_rx_6"
          :> "pe_tx_0"
          :> "pe_tx_1"
          :> "pe_tx_2"
          :> "pe_tx_3"
          :> "pe_tx_4"
          :> "pe_tx_5"
          :> "pe_tx_6"
          :> Nil
      )
        { depth = D4096
        }
      refClk
      peNotIdleStickyFree
      (pure True :: Signal Basic125 Bool)
      (xpmCdcArraySingle bittideClk refClk peInput)
      (pack <$> xpmCdcArraySingle bittideClk refClk peState)
      (xpmCdcArraySingle bittideClk refClk peOutput)
      (xpmCdcArraySingle bittideClk refClk localCounter)
      (xpmCdcArraySingle bittideClk refClk calEntry)
      (xpmCdcArraySingle bittideClk refClk (rxDatasEbs !! (0 :: Index LinkCount)))
      (xpmCdcArraySingle bittideClk refClk (rxDatasEbs !! (1 :: Index LinkCount)))
      (xpmCdcArraySingle bittideClk refClk (rxDatasEbs !! (2 :: Index LinkCount)))
      (xpmCdcArraySingle bittideClk refClk (rxDatasEbs !! (3 :: Index LinkCount)))
      (xpmCdcArraySingle bittideClk refClk (rxDatasEbs !! (4 :: Index LinkCount)))
      (xpmCdcArraySingle bittideClk refClk (rxDatasEbs !! (5 :: Index LinkCount)))
      (xpmCdcArraySingle bittideClk refClk (rxDatasEbs !! (6 :: Index LinkCount)))
      (xpmCdcArraySingle bittideClk refClk (switchDataOut !! (0 :: Index LinkCount)))
      (xpmCdcArraySingle bittideClk refClk (switchDataOut !! (1 :: Index LinkCount)))
      (xpmCdcArraySingle bittideClk refClk (switchDataOut !! (2 :: Index LinkCount)))
      (xpmCdcArraySingle bittideClk refClk (switchDataOut !! (3 :: Index LinkCount)))
      (xpmCdcArraySingle bittideClk refClk (switchDataOut !! (4 :: Index LinkCount)))
      (xpmCdcArraySingle bittideClk refClk (switchDataOut !! (5 :: Index LinkCount)))
      (xpmCdcArraySingle bittideClk refClk (switchDataOut !! (6 :: Index LinkCount)))

  frequencyAdjustments :: Signal Basic125 (FINC, FDEC)
  frequencyAdjustments =
    delay refClk enableGen minBound
      $ speedChangeToStickyPins
        refClk
        (unsafeFromActiveLow allProgrammed)
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
  fifoOverflowsFree = or <$> xpmCdcArraySingle bittideClk refClk (bundle fifoOverflowsTx)

  fifoUnderflowsFree :: Signal Basic125 Underflow
  fifoUnderflowsFree = or <$> xpmCdcArraySingle bittideClk refClk (bundle fifoUnderflowsTx)

  fifoOverflowsSticky :: Signal Basic125 Bool
  fifoOverflowsSticky = sticky refClk handshakeRstFree fifoOverflowsFree

  fifoUnderflowsSticky :: Signal Basic125 Bool
  fifoUnderflowsSticky = sticky refClk handshakeRstFree fifoUnderflowsFree

switchDemoTest ::
  "SMA_MGT_REFCLK_C" ::: DiffClock Ext200 ->
  "SYSCLK_125" ::: DiffClock Ext125 ->
  "GTH_RX_S" ::: Gth.SimWires GthRx LinkCount ->
  "GTH_RX_NS" ::: Gth.Wires GthRxS LinkCount ->
  "GTH_RX_PS" ::: Gth.Wires GthRxS LinkCount ->
  "MISO" ::: Signal Basic125 Bit ->
  "JTAG" ::: Signal Basic125 JtagIn ->
  ( "GTH_TX_S" ::: Gth.SimWires GthTx LinkCount
  , "GTH_TX_NS" ::: Gth.Wires GthTxS LinkCount
  , "GTH_TX_PS" ::: Gth.Wires GthTxS LinkCount
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
switchDemoTest boardClkDiff refClkDiff rxs rxns rxps miso jtagIn =
  hwSeqX testIla (txs, txns, txps, unbundle swFincFdecs, spiDone, spiOut, jtagOut)
 where
  boardClk :: Clock Ext200
  boardClk = Gth.ibufds_gte3 boardClkDiff

  refClk :: Clock Basic125
  refRst :: Reset Basic125
  (refClk, refRst) = clockWizardDifferential refClkDiff noReset

  ( testStart :: Signal Basic125 Bool
    , allProgrammed :: Signal Basic125 Bool
    ) =
      unbundle
        $ setName @"vioHitlt"
        $ vioProbe
          ("probe_test_done" :> "probe_test_success" :> "probe_handshakes_done" :> Nil)
          ("probe_test_start" :> "probe_all_programmed" :> Nil)
          (False, False)
          refClk
          (testStart .&&. testDone) -- done
          (testStart .&&. testSuccess) -- success
          handshakesDone
  testReset :: Reset Basic125
  testReset = unsafeFromActiveLow testStart `orReset` refRst

  ( txs :: Gth.SimWires GthTx LinkCount
    , txns :: Gth.Wires GthTxS LinkCount
    , txps :: Gth.Wires GthTxS LinkCount
    , handshakesDone :: Signal Basic125 Bool
    , swFincFdecs :: Signal Basic125 (Bool, Bool)
    , spiDone :: Signal Basic125 Bool
    , spiOut :: (Signal Basic125 Bool, Signal Basic125 Bit, Signal Basic125 Bool)
    , jtagOut :: Signal Basic125 JtagOut
    , transceiversFailedAfterUp :: Signal Basic125 Bool
    , allStable :: Signal Basic125 Bool
    , fifoOverflows :: Signal Basic125 Bool
    , fifoUnderflows :: Signal Basic125 Bool
    ) = dut refClk testReset boardClk rxs rxns rxps allProgrammed miso jtagIn

  fifoSuccess :: Signal Basic125 Bool
  fifoSuccess = not <$> (fifoUnderflows .||. fifoOverflows)

  endSuccess :: Signal Basic125 Bool
  endSuccess = trueFor (SNat @(Seconds 5)) refClk testReset (allStable .&&. fifoSuccess)

  testDone :: Signal Basic125 Bool
  testDone = endSuccess .||. transceiversFailedAfterUp .||. fmap not fifoSuccess

  testSuccess :: Signal Basic125 Bool
  testSuccess = allStable .&&. fifoSuccess .&&. fmap not transceiversFailedAfterUp

  testIla :: Signal Basic125 ()
  testIla =
    setName @"demoTestIla"
      ila
      ( ilaConfig
          $ "trigger_fdi_dt"
          :> "capture_fdi_dt"
          :> "dt_handshakesDone"
          :> "dt_all_programmed"
          :> "dt_swFincFdecs"
          :> "dt_spiDone"
          :> "dt_spiOut"
          :> "dt_jtagOut"
          :> "dt_transceiversFailedAfterUp"
          :> "dt_allStable"
          :> "dt_fifoOverflows"
          :> "dt_fifoUnderflows"
          :> Nil
      )
        { depth = D32768
        }
      refClk
      handshakesDone
      captureFlag
      handshakesDone
      allProgrammed
      swFincFdecs
      spiDone
      (bundle spiOut)
      jtagOut
      transceiversFailedAfterUp
      allStable
      fifoOverflows
      fifoUnderflows

  captureFlag :: Signal Basic125 Bool
  captureFlag =
    riseEvery
      refClk
      testReset
      enableGen
      (SNat @(PeriodToCycles Basic125 (Milliseconds 2)))
{-# OPAQUE switchDemoTest #-}
makeTopEntity 'switchDemoTest

tests :: HitlTestGroup
tests =
  HitlTestGroup
    { topEntity = 'switchDemoTest
    , extraXdcFiles = ["jtag" </> "config.xdc", "jtag" </> "pmod1.xdc"]
    , externalHdl = []
    , testCases =
        [ HitlTestCase
            { name = "Bittide Demo DUT"
            , parameters = paramForHwTargets allHwTargets ()
            , postProcData = ()
            }
        ]
    , mDriverProc = Just D.driver
    , mPostProc = Nothing
    }
