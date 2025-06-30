-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

{- | Switch demo for a Bittide system. In concert with its driver file, this device under
test should demonstrate the predictability of a Bittide system once it has achieved logical
synchronicity.

For more details, see [QBayLogic's presentation](https://docs.google.com/presentation/d/1AGbAJQ1zhTPtrekKnQcthd0TUPyQs-zowQpV1ux4k-Y)
on the topic.
-}
module Bittide.Instances.Hitl.SwitchDemo (
  switchDemoDut,
  switchDemoTest,
  memoryMapCc,
  memoryMapMu,
  memoryMapGppe,
  tests,
) where

import Clash.Explicit.Prelude
import Clash.Prelude (HiddenClockResetEnable, withClockResetEnable)

import Bittide.Arithmetic.Time (trueFor)
import Bittide.Calendar (CalendarConfig (..), ValidEntry (..))
import Bittide.ClockControl hiding (speedChangeToFincFdec)
import Bittide.ClockControl.Callisto.Types (CallistoCResult)
import Bittide.ClockControl.CallistoSw (
  SwControlCConfig (..),
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
import Bittide.Node
import Bittide.ProcessingElement (PeConfig (..))
import Bittide.ScatterGather (GatherConfig (..), ScatterConfig (..))
import Bittide.Switch (CalendarEntry)
import Bittide.Transceiver (transceiverPrbsN)
import Bittide.Wishbone (idleWbMM)

import BitPackC (ByteOrder (BigEndian, LittleEndian))
import Clash.Annotations.TH (makeTopEntity)
import Clash.Cores.Xilinx.Ila (Depth (..), IlaConfig (..), ila, ilaConfig)
import Clash.Cores.Xilinx.VIO (vioProbe)
import Clash.Cores.Xilinx.Xpm.Cdc (xpmCdcArraySingle, xpmCdcSingle)
import Clash.Functor.Extra ((<<$>>))
import Clash.Sized.Extra (unsignedToSigned)
import Clash.Sized.Vector.ToTuple (vecToTuple)
import Clash.Xilinx.ClockGen (clockWizardDifferential)
import GHC.Stack (HasCallStack)
import Protocols
import System.FilePath ((</>))
import VexRiscv (DumpVcd (..), Jtag, JtagIn (..), JtagOut (..))

import qualified Bittide.Instances.Hitl.Driver.SwitchDemo as D
import qualified Bittide.Transceiver as Transceiver
import qualified Clash.Cores.Xilinx.GTH as Gth
import qualified Protocols.MemoryMap as MM

type NumGppes = 1

{- FOURMOLU_DISABLE -} -- Fourmolu doesn't do well with tabular code
calConf :: CalendarConfig
  4
  (NmuRemBusWidth LinkCount NumGppes)
  (CalendarEntry (LinkCount + NumGppes + 1))
calConf =
  CalendarConfig
    { maxCalDepth = SNat @LinkCount

    -- Active calendar. It will broadcast the PE (node 1) data to all links. Other
    -- than that we cycle through the other nodes.
    , activeCalendar =
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
    , inactiveCalendar = (ValidEntry (repeat 0) 0 :> Nil)
    }
 where
  -- We want enough time to read _number of FPGAs_ triplets
  nRepetitions = bitCoerce (maxBound :: Index (FpgaCount * 3))
{- FOURMOLU_ENABLE -}

memoryMapCc :: MM.MemoryMap
memoryMapCc = let (mm, _, _) = memoryMaps in mm

memoryMapMu :: MM.MemoryMap
memoryMapMu = let (_, mm, _) = memoryMaps in mm

memoryMapGppe :: MM.MemoryMap
memoryMapGppe = let (_, _, mm) = memoryMaps in mm

memoryMaps :: ("CC" ::: MM.MemoryMap, "MU" ::: MM.MemoryMap, "GPPE" ::: MM.MemoryMap)
memoryMaps = (ccMm, muMm, gppeMm)
 where
  SimOnly gppeMm = head gppeMms
  ((SimOnly ccMm, SimOnly muMm, gppeMms, _, _, _, _, _), _) =
    switchCircuitFn
      (
        ( ()
        , ()
        , repeat ()
        , pure (JtagIn 0 0 0)
        , pure False
        , pure maxBound
        , repeat (pure 0)
        , repeat (pure Nothing)
        )
      ,
        ( pure ()
        , repeat (pure ())
        , pure ()
        , repeat (pure ())
        , repeat (pure ())
        , pure ()
        )
      )
  Circuit switchCircuitFn =
    switchCircuit (clockGen, resetGen, enableGen) (clockGen, resetGen, enableGen)
   where
    ?busByteOrder = BigEndian
    ?regByteOrder = LittleEndian

nodeConfig :: NodeConfig LinkCount NumGppes
nodeConfig =
  NodeConfig
    { managementConfig = muConfig
    , calendarConfig = calConf
    , gppeConfigs = gppeConfig0 :> Nil
    }

muConfig :: ManagementConfig LinkCount NumGppes
muConfig =
  ManagementConfig
    { scatterConfig =
        ScatterConfig
          { memDepth = SNat @(LinkCount + NumGppes + 1)
          , calendarConfig =
              CalendarConfig
                { maxCalDepth = SNat @LinkCount
                , activeCalendar = repeat @LinkCount (ValidEntry @_ @1 0 0)
                , inactiveCalendar = repeat @LinkCount (ValidEntry @_ @1 0 0)
                }
          }
    , scatterCalPrefix = 0b01011
    , scatterPrefix = 0b01100
    , gatherConfig =
        GatherConfig
          { memDepth = SNat @(LinkCount + NumGppes + 1)
          , calendarConfig =
              CalendarConfig
                { maxCalDepth = SNat @LinkCount
                , activeCalendar = repeat @LinkCount (ValidEntry @_ @1 0 0)
                , inactiveCalendar = repeat @LinkCount (ValidEntry @_ @1 0 0)
                }
          }
    , gatherCalPrefix = 0b01101
    , gatherPrefix = 0b01110
    , timerPrefix = 0b01000
    , switchPrefix = 0b01001
    , externalPrefix = 0b01010
    , captureUgnPrefixes =
        0b00001
          :> 0b00010
          :> 0b00011
          :> 0b00100
          :> 0b00101
          :> 0b00110
          :> 0b00111
          :> Nil
    , peScatterGatherPrefixes = (0b01111, 0b10001) :> Nil
    , peConfig =
        PeConfig
          { prefixI = 0b10000
          , prefixD = 0b11000
          , initI = Undefined @(Div (64 * 1024) 4)
          , initD = Undefined @(Div (64 * 1024) 4)
          , iBusTimeout = d0
          , dBusTimeout = d0
          , includeIlaWb = False
          , whoAmID = D.muWhoAmID
          , whoAmIPrefix = D.whoAmIPrefix
          }
    , dumpVcd = NoDumpVcd
    }

gppeConfig0 :: GppeConfig (NmuRemBusWidth LinkCount NumGppes)
gppeConfig0 =
  GppeConfig
    { scatterConfig =
        ScatterConfig
          { memDepth = SNat @(LinkCount + NumGppes + 1)
          , calendarConfig =
              CalendarConfig
                { maxCalDepth = SNat @LinkCount
                , activeCalendar = repeat @LinkCount (ValidEntry @_ @1 0 0)
                , inactiveCalendar = repeat @LinkCount (ValidEntry @_ @1 0 0)
                }
          }
    , scatterPrefix = 0b011
    , gatherConfig =
        GatherConfig
          { memDepth = SNat @(LinkCount + NumGppes + 1)
          , calendarConfig =
              CalendarConfig
                { maxCalDepth = SNat @LinkCount
                , activeCalendar = repeat @LinkCount (ValidEntry @_ @1 0 0)
                , inactiveCalendar = repeat @LinkCount (ValidEntry @_ @1 0 0)
                }
          }
    , gatherPrefix = 0b100
    , peConfig =
        PeConfig
          { prefixI = 0b001
          , prefixD = 0b010
          , initI = Undefined @(Div (32 * 1024) 4)
          , initD = Undefined @(Div (32 * 1024) 4)
          , iBusTimeout = d0
          , dBusTimeout = d0
          , includeIlaWb = False
          , whoAmID = D.gppeWhoAmID
          , whoAmIPrefix = D.whoAmIPrefix
          }
    , dumpVcd = NoDumpVcd
    }

ccConfig ::
  SwControlCConfig CccStabilityCheckerMargin (CccStabilityCheckerFramesize Basic125) 0
ccConfig =
  SwControlCConfig
    { margin = SNat
    , framesize = SNat
    , ccRegPrefix = 0b110
    , dbgRegPrefix = 0b101
    , timePrefix = 0b011
    , peConfig =
        PeConfig
          { prefixI = 0b100
          , prefixD = 0b010
          , initI = Undefined @(Div (64 * 1024) 4)
          , initD = Undefined @(Div (64 * 1024) 4)
          , iBusTimeout = d0
          , dBusTimeout = d0
          , includeIlaWb = True
          , whoAmID = D.ccWhoAmID
          , whoAmIPrefix = D.whoAmIPrefix
          }
    }

switchCircuit ::
  ( HasCallStack
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
  (Clock Basic125, Reset Basic125, Enable Basic125) ->
  (Clock GthTx, Reset GthTx, Enable GthTx) ->
  Circuit
    ( MM.ConstBwd MM.MM
    , MM.ConstBwd MM.MM
    , Vec NumGppes (MM.ConstBwd MM.MM)
    , Jtag Basic125
    , CSignal Basic125 Bool
    , CSignal Basic125 (BitVector LinkCount)
    , Vec LinkCount (CSignal Basic125 (RelDataCount CccBufferSize))
    , Vec LinkCount (CSignal GthTx (Maybe (BitVector 64)))
    )
    ( CSignal Basic125 (CallistoCResult LinkCount)
    , Vec LinkCount (CSignal GthTx (BitVector 64))
    , CSignal GthTx (Unsigned 64)
    , Vec NumGppes (CSignal GthTx (BitVector 64))
    , Vec NumGppes (CSignal GthTx (BitVector 64))
    , CSignal GthTx (Vec (LinkCount + NumGppes + 1) (Index (LinkCount + NumGppes + 2)))
    )
switchCircuit (refClk, refRst, refEna) (bitClk, bitRst, bitEna) =
  circuit $ \(ccMM, muMM, gppeMMs, jtag, reframe, mask, dc, rxs) -> do
    [nodeJtagFree, ccJtag] <- jtagChain -< jtag
    nodeJtagTx <- unsafeJtagSynchronizer refClk bitClk -< nodeJtagFree

    (txs, lc, (externalMM, externalWb), peIn, peOut, ce) <-
      defaultBittideClkRstEn $ node nodeConfig -< (muMM, gppeMMs, nodeJtagTx, rxs)
    defaultBittideClkRstEn $ idleWbMM -< (externalMM, externalWb)

    (swCcOut, []) <-
      defaultRefClkRstEn (callistoSwClockControlC @LinkCount @CccBufferSize NoDumpVcd ccConfig)
        -< (ccMM, (ccJtag, reframe, mask, dc))

    idC -< (swCcOut, txs, lc, peIn, peOut, ce)
 where
  defaultRefClkRstEn :: forall r. ((HiddenClockResetEnable Basic125) => r) -> r
  defaultRefClkRstEn = withClockResetEnable refClk refRst refEna
  defaultBittideClkRstEn :: forall r. ((HiddenClockResetEnable GthTx) => r) -> r
  defaultBittideClkRstEn = withClockResetEnable bitClk bitRst bitEna

{- | Reset logic:

HW:

  1. Wait for SPI
  2. Wait for transceivers handshakes (=> all domains are up after this)
  3. Send local counter for one cycle, connect to switch after (=> in parallel
     with steps 4 and onwards, just wait until the transceiver says it's sampling from the
     transmit data input (@txDatas@))
  4a. Deassert CC CPU reset
  4b. Deassert Bittide domain reset (=> MU CPU, PE)
  5. Wait for stable buffers
  6. Wait for elastic buffer initialization (=> signal we're ready to receive data)

SW (MU):

  1. Wait for all UGNs to be captured
-}
switchDemoDut ::
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
switchDemoDut refClk refRst skyClk rxSims rxNs rxPs allProgrammed miso jtagIn =
  hwSeqX
    debugIla
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
      0
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
      0
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
        , rxSims
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

  Circuit circuitFn =
    switchCircuit
      (refClk, handshakeRstFree, enableGen)
      (bittideClk, handshakeRstTx, enableGen)
   where
    ?busByteOrder = BigEndian
    ?regByteOrder = LittleEndian
  ( (_, _, _, jtagOut, _, _, _, _)
    , (callistoResult, switchDataOut, localCounter, _peInput, _peOutput, _calendarEntries)
    ) =
      circuitFn
        (
          ( ()
          , ()
          , repeat ()
          , jtagIn
          , pure False
          , pure maxBound
          , resize <<$>> domainDiffs
          , rxDatasEbs
          )
        ,
          ( pure ()
          , repeat (pure ())
          , pure ()
          , repeat (pure ())
          , repeat (pure ())
          , pure ()
          )
        )

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
    ) = switchDemoDut refClk testReset boardClk rxs rxns rxps allProgrammed miso jtagIn

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
