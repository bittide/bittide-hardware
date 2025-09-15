-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

{- | Switch demo for a Bittide system. In concert with its driver file, this device under
test should demonstrate the predictability of a Bittide system once it has achieved logical
synchronicity.

For more details, see [QBayLogic's presentation](https://docs.google.com/presentation/d/1AGbAJQ1zhTPtrekKnQcthd0TUPyQs-zowQpV1ux4k-Y)
on the topic.
-}
module Bittide.Instances.Hitl.Dut.SoftUgnDemo (
  softUgnDemoDut,
  softUgnDemoTest,
  memoryMapCc,
  memoryMapMu,
  tests,
) where

import Clash.Explicit.Prelude
import Clash.Prelude (HiddenClockResetEnable, withClockResetEnable)

import Bittide.Calendar (CalendarConfig (..), ValidEntry (..))
import Bittide.CaptureUgn (captureUgn)
import Bittide.ClockControl
import Bittide.ClockControl.Callisto.Types (
  CallistoResult (..),
  Stability (..),
 )
import Bittide.ClockControl.CallistoSw (
  SwcccInternalBusses,
  callistoSwClockControlC,
 )
import Bittide.ClockControl.Si539xSpi (ConfigState (Error, Finished), si539xSpi)
import Bittide.Df (asciiDebugMux)
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
import Bittide.Instances.Common (commonSpiConfig)
import Bittide.Instances.Domains (
  Basic125,
  Bittide,
  Ext125,
  Ext200,
  GthRx,
  GthRxS,
  GthTxS,
 )
import Bittide.Instances.Hitl.Setup (
  LinkCount,
  allHwTargets,
  channelNames,
  clockPaths,
 )
import Bittide.Jtag (jtagChain)
import Bittide.ProcessingElement (
  PeConfig (..),
  PeInternalBusses,
  PrefixWidth,
  processingElement,
 )
import Bittide.ScatterGather
import Bittide.SharedTypes (Byte, Bytes, withBittideByteOrder)
import Bittide.Transceiver (transceiverPrbsN)
import Bittide.Wishbone (
  readDnaPortE2Wb,
  timeWb,
  uartBytes,
  uartDf,
  uartInterfaceWb,
  whoAmIC,
 )

import Clash.Annotations.TH (makeTopEntity)
import Clash.Class.BitPackC (ByteOrder)
import Clash.Cores.Xilinx.DcFifo (dcFifoDf)
import Clash.Cores.Xilinx.Ila (Depth (..), IlaConfig (..), ila, ilaConfig)
import Clash.Cores.Xilinx.Unisim.DnaPortE2 (simDna2)
import Clash.Cores.Xilinx.VIO (vioProbe)
import Clash.Cores.Xilinx.Xpm.Cdc (xpmCdcArraySingle, xpmCdcSingle)
import Clash.Xilinx.ClockGen (clockWizardDifferential)
import Data.Char (ord)
import Protocols
import Protocols.Extra
import Protocols.Idle
import Protocols.MemoryMap (ConstBwd, MM, MemoryMap)
import Protocols.Wishbone
import System.FilePath ((</>))
import VexRiscv (DumpVcd (..), Jtag, JtagIn (..), JtagOut (..))

import qualified Bittide.Transceiver as Transceiver
import qualified Clash.Cores.Xilinx.GTH as Gth
import qualified Protocols.MemoryMap as MM
import qualified Protocols.Vec as Vec

#ifdef SIM_BAUD_RATE
type Baud = MaxBaudRate Basic125
#else
type Baud = 921_600
#endif

baud :: SNat Baud
baud = SNat

type FifoSize = 5 -- = 2^5 = 32

{- Internal busses:
    - Instruction memory
    - Data memory
    - `timeWb`
-}

memoryMapCc, memoryMapMu :: MemoryMap
(memoryMapCc, memoryMapMu, _) = (ccMm, muMm, gppeMm)
 where
  (SimOnly ccMm, SimOnly muMm, SimOnly gppeMm, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =
    softUgnDemoDut
      clockGen
      resetGen
      clockGen
      (SimOnly (repeat 0))
      0
      0
      0
      (pure (JtagIn 0 0 0))
      0

ccConfig ::
  ( KnownNat n
  , PrefixWidth (n + SwcccInternalBusses) <= 30
  ) =>
  PeConfig (n + SwcccInternalBusses)
ccConfig =
  PeConfig
    { initI = Undefined @(Div (64 * 1024) 4)
    , initD = Undefined @(Div (64 * 1024) 4)
    , iBusTimeout = d0
    , dBusTimeout = d0
    , includeIlaWb = False
    }

ccLabel :: Vec 2 Byte
ccLabel = fromIntegral (ord 'C') :> fromIntegral (ord 'C') :> Nil

muLabel :: Vec 2 Byte
muLabel = fromIntegral (ord 'M') :> fromIntegral (ord 'U') :> Nil

gppeLabel :: Vec 2 Byte
gppeLabel = fromIntegral (ord 'P') :> fromIntegral (ord 'E') :> Nil

type NmuInternalBusses = 4 + PeInternalBusses
type NmuExternalBusses = LinkCount * PeripheralsPerLink
type PeripheralsPerLink = 3 -- Scatter calendar, Gather calendar, UGN component.
type NmuRemBusWidth = 30 - CLog 2 (NmuExternalBusses + NmuInternalBusses)

managementUnit ::
  forall dom.
  ( HiddenClockResetEnable dom
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  , 1 <= DomainPeriod dom
  ) =>
  Circuit
    (MM.ConstBwd MM.MM, Jtag dom)
    ( CSignal dom (Unsigned 64)
    , Vec
        NmuExternalBusses
        ( MM.ConstBwd MM.MM
        , Wishbone dom 'Standard NmuRemBusWidth (Bytes 4)
        )
    , Df dom (BitVector 8)
    )
managementUnit =
  circuit $ \(mm, jtag) -> do
    -- Core and interconnect
    wbs0 <- processingElement NoDumpVcd peConfig -< (mm, jtag)
    ([wbTime, uartWb, dnaWb, whoAmIWb], wbs1) <- splitAtCI -< wbs0

    -- Peripherals
    cnt <- timeWb -< wbTime
    (uartOut, _uartStatus) <-
      uartInterfaceWb d16 d16 uartBytes -< (uartWb, Fwd (pure Nothing))
    _dna <- readDnaPortE2Wb simDna2 -< dnaWb
    whoAmIC 0x746d_676d -< whoAmIWb

    -- Output
    idC -< (cnt, wbs1, uartOut)
 where
  peConfig =
    PeConfig
      { initI = Undefined @(Div (64 * 1024) 4)
      , initD = Undefined @(Div (64 * 1024) 4)
      , iBusTimeout = d0
      , dBusTimeout = d0
      , includeIlaWb = False
      }

gppe ::
  (HiddenClockResetEnable dom, 1 <= DomainPeriod dom) =>
  Vec LinkCount (Signal dom (BitVector 64)) ->
  Circuit
    ( ConstBwd MM
    , Vec (2 * LinkCount) (ConstBwd MM, Wishbone dom 'Standard NmuRemBusWidth (Bytes 4))
    , Jtag dom
    )
    ( Vec LinkCount (CSignal dom (BitVector 64))
    , Df dom (BitVector 8)
    )
gppe linksIn = withBittideByteOrder $ circuit $ \(mm, nmuWbs, jtag) -> do
  -- Core and interconnect
  (wbScats, wbs0) <- splitAtCI <| processingElement NoDumpVcd peConfig -< (mm, jtag)
  (wbGus, wbs1) <- splitAtCI -< wbs0
  [wbTime, uartWb] <- idC -< wbs1

  -- Scatter Gather units
  (wbScatCals, wbGathCal) <- splitAtC SNat -< nmuWbs
  idleSink
    <| Vec.vecCircuits (fmap (scatterUnitWbC scatterConfig) linksIn)
    <| zipC
    -< (wbScats, wbScatCals)
  linksOut <- mapCircuit (gatherUnitWbC gatherConfig) <| zipC -< (wbGus, wbGathCal)

  -- Peripherals
  _cnt <- timeWb -< wbTime
  (uart, _uartStatus) <- uartInterfaceWb d16 d16 uartBytes -< (uartWb, Fwd (pure Nothing))

  -- Output
  idC -< (linksOut, uart)
 where
  peConfig =
    PeConfig
      { initI = Undefined @(Div (64 * 1024) 4)
      , initD = Undefined @(Div (64 * 1024) 4)
      , iBusTimeout = d0
      , dBusTimeout = d0
      , includeIlaWb = False
      }
  scatterConfig = ScatterConfig (SNat @1024) (CalendarConfig maxCalDepth repetitionBits sgCal sgCal)
  gatherConfig = GatherConfig (SNat @1024) (CalendarConfig maxCalDepth repetitionBits sgCal sgCal)
  maxCalDepth = d1024
  repetitionBits = d12
  sgCal = ValidEntry 0 1000 :> Nil

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
softUgnDemoDut ::
  "REFCLK" ::: Clock Basic125 ->
  "TEST_RST" ::: Reset Basic125 ->
  "SKYCLK" ::: Clock Ext200 ->
  "GTH_RX_S" ::: Gth.SimWires GthRx LinkCount ->
  "GTH_RX_NS" ::: Gth.Wires GthRxS LinkCount ->
  "GTH_RX_PS" ::: Gth.Wires GthRxS LinkCount ->
  "MISO" ::: Signal Basic125 Bit ->
  "JTAG_IN" ::: Signal Bittide JtagIn ->
  "SYNC_IN" ::: Signal Bittide Bit ->
  ( "CC_MEMORYMAP" ::: MM.MM
  , "MU_MEMORYMAP" ::: MM.MM
  , "GPPE_MEMORYMAP" ::: MM.MM
  , "GTH_TX_S" ::: Gth.SimWires Bittide LinkCount
  , "GTH_TX_NS" ::: Gth.Wires GthTxS LinkCount
  , "GTH_TX_PS" ::: Gth.Wires GthTxS LinkCount
  , "handshakesDone" ::: Signal Basic125 Bool
  , "FINC_FDEC" ::: Signal Bittide (FINC, FDEC)
  , "spiDone" ::: Signal Basic125 Bool
  , ""
      ::: ( "SCLK" ::: Signal Basic125 Bool
          , "MOSI" ::: Signal Basic125 Bit
          , "CSB" ::: Signal Basic125 Bool
          )
  , "JTAG_OUT" ::: Signal Bittide JtagOut
  , "transceiversFailedAfterUp" ::: Signal Basic125 Bool
  , "ALL_STABLE" ::: Signal Basic125 Bool
  , "fifoOverflowsSticky" ::: Signal Basic125 Bool
  , "fifoUnderflowsSticky" ::: Signal Basic125 Bool
  , "UART_TX" ::: Signal Basic125 Bit
  , "SYNC_OUT" ::: Signal Basic125 Bit
  )
softUgnDemoDut refClk refRst skyClk rxSims rxNs rxPs miso jtagIn syncIn =
  -- Replace 'seqX' with 'hwSeqX' to include ILAs in hardware
  seqX
    (bundle (debugIla, bittidePeIla))
    ( ccMm
    , muMm
    , gppeMm
    , transceivers.txSims
    , transceivers.txNs
    , transceivers.txPs
    , handshakesDoneFree
    , frequencyAdjustments
    , spiDone
    , spiOut
    , jtagOut
    , transceiversFailedAfterUp
    , allStableFree
    , fifoOverflowsStickyFree
    , fifoUnderflowsStickyFree
    , uartTx
    , syncOut
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
      (xpmCdcSingle bittideClk refClk allStable)
      (bundle $ xpmCdcArraySingle bittideClk refClk <$> ebReadys)
      transceiversFailedAfterUp

  captureFlag =
    riseEvery
      refClk
      spiRst
      enableGen
      (SNat @(PeriodToCycles Basic125 (Milliseconds 2)))

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

  transceivers :: Transceiver.Outputs LinkCount Bittide GthRx GthTxS Basic125
  transceivers =
    transceiverPrbsN
      @Bittide
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

  bittideClk :: Clock Bittide
  bittideClk = transceivers.txClock

  bittideRst :: Reset Bittide
  bittideRst = transceivers.txReset

  linksSuitableForCc :: Signal Bittide (BitVector LinkCount)
  linksSuitableForCc = fmap pack (bundle transceivers.handshakesDoneTx)

  handshakesDoneFree :: Signal Basic125 Bool
  handshakesDoneFree = and <$> bundle transceivers.handshakesDoneFree

  handshakesDoneTx :: Signal Bittide Bool
  handshakesDoneTx = and <$> bundle transceivers.handshakesDoneTx

  -- Step 3, send local counter for one cycle, connect to switch after:
  txSamplingsDelayed :: Vec 7 (Signal Bittide Bool)
  txSamplingsDelayed =
    register bittideClk handshakeRstTx enableGen False <$> transceivers.txSamplings

  txDatas :: Vec 7 (Signal Bittide (BitVector 64))
  txDatas = mux <$> txSamplingsDelayed <*> switchDataOut <*> repeat (pack <$> localCounter)

  txStarts :: Vec 7 (Signal Bittide Bool)
  txStarts = repeat allStableSticky

  -- Step 4, deassert CC CPU reset, deassert Bittide domain reset:
  handshakeRstFree :: Reset Basic125
  handshakeRstFree = unsafeFromActiveLow handshakesDoneFree

  handshakeRstTx :: Reset Bittide
  handshakeRstTx = unsafeFromActiveLow handshakesDoneTx

  -- Step 5, wait for stable buffers:
  allStable :: Signal Bittide Bool
  allStable = callistoResult.allStable

  allStableSticky :: Signal Bittide Bool
  allStableSticky = sticky bittideClk bittideRst allStable

  allStableFree :: Signal Basic125 Bool
  allStableFree = xpmCdcSingle bittideClk refClk allStableSticky

  -- Step 6, wait for elastic buffer initialization
  --         (=> signal we're ready to receive data):
  ebReset :: Reset Bittide
  ebReset = unsafeFromActiveLow allStableSticky

  ebReadys :: Vec 7 (Signal Bittide Bool)
  ebReadys = map (.==. pure Pass) ebModes

  ebReadysRx :: Vec 7 (Signal GthRx Bool)
  ebReadysRx = xpmCdcArraySingle bittideClk <$> transceivers.rxClocks <*> ebReadys

  -- Connect everything together:
  transceiversFailedAfterUp :: Signal Basic125 Bool
  transceiversFailedAfterUp =
    sticky refClk refRst (isFalling refClk spiRst enableGen False handshakesDoneFree)

  defaultBittideClkRstEn :: forall r. ((HiddenClockResetEnable Bittide) => r) -> r
  defaultBittideClkRstEn = withClockResetEnable bittideClk handshakeRstTx enableGen
  defaultRefClkRstEn :: forall r. ((HiddenClockResetEnable Basic125) => r) -> r
  defaultRefClkRstEn = withClockResetEnable refClk handshakeRstFree enableGen

  circuitFnC ::
    ( ?busByteOrder :: ByteOrder
    , ?regByteOrder :: ByteOrder
    ) =>
    Circuit
      ( "CC" ::: ConstBwd MM
      , "MU" ::: ConstBwd MM
      , "GPPE" ::: ConstBwd MM
      , Jtag Bittide
      , CSignal Bittide (BitVector LinkCount)
      , "RXS" ::: Vec LinkCount (CSignal Bittide (Maybe (BitVector 64)))
      )
      ( CSignal Bittide (CallistoResult LinkCount)
      , "TXS" ::: Vec LinkCount (CSignal Bittide (BitVector 64))
      , "LOCAL_COUNTER" ::: CSignal Bittide (Unsigned 64)
      , "UART_TX" ::: CSignal Basic125 Bit
      , "SYNC_OUT" ::: CSignal Basic125 Bit
      )
  circuitFnC = circuit $ \(ccMM, muMM, gppeMm, jtag, mask, Fwd rxs0) -> do
    [muJtag, ccJtag, gppeJtag] <- jtagChain -< jtag

    -- Start management unit
    (Fwd lc, muWbAll, muUartBytesBittide) <-
      defaultBittideClkRstEn managementUnit -< (muMM, muJtag)
    (ugnWbs, muSgWbs) <- splitAtC SNat -< muWbAll
    -- Stop management unit

    -- Start internal links
    Fwd rxs1 <- defaultBittideClkRstEn $ Vec.vecCircuits (captureUgn lc <$> rxs0) -< ugnWbs
    (txs, gppeUartBytesBittide) <-
      defaultBittideClkRstEn gppe rxs1 -< (gppeMm, muSgWbs, gppeJtag)
    -- Stop internal links

    -- Start UART multiplexing
    uartTxBytes <-
      defaultRefClkRstEn
        $ asciiDebugMux d1024 (ccLabel :> muLabel :> gppeLabel :> Nil)
        -< [ccUartBytes, muUartBytes, gppeUartBytes]
    (_uartInBytes, uartTx) <- defaultRefClkRstEn $ uartDf baud -< (uartTxBytes, Fwd 0)

    muUartBytes <-
      dcFifoDf d5 bittideClk handshakeRstTx refClk handshakeRstFree -< muUartBytesBittide
    gppeUartBytes <-
      dcFifoDf d5 bittideClk handshakeRstTx refClk handshakeRstFree -< gppeUartBytesBittide
    -- Stop UART multiplexing

    -- Start Clock control
    ( syncOut
      , Fwd swCcOut0
      , [ ccWhoAmIBus
          , ccUartBus
          , ccSampleMemoryBus
          ]
      ) <-
      defaultBittideClkRstEn
        $ callistoSwClockControlC
          @LinkCount
          refClk
          refRst
          transceivers.rxClocks
          (unsafeFromActiveLow <$> transceivers.handshakesDone)
          NoDumpVcd
          ccConfig
        -< (ccMM, (Fwd syncIn, ccJtag, mask, Fwd linksSuitableForCc))

    defaultBittideClkRstEn
      (wbStorage "SampleMemory")
      (Undefined @36_000 @(BitVector 32))
      -< ccSampleMemoryBus

    defaultBittideClkRstEn (whoAmIC 0x6363_7773) -< ccWhoAmIBus

    (ccUartBytesBittide, _uartStatus) <-
      defaultBittideClkRstEn
        $ uartInterfaceWb d16 d16 uartBytes
        -< (ccUartBus, Fwd (pure Nothing))

    ccUartBytes <-
      dcFifoDf d5 bittideClk handshakeRstTx refClk handshakeRstFree
        -< ccUartBytesBittide
    -- Stop Clock control

    let swCcOut1 =
          if clashSimulation
            then
              let
                -- Should all clock control steps be run in simulation?
                -- False means that clock control will always immediately be done.
                simulateCc = False
               in
                if simulateCc
                  then swCcOut0
                  else
                    pure
                      $ CallistoResult
                        { maybeSpeedChange = Nothing
                        , stability = repeat (Stability{stable = True, settled = True})
                        , allStable = True
                        , allSettled = True
                        }
            else swCcOut0

    idC
      -< ( Fwd swCcOut1
         , txs
         , Fwd lc
         , uartTx
         , syncOut
         )

  ( (ccMm, muMm, gppeMm, jtagOut, _maskBwd, _insBwd)
    , ( callistoResult
        , switchDataOut
        , localCounter
        , uartTx
        , syncOut
        )
    ) =
      withBittideByteOrder
        $ toSignals
          circuitFnC
          (
            ( ()
            , ()
            , ()
            , jtagIn
            , pure maxBound -- enable mask
            , rxDatasEbs
            )
          ,
            ( pure ()
            , repeat (pure ())
            , pure ()
            , pure ()
            , pure ()
            )
          )

  bittidePeIla :: Signal Basic125 ()
  bittidePeIla =
    setName @"bittidePeIla"
      ila
      ( ilaConfig
          $ "trigger_fdi_pe"
          :> "capture_fdi_pe"
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

  frequencyAdjustments :: Signal Bittide (FINC, FDEC)
  frequencyAdjustments =
    delay bittideClk enableGen minBound
      $ speedChangeToStickyPins
        bittideClk
        bittideRst
        enableGen
        (SNat @Si539xHoldTime)
        callistoResult.maybeSpeedChange

  rxFifos ::
    Vec
      LinkCount
      ( Signal Bittide (RelDataCount FifoSize)
      , Signal Bittide Underflow
      , Signal Bittide Overflow
      , Signal Bittide EbMode
      , Signal Bittide (Maybe (BitVector 64))
      )
  rxFifos = zipWith go transceivers.rxClocks transceivers.rxDatas
   where
    go rxClk rxData = resettableXilinxElasticBuffer bittideClk rxClk ebReset rxData

  fifoUnderflowsTx :: Vec LinkCount (Signal Bittide Underflow)
  fifoOverflowsTx :: Vec LinkCount (Signal Bittide Overflow)
  rxDatasEbs :: Vec LinkCount (Signal Bittide (Maybe (BitVector 64)))
  ebModes :: Vec 7 (Signal Bittide EbMode)
  (_, fifoUnderflowsTx, fifoOverflowsTx, ebModes, rxDatasEbs) = unzip5 rxFifos

  fifoOverflows :: Signal Bittide Overflow
  fifoOverflows = or <$> bundle fifoOverflowsTx

  fifoUnderflows :: Signal Bittide Underflow
  fifoUnderflows = or <$> bundle fifoUnderflowsTx

  fifoOverflowsStickyFree :: Signal Basic125 Bool
  fifoOverflowsStickyFree =
    xpmCdcSingle bittideClk refClk $ sticky bittideClk bittideRst fifoOverflows

  fifoUnderflowsStickyFree :: Signal Basic125 Bool
  fifoUnderflowsStickyFree =
    xpmCdcSingle bittideClk refClk $ sticky bittideClk bittideRst fifoUnderflows

softUgnDemoTest ::
  "SMA_MGT_REFCLK_C" ::: DiffClock Ext200 ->
  "SYSCLK_125" ::: DiffClock Ext125 ->
  "GTH_RX_S" ::: Gth.SimWires GthRx LinkCount ->
  "GTH_RX_NS" ::: Gth.Wires GthRxS LinkCount ->
  "GTH_RX_PS" ::: Gth.Wires GthRxS LinkCount ->
  "MISO" ::: Signal Basic125 Bit ->
  "JTAG" ::: Signal Bittide JtagIn ->
  "USB_UART_TXD" ::: Signal Basic125 Bit ->
  "SYNC_IN" ::: Signal Bittide Bit ->
  ( "GTH_TX_S" ::: Gth.SimWires Bittide LinkCount
  , "GTH_TX_NS" ::: Gth.Wires GthTxS LinkCount
  , "GTH_TX_PS" ::: Gth.Wires GthTxS LinkCount
  , ""
      ::: ( "FINC" ::: Signal Bittide Bool
          , "FDEC" ::: Signal Bittide Bool
          )
  , "spiDone" ::: Signal Basic125 Bool
  , ""
      ::: ( "SCLK" ::: Signal Basic125 Bool
          , "MOSI" ::: Signal Basic125 Bit
          , "CSB" ::: Signal Basic125 Bool
          )
  , "JTAG" ::: Signal Bittide JtagOut
  , "USB_UART_RXD" ::: Signal Basic125 Bit
  , "SYNC_OUT" ::: Signal Basic125 Bit
  )
softUgnDemoTest boardClkDiff refClkDiff rxs rxns rxps miso jtagIn _uartRx syncIn =
  -- Replace 'seqX' with 'hwSeqX' to include ILAs in hardware
  seqX
    testIla
    ( txs
    , txns
    , txps
    , unbundle swFincFdecs
    , spiDone
    , spiOut
    , jtagOut
    , uartTx
    , syncOut
    )
 where
  boardClk :: Clock Ext200
  boardClk = Gth.ibufds_gte3 boardClkDiff

  refClk :: Clock Basic125
  refRst :: Reset Basic125
  (refClk, refRst) = clockWizardDifferential refClkDiff noReset

  testStart :: Signal Basic125 Bool
  testStart =
    unbundle
      $ setName @"vioHitlt"
      $ vioProbe
        ("probe_test_done" :> "probe_test_success" :> "probe_handshakes_done" :> Nil)
        ("probe_test_start" :> Nil)
        False
        refClk
        (testStart .&&. testDone) -- done
        (testStart .&&. testSuccess) -- success
        handshakesDone
  testReset :: Reset Basic125
  testReset = unsafeFromActiveLow testStart `orReset` refRst

  ( _ccMm
    , _muMm
    , _gppeMm
    , txs :: Gth.SimWires Bittide LinkCount
    , txns :: Gth.Wires GthTxS LinkCount
    , txps :: Gth.Wires GthTxS LinkCount
    , handshakesDone :: Signal Basic125 Bool
    , swFincFdecs :: Signal Bittide (Bool, Bool)
    , spiDone :: Signal Basic125 Bool
    , spiOut :: (Signal Basic125 Bool, Signal Basic125 Bit, Signal Basic125 Bool)
    , jtagOut :: Signal Bittide JtagOut
    , transceiversFailedAfterUp :: Signal Basic125 Bool
    , allStable :: Signal Basic125 Bool
    , fifoOverflows :: Signal Basic125 Bool
    , fifoUnderflows :: Signal Basic125 Bool
    , uartTx :: Signal Basic125 Bit
    , syncOut :: Signal Basic125 Bit
    ) = softUgnDemoDut refClk testReset boardClk rxs rxns rxps miso jtagIn syncIn

  fifoSuccess :: Signal Basic125 Bool
  fifoSuccess = not <$> (fifoUnderflows .||. fifoOverflows)

  doneSuccess :: Signal Basic125 Bool
  doneSuccess = allStable .&&. fifoSuccess

  testDone :: Signal Basic125 Bool
  testDone = doneSuccess .||. transceiversFailedAfterUp .||. fmap not fifoSuccess

  testSuccess :: Signal Basic125 Bool
  testSuccess = testDone .&&. fifoSuccess .&&. fmap not transceiversFailedAfterUp

  testIla :: Signal Basic125 ()
  testIla =
    setName @"demoTestIla"
      ila
      ( ilaConfig
          $ "trigger_fdi_dt"
          :> "capture_fdi_dt"
          :> "dt_handshakesDone"
          :> "dt_spiDone"
          :> "dt_spiOut"
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
      spiDone
      (bundle spiOut)
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
{-# OPAQUE softUgnDemoTest #-}
makeTopEntity 'softUgnDemoTest

tests :: HitlTestGroup
tests =
  HitlTestGroup
    { topEntity = 'softUgnDemoTest
    , extraXdcFiles =
        [ "jtag" </> "config.xdc"
        , "jtag" </> "pmod1.xdc"
        , "uart" </> "pmod1.xdc"
        ]
    , externalHdl = []
    , testCases =
        [ HitlTestCase
            { name = "Bittide_Demo_DUT"
            , parameters = paramForHwTargets allHwTargets ()
            , postProcData = ()
            }
        ]
    , mDriverProc = Nothing
    , mPostProc = Nothing
    }
