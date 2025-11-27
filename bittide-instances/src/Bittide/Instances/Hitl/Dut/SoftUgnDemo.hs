-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-}

{- | Switch demo for a Bittide system. In concert with its driver file, this device under
test should demonstrate the predictability of a Bittide system once it has achieved logical
synchronicity.

For more details, see [QBayLogic's presentation](https://docs.google.com/presentation/d/1AGbAJQ1zhTPtrekKnQcthd0TUPyQs-zowQpV1ux4k-Y)
on the topic.
-}
module Bittide.Instances.Hitl.Dut.SoftUgnDemo (
  ccWhoAmID,
  gppeWhoAmID,
  memoryMapCc,
  memoryMapMu,
  memoryMapGppe,
  muWhoAmID,
  softUgnDemoC,
) where

import Clash.Explicit.Prelude
import Clash.Prelude (HiddenClockResetEnable, withClockResetEnable)

import Bittide.Calendar (CalendarConfig (..), ValidEntry (..))
import Bittide.CaptureUgn (captureUgn)
import Bittide.ClockControl.Callisto.Types (
  CallistoResult (..),
  Stability (..),
 )
import Bittide.ClockControl.CallistoSw (
  SwcccInternalBusses,
  callistoSwClockControlC,
 )
import Bittide.Df (asciiDebugMux)
import Bittide.DoubleBufferedRam
import Bittide.ElasticBuffer (xilinxElasticBufferWb)
import Bittide.Instances.Domains (Basic125, Bittide, GthRx)
import Bittide.Instances.Hitl.Setup (LinkCount)
import Bittide.Jtag (jtagChain)
import Bittide.ProcessingElement (
  PeConfig (..),
  PeInternalBusses,
  PrefixWidth,
  processingElement,
 )
import Bittide.ScatterGather
import Bittide.SharedTypes (Byte, Bytes, withBittideByteOrder)
import Bittide.Wishbone (
  makeWhoAmIdTh,
  readDnaPortE2WbWorker,
  timeWb,
  uartBytes,
  uartDf,
  uartInterfaceWb,
  whoAmIC,
 )

import Clash.Class.BitPackC (ByteOrder)
import Clash.Cores.Xilinx.DcFifo (dcFifoDf)
import Clash.Cores.Xilinx.Unisim.DnaPortE2 (readDnaPortE2, simDna2)
import Data.Char (ord)
import Protocols
import Protocols.Idle
import Protocols.MemoryMap (ConstBwd, MM, MemoryMap)
import Protocols.Wishbone
import Protocols.Wishbone.Extra
import VexRiscv (DumpVcd (..), Jtag, JtagIn (..))

import qualified Bittide.Cpus.Riscv32imc as Riscv32imc
import qualified Protocols.MemoryMap as MM
import qualified Protocols.Vec as Vec

#ifdef SIM_BAUD_RATE
type Baud = MaxBaudRate Basic125
#else
type Baud = 921_600
#endif

baud :: SNat Baud
baud = SNat

ccWhoAmID :: BitVector 32
ccWhoAmID = $(makeWhoAmIdTh "swcc")
muWhoAmID :: BitVector 32
muWhoAmID = $(makeWhoAmIdTh "mgmt")
gppeWhoAmID :: BitVector 32
gppeWhoAmID = $(makeWhoAmIdTh "gppe")

{- Internal busses:
    - Instruction memory
    - Data memory
    - `timeWb`
-}

memoryMapCc, memoryMapMu, memoryMapGppe :: MemoryMap
(memoryMapCc, memoryMapMu, memoryMapGppe) = (ccMm, muMm, gppeMm)
 where
  Circuit circuitFn =
    withBittideByteOrder
      $ softUgnDemoC
        (clockGen, resetGen, enableGen)
        (clockGen, resetGen, enableGen)
        (repeat clockGen)
        (repeat resetGen)
  ((SimOnly ccMm, SimOnly muMm, SimOnly gppeMm, _, _, _, _, _), _) =
    circuitFn
      (
        ( ()
        , ()
        , ()
        , pure (JtagIn 0 0 0)
        , pure maxBound
        , pure maxBound
        , repeat (pure Nothing)
        , pure low
        )
      ,
        ( ()
        , repeat ()
        , ()
        , ()
        , ()
        , repeat ()
        )
      )

ccConfig ::
  ( KnownNat n
  , PrefixWidth (n + SwcccInternalBusses) <= 30
  ) =>
  PeConfig (n + SwcccInternalBusses)
ccConfig =
  PeConfig
    { cpu = Riscv32imc.vexRiscv1
    , initI = Undefined @(Div (64 * 1024) 4)
    , initD = Undefined @(Div (64 * 1024) 4)
    , iBusTimeout = d0
    , dBusTimeout = d0
    , includeIlaWb = False
    }

ccLabel, muLabel, gppeLabel :: Vec 2 Byte
ccLabel = fromIntegral (ord 'C') :> fromIntegral (ord 'C') :> Nil
muLabel = fromIntegral (ord 'M') :> fromIntegral (ord 'U') :> Nil
gppeLabel = fromIntegral (ord 'P') :> fromIntegral (ord 'E') :> Nil

type NmuInternalBusses = 4 + PeInternalBusses
type NmuExternalBusses = (LinkCount * PeripheralsPerLink) + LinkCount
type PeripheralsPerLink = 3 -- Scatter calendar, Gather calendar, UGN component.
type NmuRemBusWidth = 30 - CLog 2 (NmuExternalBusses + NmuInternalBusses)

type FifoSize = 5 -- = 2^5 = 32

managementUnit ::
  forall dom.
  ( HiddenClockResetEnable dom
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  , 1 <= DomainPeriod dom
  ) =>
  -- | DNA value
  Signal dom (Maybe (BitVector 96)) ->
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
managementUnit maybeDna =
  circuit $ \(mm, jtag) -> do
    -- Core and interconnect
    wbs0 <- processingElement NoDumpVcd peConfig -< (mm, jtag)
    ([wbTime, uartWb, dnaWb, whoAmIWb], wbs1) <- Vec.split -< wbs0

    -- Peripherals
    cnt <- timeWb -< wbTime
    (uartOut, _uartStatus) <-
      uartInterfaceWb d16 d16 uartBytes -< (uartWb, Fwd (pure Nothing))
    readDnaPortE2WbWorker maybeDna -< dnaWb
    whoAmIC muWhoAmID -< whoAmIWb

    -- Output
    idC -< (cnt, wbs1, uartOut)
 where
  peConfig =
    PeConfig
      { cpu = Riscv32imc.vexRiscv0
      , initI = Undefined @(Div (64 * 1024) 4)
      , initD = Undefined @(Div (64 * 1024) 4)
      , iBusTimeout = d0
      , dBusTimeout = d0
      , includeIlaWb = False
      }

gppe ::
  (HiddenClockResetEnable dom, 1 <= DomainPeriod dom) =>
  -- | DNA value
  Signal dom (Maybe (BitVector 96)) ->
  Vec LinkCount (Signal dom (BitVector 64)) ->
  Circuit
    ( ConstBwd MM
    , Vec (2 * LinkCount) (ConstBwd MM, Wishbone dom 'Standard NmuRemBusWidth (Bytes 4))
    , Jtag dom
    )
    ( Vec LinkCount (CSignal dom (BitVector 64))
    , Df dom (BitVector 8)
    )
gppe maybeDna linksIn = withBittideByteOrder $ circuit $ \(mm, nmuWbMms, jtag) -> do
  -- Core and interconnect
  (wbScats, wbs0) <- Vec.split <| processingElement NoDumpVcd peConfig -< (mm, jtag)
  (wbGus, wbs1) <- Vec.split -< wbs0
  [wbTime, uartWb, whoAmIWB, dnaWb] <- idC -< wbs1

  -- Synthesis fails on timing check unless these signals are registered. Remove as soon
  -- as possible.
  (nmuMms, nmuWbs) <- Vec.unzip -< nmuWbMms
  nmuWbsDelayed <- repeatC delayWishboneC -< nmuWbs
  nmuWbMmsDelayed <- Vec.zip -< (nmuMms, nmuWbsDelayed)

  -- Scatter Gather units
  (wbScatCals, wbGathCal) <- Vec.split -< nmuWbMmsDelayed
  idleSink
    <| Vec.vecCircuits (fmap (scatterUnitWbC scatterConfig) linksIn)
    <| Vec.zip
    -< (wbScats, wbScatCals)
  linksOut <- repeatC (gatherUnitWbC gatherConfig) <| Vec.zip -< (wbGus, wbGathCal)

  -- Peripherals
  _cnt <- timeWb -< wbTime
  (uart, _uartStatus) <- uartInterfaceWb d16 d16 uartBytes -< (uartWb, Fwd (pure Nothing))
  whoAmIC gppeWhoAmID -< whoAmIWB
  readDnaPortE2WbWorker maybeDna -< dnaWb

  -- Output
  idC -< (linksOut, uart)
 where
  peConfig =
    PeConfig
      { cpu = Riscv32imc.vexRiscv2
      , initI = Undefined @(Div (64 * 1024) 4)
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

softUgnDemoC ::
  ( ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
  (Clock Basic125, Reset Basic125, Enable Basic125) ->
  (Clock Bittide, Reset Bittide, Enable Bittide) ->
  Vec LinkCount (Clock GthRx) ->
  Vec LinkCount (Reset GthRx) ->
  Circuit
    ( "CC" ::: ConstBwd MM
    , "MU" ::: ConstBwd MM
    , "GPPE" ::: ConstBwd MM
    , Jtag Bittide
    , "MASK" ::: CSignal Bittide (BitVector LinkCount)
    , "CC_SUITABLE" ::: CSignal Bittide (BitVector LinkCount)
    , "RXS" ::: Vec LinkCount (CSignal GthRx (Maybe (BitVector 64)))
    , "SYNC_IN" ::: CSignal Bittide Bit
    )
    ( CSignal Bittide (CallistoResult LinkCount)
    , "TXS" ::: Vec LinkCount (CSignal Bittide (BitVector 64))
    , "LOCAL_COUNTER" ::: CSignal Bittide (Unsigned 64)
    , "UART_TX" ::: CSignal Basic125 Bit
    , "SYNC_OUT" ::: CSignal Basic125 Bit
    , "EB_STABLES" ::: Vec LinkCount (CSignal Bittide Bool)
    )
softUgnDemoC (refClk, refRst, refEna) (bitClk, bitRst, bitEna) rxClocks rxResets =
  circuit $ \(ccMM, muMM, gppeMm, jtag, mask, linksSuitableForCc, Fwd rxs0, syncIn) -> do
    [muJtag, ccJtag, gppeJtag] <- jtagChain -< jtag

    let maybeDna = readDnaPortE2 bitClk bitRst bitEna simDna2

    -- Start management unit
    (Fwd lc, muWbAll, muUartBytesBittide) <-
      defaultBittideClkRstEn managementUnit maybeDna -< (muMM, muJtag)
    (ugnWbs, muWbs1) <- Vec.split -< muWbAll
    (ebWbs, muSgWbs) <- Vec.split -< muWbs1
    -- Stop management unit

    -- Start internal links
    (_relDatCount, _underflow, _overflow, ebStables, Fwd rxs1) <-
      unzip5Vec
        <| ( Vec.vecCircuits
              $ xilinxElasticBufferWb
                bitClk
                bitRst
                (SNat @FifoSize)
              <$> rxClocks
              <*> rxs0
           )
        -< ebWbs

    Fwd rxs2 <- defaultBittideClkRstEn $ Vec.vecCircuits (captureUgn lc <$> rxs1) -< ugnWbs
    (txs, gppeUartBytesBittide) <-
      defaultBittideClkRstEn gppe maybeDna rxs2 -< (gppeMm, muSgWbs, gppeJtag)
    -- Stop internal links

    -- Start UART multiplexing
    uartTxBytes <-
      defaultRefClkRstEn
        $ asciiDebugMux d1024 (ccLabel :> muLabel :> gppeLabel :> Nil)
        -< [ccUartBytes, muUartBytes, gppeUartBytes]
    (_uartInBytes, uartTx) <- defaultRefClkRstEn $ uartDf baud -< (uartTxBytes, Fwd 0)

    muUartBytes <-
      dcFifoDf d5 bitClk bitRst refClk refRst -< muUartBytesBittide
    gppeUartBytes <-
      dcFifoDf d5 bitClk bitRst refClk refRst -< gppeUartBytesBittide
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
          rxClocks
          rxResets
          NoDumpVcd
          ccConfig
        -< (ccMM, (syncIn, ccJtag, mask, linksSuitableForCc))

    defaultBittideClkRstEn
      (wbStorage "SampleMemory")
      (Undefined @36_000 @(BitVector 32))
      -< ccSampleMemoryBus

    defaultBittideClkRstEn (whoAmIC ccWhoAmID) -< ccWhoAmIBus

    (ccUartBytesBittide, _uartStatus) <-
      defaultBittideClkRstEn
        $ uartInterfaceWb d16 d16 uartBytes
        -< (ccUartBus, Fwd (pure Nothing))

    ccUartBytes <-
      dcFifoDf d5 bitClk bitRst refClk refRst
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
         , ebStables
         )
 where
  defaultBittideClkRstEn :: forall r. ((HiddenClockResetEnable Bittide) => r) -> r
  defaultBittideClkRstEn = withClockResetEnable bitClk bitRst bitEna
  defaultRefClkRstEn :: forall r. ((HiddenClockResetEnable Basic125) => r) -> r
  defaultRefClkRstEn = withClockResetEnable refClk refRst refEna

uncurry5 ::
  (a -> b -> c -> d -> e -> f) ->
  (a, b, c, d, e) ->
  f
uncurry5 fn (a, b, c, d, e) = fn a b c d e

unzip5Vec ::
  Circuit (Vec n (a, b, c, d, e)) (Vec n a, Vec n b, Vec n c, Vec n d, Vec n e)
unzip5Vec = applyC unzip5 (uncurry5 zip5)
