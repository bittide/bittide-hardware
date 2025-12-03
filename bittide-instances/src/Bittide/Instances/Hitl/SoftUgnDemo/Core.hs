-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Bittide.Instances.Hitl.SoftUgnDemo.Core (core) where

import Clash.Explicit.Prelude
import Clash.Prelude (HiddenClockResetEnable, withClockResetEnable)
import Protocols

import Bittide.Calendar (CalendarConfig (..), ValidEntry (..))
import Bittide.CaptureUgn (captureUgn)
import Bittide.ClockControl.Callisto.Types (CallistoResult (..), Stability (..))
import Bittide.ClockControl.CallistoSw (SwcccInternalBusses, callistoSwClockControlC)
import Bittide.DoubleBufferedRam (InitialContent (Undefined), wbStorage)
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
import Bittide.SharedTypes (Bytes, withBittideByteOrder)
import Bittide.Sync (Sync)
import Bittide.Wishbone (readDnaPortE2WbWorker, timeWb, uartBytes, uartInterfaceWb)
import Clash.Class.BitPackC (ByteOrder)
import Clash.Cores.Xilinx.Unisim.DnaPortE2 (readDnaPortE2, simDna2)
import Protocols.Idle (idleSink)
import Protocols.MemoryMap (Mm)
import Protocols.Wishbone (Wishbone, WishboneMode (Standard))
import Protocols.Wishbone.Extra (delayWishboneC)
import VexRiscv (DumpVcd (..), Jtag)

import qualified Bittide.Cpus.Riscv32imc as Riscv32imc
import qualified Protocols.MemoryMap as Mm
import qualified Protocols.Vec as Vec

type FifoSize = 5 -- = 2^5 = 32

type NmuInternalBusses = 3 + PeInternalBusses
type NmuExternalBusses = (LinkCount * PeripheralsPerLink) + LinkCount + 1 -- +1 for tranceivers
type PeripheralsPerLink = 3 -- Scatter calendar, Gather calendar, UGN component.
type NmuRemBusWidth = 30 - CLog 2 (NmuExternalBusses + NmuInternalBusses)

muConfig ::
  ( KnownNat n
  , PrefixWidth (n + NmuInternalBusses) <= 30
  ) =>
  PeConfig (n + NmuInternalBusses)
muConfig =
  PeConfig
    { cpu = Riscv32imc.vexRiscv1
    , initI = Undefined @(Div (64 * 1024) 4)
    , initD = Undefined @(Div (64 * 1024) 4)
    , iBusTimeout = d0
    , dBusTimeout = d0
    , includeIlaWb = False
    }

ccConfig ::
  ( KnownNat n
  , PrefixWidth (n + SwcccInternalBusses) <= 30
  ) =>
  PeConfig (n + SwcccInternalBusses)
ccConfig =
  PeConfig
    { cpu = Riscv32imc.vexRiscv2
    , initI = Undefined @(Div (64 * 1024) 4)
    , initD = Undefined @(Div (64 * 1024) 4)
    , iBusTimeout = d0
    , dBusTimeout = d0
    , includeIlaWb = False
    }

gppeConfig ::
  ( KnownNat n
  , 2 <= n
  , PrefixWidth n <= 30
  ) =>
  PeConfig n
gppeConfig =
  PeConfig
    { cpu = Riscv32imc.vexRiscv3
    , initI = Undefined @(Div (64 * 1024) 4)
    , initD = Undefined @(Div (64 * 1024) 4)
    , iBusTimeout = d0
    , dBusTimeout = d0
    , includeIlaWb = False
    }

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
    (ToConstBwd Mm.Mm, Jtag dom)
    ( CSignal dom (Unsigned 64)
    , Vec
        NmuExternalBusses
        ( ToConstBwd Mm.Mm
        , Wishbone dom 'Standard NmuRemBusWidth (Bytes 4)
        )
    , Df dom (BitVector 8)
    )
managementUnit maybeDna =
  circuit $ \(mm, jtag) -> do
    -- Core and interconnect
    wbs0 <- processingElement NoDumpVcd muConfig -< (mm, jtag)
    ([wbTime, uartWb, dnaWb], wbs1) <- Vec.split -< wbs0

    -- Peripherals
    cnt <- timeWb -< wbTime
    (uartOut, _uartStatus) <-
      uartInterfaceWb d16 d16 uartBytes -< (uartWb, Fwd (pure Nothing))
    readDnaPortE2WbWorker maybeDna -< dnaWb

    -- Output
    idC -< (cnt, wbs1, uartOut)

gppe ::
  (HiddenClockResetEnable dom, 1 <= DomainPeriod dom) =>
  -- | DNA value
  Signal dom (Maybe (BitVector 96)) ->
  Vec LinkCount (Signal dom (BitVector 64)) ->
  Circuit
    ( ToConstBwd Mm
    , Vec (2 * LinkCount) (ToConstBwd Mm, Wishbone dom 'Standard NmuRemBusWidth (Bytes 4))
    , Jtag dom
    )
    ( Vec LinkCount (CSignal dom (BitVector 64))
    , Df dom (BitVector 8)
    )
gppe maybeDna linksIn = withBittideByteOrder $ circuit $ \(mm, nmuWbMms, jtag) -> do
  -- Core and interconnect
  (wbScats, wbs0) <- Vec.split <| processingElement NoDumpVcd gppeConfig -< (mm, jtag)
  (wbGus, wbs1) <- Vec.split -< wbs0
  [wbTime, uartWb, dnaWb] <- idC -< wbs1

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
  readDnaPortE2WbWorker maybeDna -< dnaWb

  -- Output
  idC -< (linksOut, uart)
 where
  scatterConfig = ScatterConfig (SNat @1024) (CalendarConfig maxCalDepth repetitionBits sgCal sgCal)
  gatherConfig = GatherConfig (SNat @1024) (CalendarConfig maxCalDepth repetitionBits sgCal sgCal)
  maxCalDepth = d1024
  repetitionBits = d12
  sgCal = ValidEntry 0 1000 :> Nil

core ::
  ( ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
  (Clock Basic125, Reset Basic125) ->
  (Clock Bittide, Reset Bittide, Enable Bittide) ->
  Vec LinkCount (Clock GthRx) ->
  Vec LinkCount (Reset GthRx) ->
  Circuit
    ( "MU" ::: ToConstBwd Mm
    , "CC" ::: ToConstBwd Mm
    , "GPPE" ::: ToConstBwd Mm
    , Jtag Bittide
    , "MASK" ::: CSignal Bittide (BitVector LinkCount)
    , "CC_SUITABLE" ::: CSignal Bittide (BitVector LinkCount)
    , "RXS" ::: Vec LinkCount (CSignal GthRx (Maybe (BitVector 64)))
    )
    ( CSignal Bittide (CallistoResult LinkCount)
    , "LOCAL_COUNTER" ::: CSignal Bittide (Unsigned 64)
    , "TXS" ::: Vec LinkCount (CSignal Bittide (BitVector 64))
    , Sync Bittide Basic125
    , "MU_UART" ::: Df Bittide (BitVector 8)
    , "CC_UART" ::: Df Bittide (BitVector 8)
    , "GPPE_UART" ::: Df Bittide (BitVector 8)
    , "MU_TRANSCEIVER"
        ::: (ToConstBwd Mm.Mm, Wishbone Bittide 'Standard NmuRemBusWidth (Bytes 4))
    )
core (refClk, refRst) (bitClk, bitRst, bitEna) rxClocks rxResets =
  circuit $ \(muMm, ccMm, gppeMm, jtag, mask, linksSuitableForCc, Fwd rxs0) -> do
    [muJtag, ccJtag, gppeJtag] <- jtagChain -< jtag

    let maybeDna = readDnaPortE2 bitClk bitRst bitEna simDna2

    -- Start management unit
    (Fwd lc, muWbAll, muUartBytesBittide) <-
      withBittideClockResetEnable managementUnit maybeDna -< (muMm, muJtag)
    (ugnWbs, muWbs1) <- Vec.split -< muWbAll
    (ebWbs, muWbs2) <- Vec.split -< muWbs1
    (muSgWbs, [muTransceiverBus]) <- Vec.split -< muWbs2
    -- Stop management unit

    -- Start internal links
    (_relDatCount, _underflow, _overflow, _ebStables, Fwd rxs1) <-
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

    Fwd rxs2 <-
      withBittideClockResetEnable $ Vec.vecCircuits (captureUgn lc <$> rxs1) -< ugnWbs
    -- Stop internal links

    -- Start general purpose processing element
    (txs, gppeUartBytesBittide) <-
      withBittideClockResetEnable gppe maybeDna rxs2 -< (gppeMm, muSgWbs, gppeJtag)
    -- Stop general purpose processing element

    -- Start Clock control
    ( sync
      , Fwd swCcOut0
      , [ ccUartBus
          , ccSampleMemoryBus
          ]
      ) <-
      withBittideClockResetEnable
        $ callistoSwClockControlC
          @LinkCount
          refClk
          refRst
          rxClocks
          rxResets
          NoDumpVcd
          ccConfig
        -< (ccMm, (ccJtag, mask, linksSuitableForCc))

    withBittideClockResetEnable
      (wbStorage "SampleMemory")
      (Undefined @36_000 @(BitVector 32))
      -< ccSampleMemoryBus

    (ccUartBytesBittide, _uartStatus) <-
      withBittideClockResetEnable
        $ uartInterfaceWb d16 d16 uartBytes
        -< (ccUartBus, Fwd (pure Nothing))
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
         , Fwd lc
         , txs
         , sync
         , muUartBytesBittide
         , ccUartBytesBittide
         , gppeUartBytesBittide
         , muTransceiverBus
         )
 where
  withBittideClockResetEnable :: forall r. ((HiddenClockResetEnable Bittide) => r) -> r
  withBittideClockResetEnable = withClockResetEnable bitClk bitRst bitEna

uncurry5 ::
  (a -> b -> c -> d -> e -> f) ->
  (a, b, c, d, e) ->
  f
uncurry5 fn (a, b, c, d, e) = fn a b c d e

unzip5Vec ::
  Circuit (Vec n (a, b, c, d, e)) (Vec n a, Vec n b, Vec n c, Vec n d, Vec n e)
unzip5Vec = applyC unzip5 (uncurry5 zip5)
