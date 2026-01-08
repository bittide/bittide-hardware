-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Bittide.Instances.Hitl.SwitchDemo.Core (core, ExtraPipeliningRx, ExtraPipeliningTx) where

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
import Bittide.Instances.Hitl.Setup (FpgaCount, LinkCount)
import Bittide.Instances.Hitl.Utils.Protocols (delayLinks)
import Bittide.Jtag (jtagChain)
import Bittide.ProcessingElement (
  PeConfig (..),
  PeInternalBusses,
  PrefixWidth,
  RemainingBusWidth,
  processingElement,
 )
import Bittide.SharedTypes (Bitbone, BitboneMm)
import Bittide.Switch (switchC)
import Bittide.SwitchDemoProcessingElement (switchDemoPeWb)
import Bittide.Sync (Sync)
import Bittide.Wishbone (readDnaPortE2WbWorker, timeWb, uartBytes, uartInterfaceWb)
import Clash.Class.BitPackC (ByteOrder)
import Clash.Cores.Xilinx.Unisim.DnaPortE2 (readDnaPortE2, simDna2)
import Data.Maybe (fromMaybe)
import Protocols.MemoryMap (Mm)
import VexRiscv (DumpVcd (..), Jtag)

import qualified Bittide.Cpus.Riscv32imc as Riscv32imc
import qualified Protocols.MemoryMap as Mm
import qualified Protocols.Vec as Vec

type FifoSize = 5 -- = 2^5 = 32

type ExtraPipeliningRxPreSwitch = 1
type ExtraPipeliningRxPostSwitch = 1
type ExtraPipeliningRx = ExtraPipeliningRxPreSwitch + ExtraPipeliningRxPostSwitch

type ExtraPipeliningTxPreSwitch = 1
type ExtraPipeliningTxPostSwitch = 1
type ExtraPipeliningTx = ExtraPipeliningTxPreSwitch + ExtraPipeliningTxPostSwitch

rxDelayPre
  , rxDelayPost
  , txDelayPre
  , txDelayPost ::
    forall dom a m.
    (KnownDomain dom, NFDataX a, KnownNat m) =>
    Clock dom ->
    Circuit (Vec m (CSignal dom a)) (Vec m (CSignal dom a))
rxDelayPre clk = delayLinks (SNat @ExtraPipeliningRxPreSwitch) (repeat clk)
rxDelayPost clk = delayLinks (SNat @ExtraPipeliningRxPostSwitch) (repeat clk)
txDelayPre clk = delayLinks (SNat @ExtraPipeliningTxPreSwitch) (repeat clk)
txDelayPost clk = delayLinks (SNat @ExtraPipeliningTxPostSwitch) (repeat clk)

{- Internal busses:
    - Instruction memory
    - Data memory
    - `timeWb`
    - DNA
    - UART
-}
type NmuInternalBusses = 3 + PeInternalBusses

{- Busses per link:
    - UGN component
    - Elastic buffer
-}
type PeripheralsPerLink = 2

{- External busses:
    - ASIC PE
    - Switch calendar
    - Transceivers
    - Callisto
-}
type NmuExternalBusses = 4 + (LinkCount * PeripheralsPerLink)
type NmuRemBusWidth = RemainingBusWidth (NmuExternalBusses + NmuInternalBusses)

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
    , Df dom (BitVector 8)
    , Vec
        NmuExternalBusses
        ( ToConstBwd Mm.Mm
        , Bitbone dom NmuRemBusWidth
        )
    )
managementUnit maybeDna =
  circuit $ \(mm, jtag) -> do
    -- Core and interconnect
    allBusses <- processingElement NoDumpVcd muConfig -< (mm, jtag)
    ([timeBus, uartBus, dnaBus], restBusses) <- Vec.split -< allBusses

    -- Peripherals
    localCounter <- timeWb -< timeBus
    (uartOut, _uartStatus) <-
      uartInterfaceWb d16 d16 uartBytes -< (uartBus, Fwd (pure Nothing))
    readDnaPortE2WbWorker maybeDna -< dnaBus

    -- Output
    idC -< (localCounter, uartOut, restBusses)

{- FOURMOLU_DISABLE -} -- Fourmolu doesn't do well with tabular code
calendarConfig :: CalendarConfig 25 (Vec 8 (Index 9))
calendarConfig =
  CalendarConfig
    (SNat @LinkCount)
    {- The '@12' is so that the generated Rust code works. At time of writing,
    the generator makes two separate device-specific types for 'ValidEntry' since
    they have differing repetition bit widths. To fix this, all tests are being
    set to a width of 12.
    -}
    (SNat @12)

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
  nRepetitions = numConvert (maxBound :: Index (FpgaCount * 3))
{- FOURMOLU_ENABLE -}

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
    , Jtag Bittide
    , CSignal Bittide (BitVector LinkCount)
    , CSignal Bittide (BitVector LinkCount)
    , Vec LinkCount (CSignal GthRx (Maybe (BitVector 64)))
    )
    ( CSignal Bittide (CallistoResult LinkCount)
    , "LOCAL_COUNTER" ::: CSignal Bittide (Unsigned 64)
    , "TXS" ::: Vec LinkCount (CSignal Bittide (BitVector 64))
    , Sync Bittide Basic125
    , "MU_UART" ::: Df Bittide (BitVector 8)
    , "CC_UART" ::: Df Bittide (BitVector 8)
    , "MU_TRANSCEIVER"
        ::: (BitboneMm Bittide NmuRemBusWidth)
    )
core (refClk, refRst) (bitClk, bitRst, bitEna) rxClocks rxResets =
  circuit $ \(muMm, ccMm, jtag, mask, linksSuitableForCc, rxs0) -> do
    [muJtag, ccJtag] <- jtagChain -< jtag

    let maybeDna = readDnaPortE2 bitClk bitRst bitEna simDna2

    -- Start management unit
    (Fwd lc, muUartBytesBittide, muWbAll) <-
      withBittideClockResetEnable managementUnit maybeDna -< (muMm, muJtag)
    (ugnWbs, muWbs1) <- Vec.split -< muWbAll
    (ebWbs, muWbs2) <- Vec.split -< muWbs1
    [ (peWbMM, peWb)
      , (switchWbMM, switchWb)
      , muTransceiverBus
      , muCallistoBus
      ] <-
      idC -< muWbs2
    -- Stop management unit

    -- Start internal links
    Fwd rxs1 <- delayLinks d1 rxClocks -< rxs0 -- alleviate timing, included in hw UGN
    (_relDatCount, _underflow, _overflow, _ebStables, rxs2) <-
      unzip5Vec
        <| ( Vec.vecCircuits
              $ xilinxElasticBufferWb
                bitClk
                bitRst
                (SNat @FifoSize)
              <$> rxClocks
              <*> rxs1
           )
        -< ebWbs

    Fwd rxs3 <- delayLinks d1 (repeat bitClk) -< rxs2 -- alleviate timing, included in hw UGN
    rxs4 <- withBittideClockResetEnable $ Vec.vecCircuits (captureUgn lc <$> rxs3) -< ugnWbs

    -- alleviate timing, not included in hw UGN
    switchInRx <- rxDelayPre bitClk -< rxs4
    switchInTx <- txDelayPre bitClk -< [peOut]
    switchIn <- Vec.append -< (switchInRx, switchInTx)

    (switchOut, _calEntry) <-
      withBittideClockResetEnable $ switchC calendarConfig -< (switchWbMM, (switchIn, switchWb))
    (switchOutRx, switchOutTx) <- Vec.split -< switchOut

    -- alleviate timing, not included in hw UGN
    txs <- txDelayPost bitClk -< switchOutTx
    [Fwd peIn] <- rxDelayPost bitClk -< switchOutRx
    -- Stop internal links

    -- Start ASIC processing element
    -- XXX: It's slightly iffy to use fromMaybe here, but in practice nothing will
    --      use it until the DNA is actually read out.
    (peOut, _peState) <-
      withBittideClockResetEnable (switchDemoPeWb (SNat @FpgaCount))
        -< (peWbMM, (Fwd lc, peWb, Fwd (fromMaybe 0 <$> maybeDna), Fwd peIn))

    -- Stop ASIC processing element

    -- Start clock control
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
        -< (ccMm, muCallistoBus, (ccJtag, mask, linksSuitableForCc))

    withBittideClockResetEnable
      (wbStorage "SampleMemory")
      (Undefined @36_000 @(BitVector 32))
      -< ccSampleMemoryBus

    (ccUartBytesBittide, _uartStatus) <-
      withBittideClockResetEnable
        $ uartInterfaceWb d16 d16 uartBytes
        -< (ccUartBus, Fwd (pure Nothing))
    -- Stop clock control

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
