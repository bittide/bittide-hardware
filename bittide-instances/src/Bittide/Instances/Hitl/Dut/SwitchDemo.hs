-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-}

module Bittide.Instances.Hitl.Dut.SwitchDemo where

import Clash.Explicit.Prelude
import Clash.Prelude (HiddenClockResetEnable, withClockResetEnable)

import Bittide.Calendar (CalendarConfig (..), ValidEntry (..))
import Bittide.CaptureUgn (captureUgn)
import Bittide.ClockControl.Callisto.Types (CallistoResult (..), Stability (..))
import Bittide.ClockControl.CallistoSw (SwcccInternalBusses, callistoSwClockControlC)
import Bittide.Df (asciiDebugMux)
import Bittide.DoubleBufferedRam
import Bittide.ElasticBuffer
import Bittide.Instances.Domains (Basic125, Bittide, GthRx)
import Bittide.Instances.Hitl.Setup (FpgaCount, LinkCount)
import Bittide.Jtag (jtagChain)
import Bittide.ProcessingElement (
  PeConfig (..),
  PrefixWidth,
  RemainingBusWidth,
  processingElement,
 )
import Bittide.SharedTypes (Byte, Bytes, withBittideByteOrder)
import Bittide.Switch (switchC)
import Bittide.SwitchDemoProcessingElement (SimplePeState, switchDemoPeWb)
import Bittide.Wishbone (
  makeWhoAmIdTh,
  readDnaPortE2Wb,
  timeWb,
  uartBytes,
  uartDf,
  uartInterfaceWb,
  whoAmIC,
 )

import Clash.Class.BitPackC (ByteOrder)
import Clash.Cores.Xilinx.DcFifo (dcFifoDf)
import Clash.Cores.Xilinx.Unisim.DnaPortE2 (simDna2)
import Data.Char (ord)
import Protocols
import Protocols.MemoryMap (ConstBwd, MM, MemoryMap)
import Protocols.Wishbone
import VexRiscv (DumpVcd (..), Jtag, JtagIn (..))

import qualified Protocols.MemoryMap as MM
import qualified Protocols.Vec as Vec

#ifdef SIM_BAUD_RATE
type Baud = MaxBaudRate Basic125
#else
type Baud = 921_600
#endif

type FifoSize = 5 -- = 2^5 = 32

baud :: SNat Baud
baud = SNat

ccWhoAmId, muWhoAmId :: BitVector 32
ccWhoAmId = $(makeWhoAmIdTh "swcc")
muWhoAmId = $(makeWhoAmIdTh "mgmt")

{- Internal busses:
    - Instruction memory
    - Data memory
    - `timeWb`
-}
type NmuInternalBusses = 3
type NmuRemBusWidth nodeBusses = RemainingBusWidth (nodeBusses + NmuInternalBusses)

data SimpleManagementConfig nodeBusses where
  SimpleManagementConfig ::
    (KnownNat nodeBusses) =>
    { peConfig :: PeConfig (nodeBusses + NmuInternalBusses)
    -- ^ Configuration for the internal 'processingElement'
    , dumpVcd :: DumpVcd
    -- ^ VCD dump configuration
    } ->
    SimpleManagementConfig nodeBusses

simpleManagementUnitC ::
  forall bitDom nodeBusses.
  ( KnownDomain bitDom
  , HiddenClockResetEnable bitDom
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  , 1 <= DomainPeriod bitDom
  , KnownNat nodeBusses
  , PrefixWidth (nodeBusses + NmuInternalBusses) <= 30
  ) =>
  SimpleManagementConfig nodeBusses ->
  Circuit
    (MM.ConstBwd MM.MM, (Jtag bitDom, CSignal bitDom (BitVector 64)))
    ( CSignal bitDom (Unsigned 64)
    , Vec
        nodeBusses
        ( MM.ConstBwd MM.MM
        , Wishbone bitDom 'Standard (NmuRemBusWidth nodeBusses) (Bytes 4)
        )
    )
simpleManagementUnitC (SimpleManagementConfig peConfig dumpVcd) =
  circuit $ \(mm, (jtag, _linkIn)) -> do
    peWbs <- processingElement dumpVcd peConfig -< (mm, jtag)
    ([timeWbBus], nmuWbs) <- Vec.split -< peWbs
    localCounter <- timeWb -< timeWbBus

    idC -< (localCounter, nmuWbs)

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

memoryMapCc, memoryMapMu :: MemoryMap
(memoryMapCc, memoryMapMu) = (ccMm, muMm)
 where
  Circuit circuitFn =
    withBittideByteOrder
      $ circuitFnC
        (clockGen, resetGen, enableGen)
        (clockGen, resetGen, enableGen)
        (repeat clockGen)
        (repeat resetGen)
  ((SimOnly ccMm, SimOnly muMm, _, _, _, _, _, _), _) =
    circuitFn
      (
        ( ()
        , ()
        , pure (JtagIn 0 0 0)
        , pure maxBound
        , pure maxBound
        , pure maxBound
        , repeat (pure Nothing)
        , pure low
        )
      ,
        ( pure ()
        , repeat (pure ())
        , pure ()
        , pure ()
        , pure ()
        , pure ()
        , pure ()
        , pure ()
        , pure ()
        , repeat $ pure ()
        )
      )

muConfig :: SimpleManagementConfig 19
muConfig =
  SimpleManagementConfig
    { peConfig =
        PeConfig
          { initI = Undefined @(Div (64 * 1024) 4)
          , initD = Undefined @(Div (64 * 1024) 4)
          , iBusTimeout = d0
          , dBusTimeout = d0
          , includeIlaWb = False
          }
    , dumpVcd = NoDumpVcd
    }

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

ccLabel, muLabel :: Vec 2 Byte
ccLabel = fromIntegral (ord 'C') :> fromIntegral (ord 'C') :> Nil
muLabel = fromIntegral (ord 'M') :> fromIntegral (ord 'U') :> Nil

circuitFnC ::
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
    , Jtag Bittide
    , CSignal Bittide (BitVector 64)
    , CSignal Bittide (BitVector LinkCount)
    , CSignal Bittide (BitVector LinkCount)
    , Vec LinkCount (CSignal GthRx (Maybe (BitVector 64)))
    , CSignal Bittide Bit
    )
    ( CSignal Bittide (CallistoResult LinkCount)
    , "TXS" ::: Vec LinkCount (CSignal Bittide (BitVector 64))
    , "LOCAL_COUNTER" ::: CSignal Bittide (Unsigned 64)
    , "PE_STATE" ::: CSignal Bittide (SimplePeState FpgaCount)
    , "PE_IN" ::: CSignal Bittide (BitVector 64)
    , "PE_OUT" ::: CSignal Bittide (BitVector 64)
    , "CAL_ENTRY" ::: CSignal Bittide (Vec (LinkCount + 1) (Index (LinkCount + 2)))
    , "UART_TX" ::: CSignal Basic125 Bit
    , "SYNC_OUT" ::: CSignal Basic125 Bit
    , "EB_STABLES" ::: Vec LinkCount (CSignal Bittide Bool)
    )
circuitFnC (refClk, refRst, refEna) (bitClk, bitRst, bitEna) rxClocks rxResets =
  circuit $ \(ccMM, muMM, jtag, linkIn, mask, linksSuitableForCc, Fwd rxs0, syncIn) -> do
    [muJtag, ccJtag] <- jtagChain -< jtag

    (muUartBytesBittide, _muUartStatus) <-
      defaultBittideClkRstEn
        $ uartInterfaceWb d16 d16 uartBytes
        -< (muUartBus, Fwd (pure Nothing))

    muUartBytes <-
      dcFifoDf d5 bitClk bitRst refClk refRst
        -< muUartBytesBittide

    (Fwd lc, muWbAll) <-
      defaultBittideClkRstEn (simpleManagementUnitC muConfig) -< (muMM, (muJtag, linkIn))

    ( [ muWhoAmIWb
        , (peWbMM, peWb)
        , (switchWbMM, switchWb)
        , dnaWb
        , muUartBus
        ]
      , restWbs
      ) <-
      Vec.split -< muWbAll

    (ugnWbs, ebWbs) <- Vec.split -< restWbs
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

    rxs2 <- defaultBittideClkRstEn $ Vec.vecCircuits (captureUgn lc <$> rxs1) -< ugnWbs

    rxs3 <- Vec.append -< ([Fwd peOut], rxs2)
    (switchOut, calEntry) <-
      defaultBittideClkRstEn $ switchC calendarConfig -< (switchWbMM, (rxs3, switchWb))
    ([Fwd peIn], txs) <- Vec.split -< switchOut

    (Fwd peOut, ps) <-
      defaultBittideClkRstEn (switchDemoPeWb (SNat @FpgaCount))
        -< (peWbMM, (Fwd lc, peWb, dna, Fwd peIn))

    dna <- defaultBittideClkRstEn (readDnaPortE2Wb simDna2) -< dnaWb

    defaultBittideClkRstEn (whoAmIC muWhoAmId) -< muWhoAmIWb

    (ccUartBytesBittide, _uartStatus) <-
      defaultBittideClkRstEn
        $ uartInterfaceWb d16 d16 uartBytes
        -< (ccUartBus, Fwd (pure Nothing))

    ccUartBytes <-
      dcFifoDf d5 bitClk bitRst refClk refRst
        -< ccUartBytesBittide

    uartTxBytes <-
      defaultRefClkRstEn
        $ asciiDebugMux d1024 (ccLabel :> muLabel :> Nil)
        -< [ccUartBytes, muUartBytes]
    (_uartInBytes, uartTx) <- defaultRefClkRstEn $ uartDf baud -< (uartTxBytes, Fwd 0)

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

    defaultBittideClkRstEn (whoAmIC ccWhoAmId) -< ccWhoAmIBus

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
         , ps
         , Fwd peIn
         , Fwd peOut
         , calEntry
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
