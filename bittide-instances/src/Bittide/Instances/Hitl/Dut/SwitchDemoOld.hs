-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-}

module Bittide.Instances.Hitl.Dut.SwitchDemoOld where

import Clash.Explicit.Prelude
import Clash.Prelude (HiddenClockResetEnable, withClockResetEnable)

import Bittide.Calendar (CalendarConfig (..), ValidEntry (..))
import Bittide.CaptureUgn (captureUgn)
import Bittide.ClockControl.Callisto.Types (CallistoResult (..), Stability (..))
import Bittide.ClockControl.CallistoSw (
  SwControlCConfig (..),
  SwcccInternalBusses,
  callistoSwClockControlC,
 )
import Bittide.Df (asciiDebugMux)
import Bittide.DoubleBufferedRam
import Bittide.Instances.Domains (Basic125, Bittide, GthRx)
import Bittide.Instances.Hitl.Dut.SwitchDemo (ccWhoAmID, muWhoAmID, whoAmIPrefix)
import Bittide.Instances.Hitl.Setup (FpgaCount, LinkCount)
import Bittide.Jtag (jtagChain)
import Bittide.ProcessingElement (PeConfig (..), processingElement)
import Bittide.SharedTypes (Byte, Bytes, withBittideByteOrder)
import Bittide.Switch (switchC)
import Bittide.SwitchDemoProcessingElement (SimplePeState, switchDemoPeWb)
import Bittide.Wishbone (
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
import Protocols.Extra
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

baud :: SNat Baud
baud = SNat

{- Internal busses:
    - Instruction memory
    - Data memory
    - `whoAmI`
    - `timeWb`
-}
type NmuInternalBusses = 4
type NmuRemBusWidth nodeBusses = 30 - CLog 2 (nodeBusses + NmuInternalBusses)

data SimpleManagementConfig nodeBusses where
  SimpleManagementConfig ::
    (KnownNat nodeBusses) =>
    { peConfig :: PeConfig (nodeBusses + NmuInternalBusses)
    -- ^ Configuration for the internal 'processingElement'
    , timeRegPrefix :: Unsigned (CLog 2 (nodeBusses + NmuInternalBusses))
    -- ^ Time control register prefix
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
  , CLog 2 (nodeBusses + NmuInternalBusses) <= 30
  ) =>
  SimpleManagementConfig nodeBusses ->
  Circuit
    (MM.ConstBwd MM.MM, (Jtag bitDom, CSignal bitDom (BitVector 64)))
    ( CSignal bitDom (Unsigned 64)
    , Vec
        nodeBusses
        ( MM.ConstBwd (Unsigned (CLog 2 (nodeBusses + NmuInternalBusses)))
        , ( MM.ConstBwd MM.MM
          , Wishbone bitDom 'Standard (NmuRemBusWidth nodeBusses) (Bytes 4)
          )
        )
    )
simpleManagementUnitC (SimpleManagementConfig peConfig pfxTime dumpVcd) =
  circuit $ \(mm, (jtag, _linkIn)) -> do
    peWbs <- processingElement dumpVcd peConfig -< (mm, jtag)
    ([(timePfx, timeWbBus)], nmuWbs) <- splitAtC d1 -< peWbs
    localCounter <- timeWb -< timeWbBus

    MM.constBwd pfxTime -< timePfx

    idC -< (localCounter, nmuWbs)

{- FOURMOLU_DISABLE -} -- Fourmolu doesn't do well with tabular code
calendarConfig :: CalendarConfig 26 (Vec 8 (Index 9))
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
  creGen :: forall dom. (KnownDomain dom) => (Clock dom, Reset dom, Enable dom)
  creGen = (clockGen, resetGen, enableGen)

  Circuit switchDemoOldFn =
    withBittideByteOrder
      $ switchDemoOldC
        creGen
        creGen
        (repeat clockGen)
        (repeat resetGen)
  ((SimOnly ccMm, SimOnly muMm, _, _, _, _, _, _), _) =
    switchDemoOldFn
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
        )
      )

muConfig :: SimpleManagementConfig 12
muConfig =
  SimpleManagementConfig
    { peConfig =
        PeConfig
          { prefixI = 0b1000
          , prefixD = 0b1100
          , initI = Undefined @(Div (64 * 1024) 4)
          , initD = Undefined @(Div (64 * 1024) 4)
          , iBusTimeout = d0
          , dBusTimeout = d0
          , includeIlaWb = False
          , whoAmID = muWhoAmID
          , whoAmIPrefix = whoAmIPrefix
          }
    , timeRegPrefix = 0b1101
    , dumpVcd = NoDumpVcd
    }

ccConfig ::
  forall n.
  ( CLog 2 (n + SwcccInternalBusses) <= 30
  , 3 <= CLog 2 (n + SwcccInternalBusses)
  , KnownNat n
  ) =>
  SwControlCConfig n
ccConfig =
  SwControlCConfig
    { peConfig =
        PeConfig
          { prefixI = 0b1000
          , prefixD = 0b0100
          , initI = Undefined @(Div (64 * 1024) 4)
          , initD = Undefined @(Div (64 * 1024) 4)
          , iBusTimeout = d0
          , dBusTimeout = d0
          , includeIlaWb = False
          , whoAmID = ccWhoAmID
          , whoAmIPrefix = whoAmIPrefix @(CLog 2 (n + SwcccInternalBusses))
          }
    , ccRegPrefix = 0b1100
    , timePrefix = 0b0110
    , freezePrefix = 0b0010
    , syncOutGeneratorPrefix = 0b0001
    , domainDiffsPrefix = 0b0011
    }

ccLabel :: Vec 2 Byte
ccLabel = fromIntegral (ord 'C') :> fromIntegral (ord 'C') :> Nil

muLabel :: Vec 2 Byte
muLabel = fromIntegral (ord 'M') :> fromIntegral (ord 'U') :> Nil

switchDemoOldC ::
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
    , Vec LinkCount (CSignal Bittide (Maybe (BitVector 64)))
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
    )
switchDemoOldC
  (refClk, refRst, refEna)
  (bittideClk, bittideRst, bittideEna)
  rxClocks
  rxResets =
    circuit $ \(ccMM, muMM, jtag, linkIn, mask, linksSuitableForCc, Fwd rxs, syncIn) -> do
      [muJtag, ccJtag] <- jtagChain -< jtag

      (muUartBytesBittide, _muUartStatus) <-
        defaultBittideClkRstEn
          $ uartInterfaceWb d16 d16 uartBytes
          -< (muUartBus, Fwd (pure Nothing))

      muUartBytes <-
        dcFifoDf d5 bittideClk bittideRst refClk refRst
          -< muUartBytesBittide

      (Fwd lc, muWbAll) <-
        defaultBittideClkRstEn (simpleManagementUnitC muConfig) -< (muMM, (muJtag, linkIn))
      ( [ (muWhoAmIPfx, (muWhoAmIMM, muWhoAmI))
          , (peWbPfx, (peWbMM, peWb))
          , (switchWbPfx, (switchWbMM, switchWb))
          , (dnaWbPfx, (dnaWbMM, dnaWb))
          , (muUartPfx, muUartBus)
          ]
        , muWbRest
        ) <-
        splitAtC SNat -< muWbAll
      ([ugn0Pfx, ugn1Pfx, ugn2Pfx, ugn3Pfx, ugn4Pfx, ugn5Pfx, ugn6Pfx], ugnData) <-
        unzipC -< muWbRest

      MM.constBwd 0b0000 -< muUartPfx
      MM.constBwd 0b0001 -< ugn0Pfx
      MM.constBwd 0b0010 -< ugn1Pfx
      MM.constBwd 0b0011 -< ugn2Pfx
      MM.constBwd 0b0100 -< ugn3Pfx
      MM.constBwd 0b0101 -< ugn4Pfx
      MM.constBwd 0b0110 -< ugn5Pfx
      MM.constBwd 0b0111 -< ugn6Pfx
      --          0b1000    IMEM
      MM.constBwd 0b1001 -< peWbPfx
      MM.constBwd 0b1010 -< switchWbPfx
      MM.constBwd 0b1011 -< dnaWbPfx
      --          0b1100    DMEM
      --          0b1101    TIME (not the same as CC!)
      MM.constBwd 0b1110 -< muWhoAmIPfx

      ugnRxs <-
        defaultBittideClkRstEn $ Vec.vecCircuits (captureUgn lc <$> rxs) -< ugnData

      rxLinks <- appendC -< ([Fwd peOut], ugnRxs)
      (switchOut, calEntry) <-
        defaultBittideClkRstEn $ switchC calendarConfig -< (switchWbMM, (rxLinks, switchWb))
      ([Fwd peIn], txs) <- splitAtC SNat -< switchOut

      (Fwd peOut, ps) <-
        defaultBittideClkRstEn (switchDemoPeWb (SNat @FpgaCount))
          -< (peWbMM, (Fwd lc, peWb, dna, Fwd peIn))

      dna <- defaultBittideClkRstEn (readDnaPortE2Wb simDna2) -< (dnaWbMM, dnaWb)

      defaultBittideClkRstEn (whoAmIC 0x746d_676d) -< (muWhoAmIMM, muWhoAmI)

      (ccUartBytesBittide, _uartStatus) <-
        defaultBittideClkRstEn
          $ uartInterfaceWb d16 d16 uartBytes
          -< (ccUartBus, Fwd (pure Nothing))

      ccUartBytes <-
        dcFifoDf d5 bittideClk bittideRst refClk refRst
          -< ccUartBytesBittide

      uartTxBytes <-
        defaultRefClkRstEn
          $ asciiDebugMux d1024 (ccLabel :> muLabel :> Nil)
          -< [ccUartBytes, muUartBytes]
      (_uartInBytes, uartTx) <- defaultRefClkRstEn $ uartDf baud -< (uartTxBytes, Fwd 0)

      ( syncOut
        , Fwd swCcOut0
        , [ (ccWhoAmIPfx, ccWhoAmIBus)
            , (ccUartPfx, ccUartBus)
            , (ccSampleMemoryPfx, ccSampleMemoryBus)
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

      MM.constBwd 0b0000 -< ccUartPfx
      --          0b0001    SYNC_OUT_GENERATOR
      --          0b0010    FREEZE
      --          0b0011    DOMAIN_DIFFS
      --          0b0100    DMEM
      --          0b0110    TIME (not the same as MU!)
      --          0b1000    IMEM
      --          0b1010    DBG
      --          0b1100    CC
      MM.constBwd 0b1110 -< ccWhoAmIPfx
      MM.constBwd 0b1111 -< ccSampleMemoryPfx

      defaultBittideClkRstEn
        (wbStorage "SampleMemory")
        (Undefined @36_000 @(BitVector 32))
        -< ccSampleMemoryBus

      defaultBittideClkRstEn (whoAmIC 0x6363_7773) -< ccWhoAmIBus

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
           )
   where
    defaultRefClkRstEn :: forall r. ((HiddenClockResetEnable Basic125) => r) -> r
    defaultRefClkRstEn = withClockResetEnable refClk refRst refEna
    defaultBittideClkRstEn :: forall r. ((HiddenClockResetEnable Bittide) => r) -> r
    defaultBittideClkRstEn = withClockResetEnable bittideClk bittideRst bittideEna
