-- SPDX-FileCopyrightText: 2022-2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module VexRiscv where

import Clash.Annotations.Primitive
import Clash.Prelude
import Clash.Signal.Internal
import Data.Bifunctor (first)
import Data.String.Interpolate (__i)
import Data.Word (Word64)
import Foreign (Ptr, nullPtr)
import Foreign.C.String (newCString)
import Foreign.Marshal (alloca)
import Foreign.Storable
import GHC.IO (unsafeInterleaveIO, unsafePerformIO)
import GHC.Stack (HasCallStack)
import Protocols
import Protocols.Idle
import Protocols.Wishbone

import VexRiscv.BlackBox (vexRiscvBBF)
import VexRiscv.ClockTicks (singleClockEdgesRelative)
import VexRiscv.FFI
import VexRiscv.Random (unsafeMakeDefinedRandom)
import VexRiscv.Reset

data JtagIn = JtagIn
  { testClock :: "TCK" ::: Bit
  , testModeSelect :: "TMS" ::: Bit
  , testDataIn :: "TDI" ::: Bit
  }
  deriving (Generic, Eq, NFDataX, ShowX, BitPack)

data JtagOut = JtagOut
  { testDataOut :: "TDO" ::: Bit
  }
  deriving (Generic, NFDataX, ShowX, Eq, BitPack)

data CpuIn = CpuIn
  { timerInterrupt :: "TIMER_INTERRUPT" ::: Bit
  , externalInterrupt :: "EXTERNAL_INTERRUPT" ::: Bit
  , softwareInterrupt :: "SOFTWARE_INTERRUPT" ::: Bit
  , iBusWbS2M :: "IBUS_IN_" ::: WishboneS2M (BitVector 32)
  , dBusWbS2M :: "DBUS_IN_" ::: WishboneS2M (BitVector 32)
  }
  deriving (Generic, NFDataX, ShowX, Eq, BitPack)

data CpuOut = CpuOut
  { iBusWbM2S :: "IBUS_OUT_" ::: WishboneM2S 30 4 (BitVector 32)
  , dBusWbM2S :: "DBUS_OUT_" ::: WishboneM2S 30 4 (BitVector 32)
  , ndmreset :: "RST" ::: Bit
  -- ^ Peripheral reset produced by `EmbeddedRiscvJtag` plugin.
  , stoptime :: "STOPTIME" ::: Bit
  -- ^ Seems to be some kind of debug signal produced by the `CsrPlugin`.
  }
  deriving (Generic, NFDataX, ShowX, Eq, BitPack)

inputToNonCombInput :: Bool -> CpuIn -> NON_COMB_INPUT
inputToNonCombInput rst cpuIn =
  NON_COMB_INPUT
    { reset = boolToBit (unsafeMakeDefinedRandom rst)
    , timerInterrupt = unsafeMakeDefinedRandom timerInterrupt
    , externalInterrupt = unsafeMakeDefinedRandom externalInterrupt
    , softwareInterrupt = unsafeMakeDefinedRandom softwareInterrupt
    }
 where
  CpuIn{timerInterrupt, externalInterrupt, softwareInterrupt} = cpuIn

inputToCombInput :: CpuIn -> JtagIn -> COMB_INPUT
inputToCombInput cpuIn jtagIn =
  COMB_INPUT
    { iBusWishbone_ACK = boolToBit (unsafeMakeDefinedRandom iBusAck)
    , iBusWishbone_DAT_MISO = unpack (unsafeMakeDefinedRandom iBusDat)
    , iBusWishbone_ERR = boolToBit (unsafeMakeDefinedRandom iBusErr)
    , dBusWishbone_ACK = boolToBit (unsafeMakeDefinedRandom dBusAck)
    , dBusWishbone_DAT_MISO = unpack (unsafeMakeDefinedRandom dBusDat)
    , dBusWishbone_ERR = boolToBit (unsafeMakeDefinedRandom dBusErr)
    , jtag_TCK = (unsafeMakeDefinedRandom testClock)
    , jtag_TMS = (unsafeMakeDefinedRandom testModeSelect)
    , jtag_TDI = (unsafeMakeDefinedRandom testDataIn)
    }
 where
  CpuIn{iBusWbS2M, dBusWbS2M} = cpuIn
  JtagIn{testClock, testModeSelect, testDataIn} = jtagIn
  WishboneS2M{acknowledge = iBusAck, readData = iBusDat, err = iBusErr} = iBusWbS2M
  WishboneS2M{acknowledge = dBusAck, readData = dBusDat, err = dBusErr} = dBusWbS2M

outputToCpuOut :: OUTPUT -> CpuOut
outputToCpuOut OUTPUT{..} =
  CpuOut
    { iBusWbM2S =
        WishboneM2S
          { addr = truncateB (pack iBusWishbone_ADR)
          , writeData = pack (iBusWishbone_DAT_MOSI)
          , busSelect = unpack (truncateB (pack iBusWishbone_SEL))
          , lock = False
          , busCycle = bitToBool iBusWishbone_CYC
          , strobe = bitToBool iBusWishbone_STB
          , writeEnable = bitToBool iBusWishbone_WE
          , cycleTypeIdentifier = unpack (truncateB (pack iBusWishbone_CTI))
          , burstTypeExtension = unpack (truncateB (pack iBusWishbone_BTE))
          }
    , dBusWbM2S =
        WishboneM2S
          { addr = truncateB (pack dBusWishbone_ADR)
          , writeData = pack (dBusWishbone_DAT_MOSI)
          , busSelect = unpack (truncateB (pack dBusWishbone_SEL))
          , lock = False
          , busCycle = bitToBool dBusWishbone_CYC
          , strobe = bitToBool dBusWishbone_STB
          , writeEnable = bitToBool dBusWishbone_WE
          , cycleTypeIdentifier = unpack (truncateB (pack dBusWishbone_CTI))
          , burstTypeExtension = unpack (truncateB (pack dBusWishbone_BTE))
          }
    , ndmreset = ndmreset
    , stoptime = stoptime
    }

outputToJtagOut :: OUTPUT -> JtagOut
outputToJtagOut OUTPUT{jtag_TDO} = JtagOut{testDataOut = jtag_TDO}

data DumpVcd = DumpVcd FilePath | NoDumpVcd

data Jtag (dom :: Domain)

instance Protocol (Jtag dom) where
  type Fwd (Jtag dom) = Signal dom JtagIn
  type Bwd (Jtag dom) = Signal dom JtagOut

instance IdleCircuit (Jtag dom) where
  idleFwd _ = pure $ JtagIn 0 0 0
  idleBwd _ = pure $ JtagOut 0

vexRiscv ::
  forall dom.
  ( HasCallStack
  , KnownDomain dom
  ) =>
  DumpVcd ->
  Clock dom ->
  MinCyclesReset dom 2 ->
  Signal dom CpuIn ->
  Signal dom JtagIn ->
  ( Signal dom CpuOut
  , Signal dom JtagOut
  )
vexRiscv dumpVcd clk rst cpuInput jtagInput = unsafePerformIO $ do
  -- PeriodToHz imposes a minimum period of 1 Hz, but the KnownDomain constraint does
  -- not supply this information.
  let domPeriodFs = hzToFs (natToNum @(PeriodToHz (Max 1 (DomainPeriod dom))))
  (v, initStage1, initStage2, stepRising, stepFalling, _shutDown) <- vexCPU dumpVcd domPeriodFs

  let
    nonCombInput = inputToNonCombInput <$> unsafeToActiveHigh (fromMinCycles rst) <*> cpuInput
    combInput = inputToCombInput <$> cpuInput <*> jtagInput

    simInitThenCycles ::
      Signal dom NON_COMB_INPUT ->
      Signal dom COMB_INPUT ->
      IO (Signal dom OUTPUT)
    simInitThenCycles (cnc :- cncs) ~(cc :- ccs) = do
      let
        -- Note: we don't need @ticks@ for the initialization stages, because this
        -- first cycle of a 'Signal' is meant to model what happens _before_ a
        -- clock edge.
        ticks = first fromIntegral <$> singleClockEdgesRelative clk

      out0 <- initStage1 v cnc
      stage2Out <- unsafeInterleaveIO (initStage2 v cc)
      out1 <- unsafeInterleaveIO (simCycles ticks cncs ccs)

      pure $ out0 :- (stage2Out `seq` out1)

    simCycles ::
      [(Word64, ActiveEdge)] ->
      Signal dom NON_COMB_INPUT ->
      Signal dom COMB_INPUT ->
      IO (Signal dom OUTPUT)
    simCycles ((fsSinceLastEvent, Rising) : ts) (cnc :- cncs) ccs = do
      out0 <- stepRising v fsSinceLastEvent cnc
      out1 <- unsafeInterleaveIO (simCycles ts cncs ccs)
      pure $ out0 :- out1
    simCycles ((fsSinceLastEvent, Falling) : ts) cncs (cc :- ccs) = do
      stepFalling v fsSinceLastEvent cc
      simCycles ts cncs ccs
    simCycles [] _ _ = error "Empty ticks: should never happen"

  output <- simInitThenCycles nonCombInput combInput

  pure
    ( outputToCpuOut <$> output
    , outputToJtagOut <$> output
    )
{-# CLASH_OPAQUE vexRiscv #-}
{-# ANN vexRiscv hasBlackBox #-}
{-# ANN
  vexRiscv
  ( let primName = 'vexRiscv
        tfName = 'vexRiscvBBF
     in InlineYamlPrimitive
          [Verilog]
          [__i|
            BlackBoxHaskell:
                name: #{primName}
                templateFunction: #{tfName}
                workInfo: Always
         |]
  )
  #-}

{- | Return a function that performs an execution step and a function to free
the internal CPU state
-}
vexCPU ::
  DumpVcd ->
  Femtoseconds ->
  IO
    ( Ptr VexRiscv
    , Ptr VexRiscv -> NON_COMB_INPUT -> IO OUTPUT -- initStage1
    , Ptr VexRiscv -> COMB_INPUT -> IO () -- initStage2
    , Ptr VexRiscv -> Word64 -> NON_COMB_INPUT -> IO OUTPUT -- rising
    , Ptr VexRiscv -> Word64 -> COMB_INPUT -> IO () -- falling
    , Ptr VexRiscv -> IO ()
    )
vexCPU dumpVcd (fromIntegral . unFemtoseconds -> domPeriodFs :: Word64) = do
  v <- vexrInit
  vcd <- case dumpVcd of
    NoDumpVcd -> pure nullPtr
    DumpVcd path -> do
      vcdPath <- newCString path
      vexrInitVcd v vcdPath

  let
    {-# NOINLINE initStage1 #-}
    initStage1 vPtr nonCombInput =
      alloca $ \nonCombInputFFI -> alloca $ \outputFFI -> do
        poke nonCombInputFFI nonCombInput
        vexrInitStage1 vcd domPeriodFs vPtr nonCombInputFFI outputFFI
        peek outputFFI

    {-# NOINLINE initStage2 #-}
    initStage2 vPtr combInput =
      alloca $ \combInputFFI -> do
        poke combInputFFI combInput
        vexrInitStage2 vPtr combInputFFI

    {-# NOINLINE stepRising #-}
    stepRising vPtr fsSinceLastEvent nonCombInput =
      alloca $ \nonCombInputFFI -> alloca $ \outputFFI -> do
        poke nonCombInputFFI nonCombInput
        vexrStepRisingEdge vcd vPtr fsSinceLastEvent nonCombInputFFI outputFFI
        peek outputFFI

    {-# NOINLINE stepFalling #-}
    stepFalling vPtr fsSinceLastEvent combInput =
      alloca $ \combInputFFI -> do
        poke combInputFFI combInput
        vexrStepFallingEdge vcd vPtr fsSinceLastEvent combInputFFI

    shutDown = vexrShutdown

  pure (v, initStage1, initStage2, stepRising, stepFalling, shutDown)
