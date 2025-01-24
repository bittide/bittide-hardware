-- SPDX-FileCopyrightText: 2022-2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module VexRiscv where

import Clash.Prelude

import Clash.Annotations.Primitive
import Clash.Signal.Internal
import Data.Bifunctor (first)
import Data.String.Interpolate (__i)
import Data.Word (Word64)
import Foreign (Ptr, nullPtr)
import Foreign.Marshal (alloca)
import Foreign.Storable
import Foreign.C.String (newCString)
import GHC.IO (unsafePerformIO, unsafeInterleaveIO)
import GHC.Stack (HasCallStack)
import Language.Haskell.TH.Syntax
import Protocols
import Protocols.Idle
import Protocols.Wishbone

import VexRiscv.ClockTicks
import VexRiscv.FFI
import VexRiscv.Random
import VexRiscv.TH
import VexRiscv.VecToTuple

import qualified VexRiscv.FFI as FFI

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
  -- | Peripheral reset produced by `EmbeddedRiscvJtag` plugin.
  , ndmreset :: "RST" ::: Bit
  -- | Seems to be some kind of debug signal produced by the `CsrPlugin`.
  , stoptime :: "STOPTIME" ::: Bit
  }
  deriving (Generic, NFDataX, ShowX, Eq, BitPack)

data DumpVcd = DumpVcd FilePath | NoDumpVcd


data Jtag (dom :: Domain)

instance Protocol (Jtag dom) where
  type Fwd (Jtag dom) = Signal dom JtagIn
  type Bwd (Jtag dom) = Signal dom JtagOut

instance IdleCircuit (Jtag dom) where
  idleFwd _ = pure $ JtagIn 0 0 0
  idleBwd _ = pure $ JtagOut 0

vexRiscv ::
  forall dom .
  ( HasCallStack
  , KnownDomain dom) =>
  DumpVcd ->
  Clock dom ->
  Reset dom ->
  Signal dom CpuIn ->
  Signal dom JtagIn ->
  ( Signal dom CpuOut
  , Signal dom JtagOut
  )
vexRiscv dumpVcd clk rst cpuInput jtagInput =
  ( CpuOut <$>
    (WishboneM2S
      <$> iBus_ADR
      <*> iBus_DAT_MOSI
      <*> iBus_SEL
      <*> pure False
      <*> iBus_CYC
      <*> iBus_STB
      <*> iBus_WE
      <*> (unpack <$> iBus_CTI)
      <*> (unpack <$> iBus_BTE)
    )
    <*>
    (WishboneM2S
      <$> dBus_ADR
      <*> dBus_DAT_MOSI
      <*> dBus_SEL
      <*> pure False
      <*> dBus_CYC
      <*> dBus_STB
      <*> dBus_WE
      <*> (unpack <$> dBus_CTI)
      <*> (unpack <$> dBus_BTE)
    )
    <*>
    ndmreset
    <*>
    stoptime
  , JtagOut <$> jtag_TDO
  )

  where
    (unbundle -> (timerInterrupt, externalInterrupt, softwareInterrupt, iBusS2M, dBusS2M))
      = (\(CpuIn a b c d e) -> (a, b, c, d, e)) <$> cpuInput

    (unbundle -> (iBus_DAT_MISO, iBus_ACK, iBus_ERR))
      = (\(WishboneS2M a b c _ _) -> (a, b, c)) <$> iBusS2M

    (unbundle -> (dBus_DAT_MISO, dBus_ACK, dBus_ERR))
      = (\(WishboneS2M a b c _ _) -> (a, b, c)) <$> dBusS2M

    (unbundle -> (jtag_TCK, jtag_TMS, jtag_TDI))
      = bitCoerce <$> jtagInput

    sourcePath = $(do
          cpuSrcPath <- runIO $ getPackageRelFilePath "example-cpu/VexRiscv.v"
          pure $ LitE $ StringL cpuSrcPath
        )

    -- TODO: Remove need for 'vexRiscv#' by doing construction / deconstruction of
    --       product types in HDL using BlackBoxHaskell functions.
    ( iBus_CYC
      , iBus_STB
      , iBus_WE
      , iBus_ADR
      , iBus_DAT_MOSI
      , iBus_SEL
      , iBus_CTI
      , iBus_BTE
      , dBus_CYC
      , dBus_STB
      , dBus_WE
      , dBus_ADR
      , dBus_DAT_MOSI
      , dBus_SEL
      , dBus_CTI
      , dBus_BTE
      , ndmreset
      , stoptime
      , jtag_TDO
      ) = vexRiscv# dumpVcd sourcePath clk rst
          timerInterrupt
          externalInterrupt
          softwareInterrupt

          iBus_ACK
          iBus_ERR
          iBus_DAT_MISO

          dBus_ACK
          dBus_ERR
          dBus_DAT_MISO

          jtag_TCK
          jtag_TMS
          jtag_TDI


vexRiscv#
  :: forall dom .
  KnownDomain dom
  => DumpVcd
  -> String
  -> Clock dom
  -> Reset dom
  -- input signals
  -> Signal dom Bit  -- ^ timerInterrupt
  -> Signal dom Bit  -- ^ externalInterrupt
  -> Signal dom Bit  -- ^ softwareInterrupt
    -- iBusWbS2M
  -> Signal dom Bool           -- ^ iBus_ACK
  -> Signal dom Bool           -- ^ iBus_ERR
  -> Signal dom (BitVector 32) -- ^ iBus_DAT_MISO
    -- dBusWbS2M
  -> Signal dom Bool           -- ^ dBus_ACK
  -> Signal dom Bool           -- ^ dBus_ERR
  -> Signal dom (BitVector 32) -- ^ dBus_DAT_MISO

  -> Signal dom Bit -- ^ jtag_TCK
  -> Signal dom Bit -- ^ jtag_TMS
  -> Signal dom Bit -- ^ jtag_TDI


  -- output signals
  ->
    (
      -- iBus M2S
      Signal dom Bool           -- ^ iBus_CYC
    , Signal dom Bool           -- ^ iBus_STB
    , Signal dom Bool           -- ^ iBus_WE
    , Signal dom (BitVector 30) -- ^ iBus_ADR
    , Signal dom (BitVector 32) -- ^ iBus_DAT_MOSI
    , Signal dom (BitVector 4)  -- ^ iBus_SEL
    , Signal dom (BitVector 3)  -- ^ iBus_CTI
    , Signal dom (BitVector 2)  -- ^ iBus_BTE

    -- dBus M2S
    , Signal dom Bool           -- ^ dBus_CYC
    , Signal dom Bool           -- ^ dBus_STB
    , Signal dom Bool           -- ^ dBus_WE
    , Signal dom (BitVector 30) -- ^ dBus_ADR
    , Signal dom (BitVector 32) -- ^ dBus_DAT_MOSI
    , Signal dom (BitVector 4)  -- ^ dBus_SEL
    , Signal dom (BitVector 3)  -- ^ dBus_CTI
    , Signal dom (BitVector 2)  -- ^ dBus_BTE

    , Signal dom Bit -- ^ ndmreset
    , Signal dom Bit -- ^ stoptime
    , Signal dom Bit -- ^ jtag_TDO
    )
vexRiscv# dumpVcd !_sourcePath clk rst0
  timerInterrupt0
  externalInterrupt0
  softwareInterrupt0
  iBus_ACK0
  iBus_ERR0
  iBus_DAT_MISO0

  dBus_ACK0
  dBus_ERR0
  dBus_DAT_MISO0

  jtag_TCK0
  jtag_TMS0
  jtag_TDI0 = unsafePerformIO $ do
    -- PeriodToHz imposes a minimum period of 1 Hz, but the KnownDomain constraint does
    -- not supply this information.
    let domPeriodFs = hzToFs (natToNum @(PeriodToHz (Max 1 (DomainPeriod dom))))
    (v, initStage1, initStage2, stepRising, stepFalling, _shutDown) <- vexCPU dumpVcd domPeriodFs

    -- Make sure all the inputs are defined
    let
      rst1 = unsafeMakeDefinedRandom <$> unsafeToActiveHigh rst0
      timerInterrupt1 = unsafeMakeDefinedRandom <$> timerInterrupt0
      externalInterrupt1 = unsafeMakeDefinedRandom <$> externalInterrupt0
      softwareInterrupt1 = unsafeMakeDefinedRandom <$> softwareInterrupt0
      iBus_ACK1 = unsafeMakeDefinedRandom <$> iBus_ACK0
      iBus_DAT_MISO1 = unsafeMakeDefinedRandom <$> iBus_DAT_MISO0
      iBus_ERR1 = unsafeMakeDefinedRandom <$> iBus_ERR0
      dBus_ACK1 = unsafeMakeDefinedRandom <$> dBus_ACK0
      dBus_DAT_MISO1 = unsafeMakeDefinedRandom <$> dBus_DAT_MISO0
      dBus_ERR1 = unsafeMakeDefinedRandom <$> dBus_ERR0
      jtag_TCK1 = unsafeMakeDefinedRandom <$> jtag_TCK0
      jtag_TMS1 = unsafeMakeDefinedRandom <$> jtag_TMS0
      jtag_TDI1 = unsafeMakeDefinedRandom <$> jtag_TDI0

    let
      nonCombInput = NON_COMB_INPUT
        <$> (boolToBit <$> rst1)
        <*> timerInterrupt1
        <*> externalInterrupt1
        <*> softwareInterrupt1

      combInput = COMB_INPUT
        <$> (boolToBit <$> iBus_ACK1)
        <*> (unpack    <$> iBus_DAT_MISO1)
        <*> (boolToBit <$> iBus_ERR1)
        <*> (boolToBit <$> dBus_ACK1)
        <*> (unpack    <$> dBus_DAT_MISO1)
        <*> (boolToBit <$> dBus_ERR1)
        <*> jtag_TCK1
        <*> jtag_TMS1
        <*> jtag_TDI1

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

    let
      iBus_CYC      = FFI.iBusWishbone_CYC      <$> output
      iBus_STB      = FFI.iBusWishbone_STB      <$> output
      iBus_WE       = FFI.iBusWishbone_WE       <$> output
      iBus_ADR      = FFI.iBusWishbone_ADR      <$> output
      iBus_DAT_MOSI = FFI.iBusWishbone_DAT_MOSI <$> output
      iBus_SEL      = FFI.iBusWishbone_SEL      <$> output
      iBus_CTI      = FFI.iBusWishbone_CTI      <$> output
      iBus_BTE      = FFI.iBusWishbone_BTE      <$> output

      dBus_CYC      = FFI.dBusWishbone_CYC      <$> output
      dBus_STB      = FFI.dBusWishbone_STB      <$> output
      dBus_WE       = FFI.dBusWishbone_WE       <$> output
      dBus_ADR      = FFI.dBusWishbone_ADR      <$> output
      dBus_DAT_MOSI = FFI.dBusWishbone_DAT_MOSI <$> output
      dBus_SEL      = FFI.dBusWishbone_SEL      <$> output
      dBus_CTI      = FFI.dBusWishbone_CTI      <$> output
      dBus_BTE      = FFI.dBusWishbone_BTE      <$> output

    pure
      ( -- iBus
        bitToBool <$> iBus_CYC
      , bitToBool <$> iBus_STB
      , bitToBool <$> iBus_WE
      , truncateB . pack <$> iBus_ADR
      , pack <$> iBus_DAT_MOSI
      , truncateB . pack <$> iBus_SEL
      , truncateB . pack <$> iBus_CTI
      , truncateB . pack <$> iBus_BTE

      -- dBus
      , bitToBool <$> dBus_CYC
      , bitToBool <$> dBus_STB
      , bitToBool <$> dBus_WE
      , truncateB . pack <$> dBus_ADR
      , pack <$> dBus_DAT_MOSI
      , truncateB . pack <$> dBus_SEL
      , truncateB . pack <$> dBus_CTI
      , truncateB . pack <$> dBus_BTE

      -- JTAG
      , FFI.ndmreset <$> output
      , FFI.stoptime <$> output
      , FFI.jtag_TDO <$> output
      )
{-# CLASH_OPAQUE vexRiscv# #-}
{-# ANN vexRiscv# (
    let
      primName = 'vexRiscv#


      (
       -- ARGs
       _knownDomain
       , _vcdPath
       , srcPath
       , clk
       , rst
       , timerInterrupt
       , externalInterrupt
       , softwareInterrupt
       , iBus_ACK
       , iBus_ERR
       , iBus_DAT_MISO
       , dBus_ACK
       , dBus_ERR
       , dBus_DAT_MISO
       , jtag_TCK
       , jtag_TMS
       , jtag_TDI

       -- GENSYMs
       , iBus_CYC
       , iBus_STB
       , iBus_WE
       , iBus_ADR
       , iBus_DAT_MOSI
       , iBus_SEL
       , iBus_CTI
       , iBus_BTE
       , dBus_CYC
       , dBus_STB
       , dBus_WE
       , dBus_ADR
       , dBus_DAT_MOSI
       , dBus_SEL
       , dBus_CTI
       , dBus_BTE
       , ndmreset
       , stoptime
       , jtag_TDO

       , cpu
       ) = vecToTuple $ indicesI @37
    in
      InlineYamlPrimitive [Verilog] [__i|
  BlackBox:
    name: #{primName}
    kind: Declaration
    template: |-
      // vexRiscv begin

      ~DEVNULL[~FILE[~LIT[#{srcPath}]]]

      wire ~GENSYM[iBus_CYC][#{iBus_CYC}];
      wire ~GENSYM[iBus_STB][#{iBus_STB}];
      wire ~GENSYM[iBus_WE][#{iBus_WE}];
      wire [29:0] ~GENSYM[iBus_ADR][#{iBus_ADR}];
      wire [31:0] ~GENSYM[iBus_DAT_MOSI][#{iBus_DAT_MOSI}];
      wire [3:0] ~GENSYM[iBus_SEL][#{iBus_SEL}];
      wire [2:0] ~GENSYM[iBus_CTI][#{iBus_CTI}];
      wire [1:0] ~GENSYM[iBus_BTE][#{iBus_BTE}];

      wire ~GENSYM[dBus_CYC][#{dBus_CYC}];
      wire ~GENSYM[dBus_STB][#{dBus_STB}];
      wire ~GENSYM[dBus_WE][#{dBus_WE}];
      wire [29:0] ~GENSYM[dBus_ADR][#{dBus_ADR}];
      wire [31:0] ~GENSYM[dBus_DAT_MOSI][#{dBus_DAT_MOSI}];
      wire [3:0] ~GENSYM[dBus_SEL][#{dBus_SEL}];
      wire [2:0] ~GENSYM[dBus_CTI][#{dBus_CTI}];
      wire [1:0] ~GENSYM[dBus_BTE][#{dBus_BTE}];

      wire ~GENSYM[ndmreset][#{ndmreset}];
      wire ~GENSYM[stoptime][#{stoptime}];
      wire ~GENSYM[jtag_TDO][#{jtag_TDO}];

      VexRiscv ~GENSYM[cpu][#{cpu}] (
        .timerInterrupt    ( ~ARG[#{timerInterrupt}] ),
        .externalInterrupt ( ~ARG[#{externalInterrupt}] ),
        .softwareInterrupt ( ~ARG[#{softwareInterrupt}] ),

        .iBusWishbone_CYC      ( ~SYM[#{iBus_CYC}] ),
        .iBusWishbone_STB      ( ~SYM[#{iBus_STB}] ),
        .iBusWishbone_ACK      ( ~ARG[#{iBus_ACK}] ),
        .iBusWishbone_WE       ( ~SYM[#{iBus_WE}] ),
        .iBusWishbone_ADR      ( ~SYM[#{iBus_ADR}] ),
        .iBusWishbone_DAT_MISO ( ~ARG[#{iBus_DAT_MISO}] ),
        .iBusWishbone_DAT_MOSI ( ~SYM[#{iBus_DAT_MOSI}] ),
        .iBusWishbone_SEL      ( ~SYM[#{iBus_SEL}] ),
        .iBusWishbone_ERR      ( ~ARG[#{iBus_ERR}] ),
        .iBusWishbone_CTI      ( ~SYM[#{iBus_CTI}] ),
        .iBusWishbone_BTE      ( ~SYM[#{iBus_BTE}] ),

        .dBusWishbone_CYC      ( ~SYM[#{dBus_CYC}] ),
        .dBusWishbone_STB      ( ~SYM[#{dBus_STB}] ),
        .dBusWishbone_ACK      ( ~ARG[#{dBus_ACK}] ),
        .dBusWishbone_WE       ( ~SYM[#{dBus_WE}] ),
        .dBusWishbone_ADR      ( ~SYM[#{dBus_ADR}] ),
        .dBusWishbone_DAT_MISO ( ~ARG[#{dBus_DAT_MISO}] ),
        .dBusWishbone_DAT_MOSI ( ~SYM[#{dBus_DAT_MOSI}] ),
        .dBusWishbone_SEL      ( ~SYM[#{dBus_SEL}] ),
        .dBusWishbone_ERR      ( ~ARG[#{dBus_ERR}] ),
        .dBusWishbone_CTI      ( ~SYM[#{dBus_CTI}] ),
        .dBusWishbone_BTE      ( ~SYM[#{dBus_BTE}] ),

        .jtag_tms       ( ~ARG[#{jtag_TMS}]),
        .jtag_tdi       ( ~ARG[#{jtag_TDI}]),
        .jtag_tck       ( ~ARG[#{jtag_TCK}]),
        .jtag_tdo       ( ~SYM[#{jtag_TDO}] ),

        .ndmreset ( ~SYM[#{ndmreset}] ),
        .stoptime ( ~SYM[#{stoptime}] ),

        .clk   ( ~ARG[#{clk}] ),
        .reset ( ~ARG[#{rst}] )
      );

      assign ~RESULT = {
        ~SYM[#{iBus_CYC}],
        ~SYM[#{iBus_STB}],
        ~SYM[#{iBus_WE}],
        ~SYM[#{iBus_ADR}],
        ~SYM[#{iBus_DAT_MOSI}],
        ~SYM[#{iBus_SEL}],
        ~SYM[#{iBus_CTI}],
        ~SYM[#{iBus_BTE}],
        ~SYM[#{dBus_CYC}],
        ~SYM[#{dBus_STB}],
        ~SYM[#{dBus_WE}],
        ~SYM[#{dBus_ADR}],
        ~SYM[#{dBus_DAT_MOSI}],
        ~SYM[#{dBus_SEL}],
        ~SYM[#{dBus_CTI}],
        ~SYM[#{dBus_BTE}],
        ~SYM[#{ndmreset}],
        ~SYM[#{stoptime}],
        ~SYM[#{jtag_TDO}]
      };

      // vexRiscv end

    |] ) #-}



-- | Return a function that performs an execution step and a function to free
-- the internal CPU state
vexCPU ::
  DumpVcd ->
  Femtoseconds ->
  IO
    ( Ptr VexRiscv
    , Ptr VexRiscv -> NON_COMB_INPUT -> IO OUTPUT           -- initStage1
    , Ptr VexRiscv -> COMB_INPUT -> IO ()                   -- initStage2
    , Ptr VexRiscv -> Word64 -> NON_COMB_INPUT -> IO OUTPUT -- rising
    , Ptr VexRiscv -> Word64 -> COMB_INPUT -> IO ()         -- falling
    , Ptr VexRiscv -> IO ()
    )
vexCPU dumpVcd (fromIntegral . unFemtoseconds -> domPeriodFs :: Word64)  = do
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
