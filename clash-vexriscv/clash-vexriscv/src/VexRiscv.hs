-- SPDX-FileCopyrightText: 2022-2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE QuasiQuotes #-}

module VexRiscv where

import Clash.Prelude

import Clash.Annotations.Primitive
import Clash.Signal.Internal
import Data.String.Interpolate (__i)
import Foreign.Marshal (alloca)
import Foreign.Storable
import GHC.IO (unsafePerformIO)
import GHC.Stack (HasCallStack)
import Protocols.Wishbone
import VexRiscv.FFI
import VexRiscv.TH
import qualified Data.List
import Language.Haskell.TH.Syntax


data Input = Input
  { timerInterrupt :: Bit
  , externalInterrupt :: Bit
  , softwareInterrupt :: Bit
  , iBusWbS2M :: WishboneS2M (BitVector 32)
  , dBusWbS2M :: WishboneS2M (BitVector 32)
  }
  deriving (Generic, NFDataX, ShowX, Eq, BitPack)

data Output = Output
  { iBusWbM2S :: WishboneM2S 30 4 (BitVector 32)
  , dBusWbM2S :: WishboneM2S 30 4 (BitVector 32)
  }
  deriving (Generic, NFDataX, ShowX, Eq)

inputToFFI :: Bool -> Input -> INPUT
inputToFFI reset Input {..} =
  INPUT
    { reset = boolToBit reset
    , timerInterrupt
    , externalInterrupt
    , softwareInterrupt
    , iBusWishbone_ACK = boolToBit $ acknowledge iBusWbS2M
    , iBusWishbone_DAT_MISO = unpack $ readData iBusWbS2M
    , iBusWishbone_ERR = boolToBit $ err iBusWbS2M
    , dBusWishbone_ACK = boolToBit $ acknowledge dBusWbS2M
    , dBusWishbone_DAT_MISO = unpack $ readData dBusWbS2M
    , dBusWishbone_ERR = boolToBit $ err dBusWbS2M
    }

outputFromFFI :: OUTPUT -> Output
outputFromFFI OUTPUT {..} =
  Output
    { iBusWbM2S =
        (emptyWishboneM2S @30 @(BitVector 32))
          { busCycle = bitToBool iBusWishbone_CYC,
            strobe = bitToBool iBusWishbone_STB,
            writeEnable = bitToBool iBusWishbone_WE,
            addr = truncateB $ pack iBusWishbone_ADR,
            writeData = pack iBusWishbone_DAT_MOSI,
            busSelect = resize $ pack iBusWishbone_SEL,
            cycleTypeIdentifier = unpack $ resize $ pack iBusWishbone_CTI,
            burstTypeExtension = unpack $ resize $ pack iBusWishbone_BTE
          },
      dBusWbM2S =
        (emptyWishboneM2S @30 @(BitVector 32))
          { busCycle = bitToBool dBusWishbone_CYC,
            strobe = bitToBool dBusWishbone_STB,
            writeEnable = bitToBool dBusWishbone_WE,
            addr = truncateB $ pack dBusWishbone_ADR,
            writeData = pack dBusWishbone_DAT_MOSI,
            busSelect = resize $ pack dBusWishbone_SEL,
            cycleTypeIdentifier = unpack $ resize $ pack dBusWishbone_CTI,
            burstTypeExtension = unpack $ resize $ pack dBusWishbone_BTE
          }
    }

vexRiscv :: (HasCallStack, HiddenClockResetEnable dom) => Signal dom Input -> Signal dom Output
vexRiscv input =
  Output <$>
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

  where
    -- When passing S2M values from Haskell to VexRiscv over the FFI, undefined
    -- bits/values cause errors when forcing their evaluation to something that can
    -- be passed through the FFI.
    --
    -- This function makes sure the Wishbone S2M values are free from undefined bits.
    makeDefined :: WishboneS2M (BitVector 32) -> WishboneS2M (BitVector 32)
    makeDefined wb = wb {readData = defaultX 0 (readData wb)}

    defaultX :: (NFDataX a) => a -> a -> a
    defaultX dflt val
      | hasUndefined val = dflt
      | otherwise = val

    (unbundle -> (timerInterrupt, externalInterrupt, softwareInterrupt, iBusS2M, dBusS2M))
      = (<$> ( unpack 0 :- input)) $ \(Input a b c d e) -> (a, b, c, d, e)

    (unbundle -> (iBus_DAT_MISO, iBus_ACK, iBus_ERR))
      = (<$> iBusS2M) $ (\(WishboneS2M a b c _ _) -> (a, b, c)) . makeDefined

    (unbundle -> (dBus_DAT_MISO, dBus_ACK, dBus_ERR))
      = (<$> dBusS2M) $ (\(WishboneS2M a b c _ _) -> (a, b, c)) . makeDefined

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
      ) = vexRiscv# hasClock hasReset
          timerInterrupt
          externalInterrupt
          softwareInterrupt

          iBus_ACK
          iBus_ERR
          iBus_DAT_MISO

          dBus_ACK
          dBus_ERR
          dBus_DAT_MISO





vexRiscv#
  :: KnownDomain dom
  => Clock dom
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
    )
vexRiscv# !_clk rst0
  timerInterrupt
  externalInterrupt
  softwareInterrupt
  iBus_ACK
  iBus_ERR
  iBus_DAT_MISO

  dBus_ACK
  dBus_ERR
  dBus_DAT_MISO

  =
    let
      iBusS2M = WishboneS2M <$> iBus_DAT_MISO <*> iBus_ACK <*> iBus_ERR <*> pure False <*> pure False
      dBusS2M = WishboneS2M <$> dBus_DAT_MISO <*> dBus_ACK <*> dBus_ERR <*> pure False <*> pure False

      input = Input <$> timerInterrupt <*> externalInterrupt <*> softwareInterrupt <*> iBusS2M <*> dBusS2M

      output = unsafePerformIO $ do
        (step, _) <- vexCPU
        pure $ go step (unsafeFromReset rst0) input

      (unbundle -> (iBusM2S, dBusM2S)) = (<$> output) $ \(Output iBus dBus) -> (iBus, dBus)

      (unbundle -> (iBus_ADR, iBus_DAT_MOSI, iBus_SEL, iBus_CYC, iBus_STB, iBus_WE, iBus_CTI, iBus_BTE)) =
        (<$> iBusM2S) $ \(WishboneM2S a b c _ e f g h i) -> (a, b, c, e, f, g, h, i)

      (unbundle -> (dBus_ADR, dBus_DAT_MOSI, dBus_SEL, dBus_CYC, dBus_STB, dBus_WE, dBus_CTI, dBus_BTE)) =
        (<$> dBusM2S) $ \(WishboneM2S a b c _ e f g h i) -> (a, b, c, e, f, g, h, i)
    in
      ( -- iBus
        iBus_CYC
      , iBus_STB
      , iBus_WE
      , iBus_ADR
      , iBus_DAT_MOSI
      , iBus_SEL
      , pack <$> iBus_CTI
      , pack <$> iBus_BTE

      -- dBus
      , dBus_CYC
      , dBus_STB
      , dBus_WE
      , dBus_ADR
      , dBus_DAT_MOSI
      , dBus_SEL
      , pack <$> dBus_CTI
      , pack <$> dBus_BTE
      )
  where
    {-# NOINLINE go #-}
    go step (rst :- rsts) (input :- inputs) = unsafePerformIO $ do
      out <- step rst input
      pure $ out :- go step rsts inputs
{-# NOINLINE vexRiscv# #-}
{-# ANN vexRiscv# (
    let
      primName = 'vexRiscv#
      ( _
       :> clk
       :> rst
       :> timerInterrupt
       :> externalInterrupt
       :> softwareInterrupt
       :> iBus_ACK
       :> iBus_ERR
       :> iBus_DAT_MISO
       :> dBus_ACK
       :> dBus_ERR
       :> dBus_DAT_MISO
       :> Nil
       ) = indicesI @12

      ( iBus_CYC
       :> iBus_STB
       :> iBus_WE
       :> iBus_ADR
       :> iBus_DAT_MOSI
       :> iBus_SEL
       :> iBus_CTI
       :> iBus_BTE
       :> dBus_CYC
       :> dBus_STB
       :> dBus_WE
       :> dBus_ADR
       :> dBus_DAT_MOSI
       :> dBus_SEL
       :> dBus_CTI
       :> dBus_BTE
       :> Nil
       ) = (\x -> extend @_ @16 @12 x + 1) <$> indicesI @16

      cpu = extend @_ @_ @1 dBus_BTE + 1

      indent n str =
        let
          l0 = lines str
          indentation = Data.List.replicate n ' '
          l1 = (indentation <>) <$> l0
        in unlines l1

      vexRiscvSrc0 = $(do
        cpuSrcPath <- runIO $ getPackageRelFilePath "example-cpu/VexRiscv.v"
        content <- runIO $ readFile cpuSrcPath
        pure $ LitE $ StringL content
        )
      vexRiscvSrc1 = indent 6 vexRiscvSrc0
    in
      InlineYamlPrimitive [Verilog] [__i|
  BlackBox:
    name: #{primName}
    kind: Declaration
    includes:
    - name: vexRiscvSource
      extension: v
      template: |-
        `default_nettype wire
#{vexRiscvSrc1}
        `default_nettype none
    template: |-
      // vexRiscv begin

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
        ~SYM[#{dBus_BTE}]
      };

      // vexRiscv end

    |] ) #-}


-- | Return a function that performs an execution step and a function to free
-- the internal CPU state
vexCPU :: IO (Bool -> Input -> IO Output, IO ())
vexCPU = do
  v <- vexrInit
  let
    step reset input = alloca $ \inputFFI -> alloca $ \outputFFI -> do
      poke inputFFI (inputToFFI reset input)
      vexrStep v inputFFI outputFFI
      outVal <- peek outputFFI
      pure $ outputFromFFI outVal
    shutDown = vexrShutdown v
  pure (step, shutDown)
