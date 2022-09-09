-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module VexRiscv where

import Clash.Prelude
import Clash.Signal.Internal
import Foreign.Marshal (alloca)
import Foreign.Storable
import GHC.IO (unsafePerformIO)
import GHC.Stack (HasCallStack)
import Protocols.Wishbone
import VexRiscv.FFI

data Input = Input
  { timerInterrupt :: Bit
  , externalInterrupt :: Bit
  , softwareInterrupt :: Bit
  , iBusWbS2M :: WishboneS2M (BitVector 32)
  , dBusWbS2M :: WishboneS2M (BitVector 32)
  }
  deriving (Generic, NFDataX, ShowX, Eq)

data Output = Output
  { iBusWbM2S :: WishboneM2S 32 4 (BitVector 32)
  , dBusWbM2S :: WishboneM2S 32 4 (BitVector 32)
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
        (emptyWishboneM2S @32 @(BitVector 32))
          { busCycle = bitToBool iBusWishbone_CYC,
            strobe = bitToBool iBusWishbone_STB,
            writeEnable = bitToBool iBusWishbone_WE,
            addr = pack iBusWishbone_ADR,
            writeData = pack iBusWishbone_DAT_MOSI,
            busSelect = resize $ pack iBusWishbone_SEL,
            cycleTypeIdentifier = unpack $ resize $ pack iBusWishbone_CTI,
            burstTypeExtension = unpack $ resize $ pack iBusWishbone_BTE
          },
      dBusWbM2S =
        (emptyWishboneM2S @32 @(BitVector 32))
          { busCycle = bitToBool dBusWishbone_CYC,
            strobe = bitToBool dBusWishbone_STB,
            writeEnable = bitToBool dBusWishbone_WE,
            addr = pack dBusWishbone_ADR,
            writeData = pack dBusWishbone_DAT_MOSI,
            busSelect = resize $ pack dBusWishbone_SEL,
            cycleTypeIdentifier = unpack $ resize $ pack dBusWishbone_CTI,
            burstTypeExtension = unpack $ resize $ pack dBusWishbone_BTE
          }
    }

{-# NOINLINE vexRiscv #-}
vexRiscv :: (HasCallStack, HiddenReset dom) => Signal dom Input -> Signal dom Output
vexRiscv = unsafePerformIO $ do
  (step, _) <- vexCPU
  pure $ go step (unsafeFromReset hasReset)
  where
    {-# NOINLINE go #-}
    go step (rst :- rsts) (input :- inputs) = unsafePerformIO $ do
      out <- step rst input
      pure $ out :- go step rsts inputs


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
