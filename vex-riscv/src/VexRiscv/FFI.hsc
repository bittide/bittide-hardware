-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module VexRiscv.FFI where

import Foreign.Storable
import Foreign.Ptr
import Prelude
import Clash.Prelude
import Data.Word

#include "ffi/interface.h"

data VexRiscv

foreign import ccall unsafe "vexr_init" vexrInit :: IO (Ptr VexRiscv)

foreign import ccall unsafe "vexr_shutdown" vexrShutdown :: Ptr VexRiscv -> IO ()

foreign import ccall unsafe "vexr_step" vexrStep :: Ptr VexRiscv -> Ptr INPUT -> Ptr OUTPUT -> IO ()

data INPUT = INPUT
  { reset :: Bit
  , timerInterrupt :: Bit
  , externalInterrupt :: Bit
  , softwareInterrupt :: Bit
  , iBusWishbone_ACK :: Bit
  , iBusWishbone_DAT_MISO :: Word32
  , iBusWishbone_ERR :: Bit
  , dBusWishbone_ACK :: Bit
  , dBusWishbone_DAT_MISO :: Word32
  , dBusWishbone_ERR :: Bit
  }
  deriving (Show)

data OUTPUT = OUTPUT
  { iBusWishbone_CYC :: Bit
  , iBusWishbone_STB :: Bit
  , iBusWishbone_WE :: Bit
  , iBusWishbone_ADR :: Word32
  , iBusWishbone_DAT_MOSI :: Word32
  , iBusWishbone_SEL :: Word8
  , iBusWishbone_CTI :: Word8
  , iBusWishbone_BTE :: Word8
  , dBusWishbone_CYC :: Bit
  , dBusWishbone_STB :: Bit
  , dBusWishbone_WE :: Bit
  , dBusWishbone_ADR :: Word32
  , dBusWishbone_DAT_MOSI :: Word32
  , dBusWishbone_SEL :: Word8
  , dBusWishbone_CTI :: Word8
  , dBusWishbone_BTE :: Word8
  }
  deriving (Show)

instance Storable Bit where
    alignment = alignment . bitToBool
    sizeOf = sizeOf . bitToBool
    peek = fmap boolToBit . peek . castPtr
    poke ptr = poke (castPtr ptr) . bitToBool

instance Storable INPUT where
    alignment _ = #alignment INPUT
    sizeOf _ = #size INPUT
    {-# INLINE peek #-}
    peek ptr = const INPUT <$> pure ()
      <*> (#peek INPUT, reset) ptr
      <*> (#peek INPUT, timerInterrupt) ptr
      <*> (#peek INPUT, externalInterrupt) ptr
      <*> (#peek INPUT, softwareInterrupt) ptr
      <*> (#peek INPUT, iBusWishbone_ACK) ptr
      <*> (#peek INPUT, iBusWishbone_DAT_MISO) ptr
      <*> (#peek INPUT, iBusWishbone_ERR) ptr
      <*> (#peek INPUT, dBusWishbone_ACK) ptr
      <*> (#peek INPUT, dBusWishbone_DAT_MISO) ptr
      <*> (#peek INPUT, dBusWishbone_ERR) ptr

    {-# INLINE poke #-}
    poke ptr this = do
      (#poke INPUT, reset) ptr (reset this)
      (#poke INPUT, timerInterrupt) ptr (timerInterrupt this)
      (#poke INPUT, externalInterrupt) ptr (externalInterrupt this)
      (#poke INPUT, softwareInterrupt) ptr (softwareInterrupt this)
      (#poke INPUT, iBusWishbone_ACK) ptr (iBusWishbone_ACK this)
      (#poke INPUT, iBusWishbone_DAT_MISO) ptr (iBusWishbone_DAT_MISO this)
      (#poke INPUT, iBusWishbone_ERR) ptr (iBusWishbone_ERR this)
      (#poke INPUT, dBusWishbone_ACK) ptr (dBusWishbone_ACK this)
      (#poke INPUT, dBusWishbone_DAT_MISO) ptr (dBusWishbone_DAT_MISO this)
      (#poke INPUT, dBusWishbone_ERR) ptr (dBusWishbone_ERR this)
      return ()

instance Storable OUTPUT where
    alignment _ = #alignment OUTPUT
    sizeOf _ = #size OUTPUT
    {-# INLINE peek #-}
    peek ptr = const OUTPUT <$> pure ()
      <*> (#peek OUTPUT, iBusWishbone_CYC) ptr
      <*> (#peek OUTPUT, iBusWishbone_STB) ptr
      <*> (#peek OUTPUT, iBusWishbone_WE) ptr
      <*> (#peek OUTPUT, iBusWishbone_ADR) ptr
      <*> (#peek OUTPUT, iBusWishbone_DAT_MOSI) ptr
      <*> (#peek OUTPUT, iBusWishbone_SEL) ptr
      <*> (#peek OUTPUT, iBusWishbone_CTI) ptr
      <*> (#peek OUTPUT, iBusWishbone_BTE) ptr
      <*> (#peek OUTPUT, dBusWishbone_CYC) ptr
      <*> (#peek OUTPUT, dBusWishbone_STB) ptr
      <*> (#peek OUTPUT, dBusWishbone_WE) ptr
      <*> (#peek OUTPUT, dBusWishbone_ADR) ptr
      <*> (#peek OUTPUT, dBusWishbone_DAT_MOSI) ptr
      <*> (#peek OUTPUT, dBusWishbone_SEL) ptr
      <*> (#peek OUTPUT, dBusWishbone_CTI) ptr
      <*> (#peek OUTPUT, dBusWishbone_BTE) ptr

    {-# INLINE poke #-}
    poke ptr this = do
      (#poke OUTPUT, iBusWishbone_CYC) ptr (iBusWishbone_CYC this)
      (#poke OUTPUT, iBusWishbone_STB) ptr (iBusWishbone_STB this)
      (#poke OUTPUT, iBusWishbone_WE) ptr (iBusWishbone_WE this)
      (#poke OUTPUT, iBusWishbone_ADR) ptr (iBusWishbone_ADR this)
      (#poke OUTPUT, iBusWishbone_DAT_MOSI) ptr (iBusWishbone_DAT_MOSI this)
      (#poke OUTPUT, iBusWishbone_SEL) ptr (iBusWishbone_SEL this)
      (#poke OUTPUT, iBusWishbone_CTI) ptr (iBusWishbone_CTI this)
      (#poke OUTPUT, iBusWishbone_BTE) ptr (iBusWishbone_BTE this)
      (#poke OUTPUT, dBusWishbone_CYC) ptr (dBusWishbone_CYC this)
      (#poke OUTPUT, dBusWishbone_STB) ptr (dBusWishbone_STB this)
      (#poke OUTPUT, dBusWishbone_WE) ptr (dBusWishbone_WE this)
      (#poke OUTPUT, dBusWishbone_ADR) ptr (dBusWishbone_ADR this)
      (#poke OUTPUT, dBusWishbone_DAT_MOSI) ptr (dBusWishbone_DAT_MOSI this)
      (#poke OUTPUT, dBusWishbone_SEL) ptr (dBusWishbone_SEL this)
      (#poke OUTPUT, dBusWishbone_CTI) ptr (dBusWishbone_CTI this)
      (#poke OUTPUT, dBusWishbone_BTE) ptr (dBusWishbone_BTE this)
      return ()
