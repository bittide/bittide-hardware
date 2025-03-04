-- SPDX-FileCopyrightText: 2022-2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module VexRiscv.FFI where

import Foreign.Storable
import Foreign.Ptr
import Foreign.C (CString)
import Prelude
import Clash.Prelude
import Data.Word

#include "ffi/interface.h"

data VexRiscv

data VerilatedVcdC

data VexRiscvJtagBridge

foreign import ccall unsafe "vexr_init" vexrInit :: IO (Ptr VexRiscv)
foreign import ccall unsafe "vexr_init_vcd" vexrInitVcd :: Ptr VexRiscv -> CString -> IO (Ptr VerilatedVcdC)
foreign import ccall unsafe "vexr_shutdown" vexrShutdown :: Ptr VexRiscv -> IO ()

foreign import ccall unsafe "vexr_init_stage1" vexrInitStage1 :: Ptr VerilatedVcdC -> Word64 -> Ptr VexRiscv -> Ptr NON_COMB_INPUT -> Ptr OUTPUT -> IO ()
foreign import ccall unsafe "vexr_init_stage2" vexrInitStage2 :: Ptr VexRiscv -> Ptr COMB_INPUT -> IO ()
foreign import ccall unsafe "vexr_step_rising_edge" vexrStepRisingEdge :: Ptr VerilatedVcdC -> Ptr VexRiscv -> Word64 -> Ptr NON_COMB_INPUT -> Ptr OUTPUT -> IO ()
foreign import ccall unsafe "vexr_step_falling_edge" vexrStepFallingEdge :: Ptr VerilatedVcdC -> Ptr VexRiscv -> Word64 -> Ptr COMB_INPUT -> IO ()

foreign import ccall unsafe "vexr_jtag_bridge_init" vexrJtagBridgeInit :: Word16 -> IO (Ptr VexRiscvJtagBridge)
foreign import ccall unsafe "vexr_jtag_bridge_step" vexrJtagBridgeStep :: Ptr VexRiscvJtagBridge -> Ptr JTAG_OUTPUT -> Ptr JTAG_INPUT -> IO ()
foreign import ccall unsafe "vexr_jtag_bridge_shutdown" vexrJtagBridgeShutdown :: Ptr VexRiscvJtagBridge -> IO ()

-- | CPU input that cannot combinatorially depend on the CPU output
data NON_COMB_INPUT = NON_COMB_INPUT
  { reset :: Bit
  , timerInterrupt :: Bit
  , externalInterrupt :: Bit
  , softwareInterrupt :: Bit
  }

-- | CPU input that can combinatorially depend on the CPU output
data COMB_INPUT = COMB_INPUT
  { iBusWishbone_ACK :: Bit
  , iBusWishbone_DAT_MISO :: Word32
  , iBusWishbone_ERR :: Bit

  , dBusWishbone_ACK :: Bit
  , dBusWishbone_DAT_MISO :: Word32
  , dBusWishbone_ERR :: Bit

  , jtag_TCK :: Bit
  , jtag_TMS :: Bit
  , jtag_TDI :: Bit
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

  , ndmreset :: Bit
  , stoptime :: Bit

  , jtag_TDO :: Bit
  }
  deriving (Show)

data JTAG_INPUT = JTAG_INPUT
  { tck :: Bit
  , tms :: Bit
  , tdi :: Bit
  }
  deriving (Show)

data JTAG_OUTPUT = JTAG_OUTPUT
  { tdo :: Bit
  }
  deriving (Show)

instance Storable Bit where
    alignment = alignment . bitToBool
    sizeOf = sizeOf . bitToBool
    peek = fmap boolToBit . peek . castPtr
    poke ptr = poke (castPtr ptr) . bitToBool

instance Storable NON_COMB_INPUT where
    alignment _ = #alignment NON_COMB_INPUT
    sizeOf _ = #size NON_COMB_INPUT
    {-# INLINE peek #-}
    peek ptr = const NON_COMB_INPUT <$> pure ()
      <*> (#peek NON_COMB_INPUT, reset) ptr
      <*> (#peek NON_COMB_INPUT, timerInterrupt) ptr
      <*> (#peek NON_COMB_INPUT, externalInterrupt) ptr
      <*> (#peek NON_COMB_INPUT, softwareInterrupt) ptr

    {-# INLINE poke #-}
    poke ptr this = do
      (#poke NON_COMB_INPUT, reset) ptr (reset this)
      (#poke NON_COMB_INPUT, timerInterrupt) ptr (timerInterrupt this)
      (#poke NON_COMB_INPUT, externalInterrupt) ptr (externalInterrupt this)
      (#poke NON_COMB_INPUT, softwareInterrupt) ptr (softwareInterrupt this)
      return ()

instance Storable COMB_INPUT where
    alignment _ = #alignment COMB_INPUT
    sizeOf _ = #size COMB_INPUT
    {-# INLINE peek #-}
    peek ptr = const COMB_INPUT <$> pure ()
      <*> (#peek COMB_INPUT, iBusWishbone_ACK) ptr
      <*> (#peek COMB_INPUT, iBusWishbone_DAT_MISO) ptr
      <*> (#peek COMB_INPUT, iBusWishbone_ERR) ptr
      <*> (#peek COMB_INPUT, dBusWishbone_ACK) ptr
      <*> (#peek COMB_INPUT, dBusWishbone_DAT_MISO) ptr
      <*> (#peek COMB_INPUT, dBusWishbone_ERR) ptr
      <*> (#peek COMB_INPUT, jtag_TCK) ptr
      <*> (#peek COMB_INPUT, jtag_TMS) ptr
      <*> (#peek COMB_INPUT, jtag_TDI) ptr

    {-# INLINE poke #-}
    poke ptr this = do
      (#poke COMB_INPUT, iBusWishbone_ACK) ptr (iBusWishbone_ACK this)
      (#poke COMB_INPUT, iBusWishbone_DAT_MISO) ptr (iBusWishbone_DAT_MISO this)
      (#poke COMB_INPUT, iBusWishbone_ERR) ptr (iBusWishbone_ERR this)

      (#poke COMB_INPUT, dBusWishbone_ACK) ptr (dBusWishbone_ACK this)
      (#poke COMB_INPUT, dBusWishbone_DAT_MISO) ptr (dBusWishbone_DAT_MISO this)
      (#poke COMB_INPUT, dBusWishbone_ERR) ptr (dBusWishbone_ERR this)

      (#poke COMB_INPUT, jtag_TCK) ptr (jtag_TCK this)
      (#poke COMB_INPUT, jtag_TMS) ptr (jtag_TMS this)
      (#poke COMB_INPUT, jtag_TDI) ptr (jtag_TDI this)
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

      <*> (#peek OUTPUT, ndmreset) ptr
      <*> (#peek OUTPUT, stoptime) ptr

      <*> (#peek OUTPUT, jtag_TDO) ptr

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

      (#poke OUTPUT, ndmreset) ptr (ndmreset this)
      (#poke OUTPUT, stoptime) ptr (stoptime this)

      (#poke OUTPUT, jtag_TDO) ptr (jtag_TDO this)
      return ()

instance Storable JTAG_OUTPUT where
    alignment _ = #alignment JTAG_OUTPUT
    sizeOf _ = #size JTAG_OUTPUT
    {-# INLINE peek #-}
    peek ptr = const JTAG_OUTPUT <$> pure ()
      <*> (#peek JTAG_OUTPUT, tdo) ptr

    {-# INLINE poke #-}
    poke ptr this = do
      (#poke JTAG_OUTPUT, tdo) ptr (tdo this)

instance Storable JTAG_INPUT where
    alignment _ = #alignment JTAG_INPUT
    sizeOf _ = #size JTAG_INPUT
    {-# INLINE peek #-}
    peek ptr = const JTAG_INPUT <$> pure ()
      <*> (#peek JTAG_INPUT, tck) ptr
      <*> (#peek JTAG_INPUT, tms) ptr
      <*> (#peek JTAG_INPUT, tdi) ptr

    {-# INLINE poke #-}
    poke ptr this = do
      (#poke JTAG_INPUT, tck) ptr (tck this)
      (#poke JTAG_INPUT, tms) ptr (tms this)
      (#poke JTAG_INPUT, tdi) ptr (tdi this)
      return ()
