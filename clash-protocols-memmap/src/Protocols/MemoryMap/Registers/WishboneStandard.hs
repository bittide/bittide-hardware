-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{- | Utilities for creating Wishbone devices and registers, while also creating
a memory map.
-}
module Protocols.MemoryMap.Registers.WishboneStandard (
  -- * Device creation
  deviceWb,
  deviceWithOffsetsWb,

  -- * Register creation (explicit clocks and resets)
  registerWb,
  registerWb_,
  registerWbDf,
  registerWithOffsetWb,
  registerWithOffsetWbDf,

  -- * Register creation (implicit clocks and resets)
  registerWbI,
  registerWbI_,
  registerWbDfI,
  registerWithOffsetWbI,
  registerWithOffsetWbDfI,

  -- * Supporting types and functions
  registerConfig,
  busActivityWrite,
  BusReadBehavior (..),
  BusActivity (..),
  DeviceConfig (..),
  RegisterConfig (..),
) where

import Protocols.MemoryMap.Registers.WishboneStandard.Internal
