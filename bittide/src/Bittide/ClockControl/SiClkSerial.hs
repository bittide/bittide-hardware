-- SPDX-FileCopyrightText: 2022-2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -fconstraint-solver-iterations=15 #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Bittide.ClockControl.SiClkSerial where

import Clash.Prelude

import Bittide.SharedTypes


-- | The Silicon Labs chips use "Page"s to increase their address space.
type Page = Byte
-- | Different memory location depending on the current 'Page'.
type Address = Byte
-- | Indicates that the interface producing this is currently Busy and will not respond to inputs.
type Busy = Bool
-- | Indicates that the interface producing this value has captured the input.
type Acknowledge = Bool

-- | A Silicon Labs register entry consists of a 'Page', and 'Address' and a 'Byte' value.
type RegisterEntry = (Page, Address, Byte)

-- | Used to read from or write to a register on a Silicon Labs chip via Serial.
data RegisterOperation = RegisterOperation
  { regPage     :: Page
   -- ^ Page at which to perform the read or write.
  , regAddress  :: Address
   -- ^ Address at which to perform the read or write
  , regWrite    :: Maybe Byte
   -- ^ @Nothing@ for a read operation, @Just byte@ to write @byte@ to this 'Page' and 'Address'.
  } deriving (Show, Generic, NFDataX)

-- | Contains the configuration for a Silicon Labs chip, explicitly differentiates between
-- the configuration preamble, configuration and configuration postamble.
data SiLabsRegisterMap preambleEntries configEntries postambleEntries = SiLabsRegisterMap
  { configPreamble :: Vec preambleEntries RegisterEntry
  -- ^ Configuration preamble
  , config :: Vec configEntries RegisterEntry
  -- ^ Configuration
  , configPostamble :: Vec postambleEntries RegisterEntry
  -- ^ Configuration postamble
  }

-- | Operations supported by the SiLabs chip, @BurstWrite@ is omitted because the
-- current Serial core does not support writing a variable number of bytes while slave select
-- is low.
data SerialCommand
  = SetAddress Address
  -- ^ Sets the selected register 'Address' on the SiLabs chip.
  | WriteData Byte
  -- ^ Writes data to the selected 'Address' on the selected 'Page'.
  | ReadData
  -- ^ Reads data from the selected 'Address' on the selected 'Page'.
  | WriteDataInc Byte
  -- ^ Writes data to the selected 'Address' on the selected 'Page' and increments the 'Address'.
  | ReadDataInc
  -- ^ Reads data from the selected 'Address' on the selected 'Page' and increments the 'Address'.
  deriving Eq
