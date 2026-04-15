-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Protocols.Spi where

import Clash.Prelude
import Protocols

import qualified Clash.Shockwaves as Shock

data Spi (dom :: Domain)

-- | SPI data that flows from the manager to the subordinate.
data M2S = M2S
  { sclk :: "SCLK" ::: Bool
  -- ^ Clock signal
  , mosi :: "MOSI" ::: Bit
  -- ^ Serial data output from the manager to the subordinate
  , cs :: "CS" ::: Bool
  -- ^ Active-low chip select signal from manager to enable communication with a
  -- specific subordinate device
  }
  deriving (Show, ShowX, BitPack, Shock.Waveform, Generic, NFDataX, Eq)

data S2M = S2M
  { miso :: "MISO" ::: Bit
  -- ^ Serial data output from the subordinate to the manager
  }
  deriving (Show, ShowX, BitPack, Shock.Waveform, Generic, NFDataX, Eq)

instance Protocol (Spi dom) where
  type Fwd (Spi dom) = Signal dom M2S
  type Bwd (Spi dom) = Signal dom S2M
