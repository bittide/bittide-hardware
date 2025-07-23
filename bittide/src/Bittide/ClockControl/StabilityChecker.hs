-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Bittide.ClockControl.StabilityChecker where

import Clash.Prelude

-- | Stability results to be returned by the 'stabilityChecker'.
data StabilityIndication = StabilityIndication
  { stable :: Bool
  -- ^ Indicates stability of the signal over time.
  , settled :: Bool
  -- ^ Indicates whether the signal is stable and close to
  -- 'targetDataCount'.
  }
  deriving (Generic, NFDataX, BitPack, ShowX, Show)
