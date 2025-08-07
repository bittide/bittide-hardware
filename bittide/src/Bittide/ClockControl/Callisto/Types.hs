-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=20 #-}

module Bittide.ClockControl.Callisto.Types (
  CallistoResult (..),
  ReframingState (..),
  Stability (..),
) where

import Clash.Prelude

import Bittide.ClockControl
import Clash.Class.BitPackC (BitPackC)
import Protocols.MemoryMap.FieldType (ToFieldType)

-- | Stability results to be returned by the 'stability_detector'.
data Stability = Stability
  { stable :: Bool
  -- ^ Indicates stability of the signal over time.
  , settled :: Bool
  -- ^ Indicates whether the signal is stable and close to
  -- 'targetDataCount'.
  }
  deriving (Generic, NFDataX, BitPack, ShowX, Show)

-- | Result of the clock control algorithm.
data CallistoResult (n :: Nat) = CallistoResult
  { maybeSpeedChange :: Maybe SpeedChange
  -- ^ Speed change requested for clock multiplier. This is 'Just' for a single
  -- cycle.
  , stability :: Vec n Stability
  -- ^ All stability indicators for all of the elastic buffers.
  , allStable :: Bool
  -- ^ Joint stability indicator signaling that all elastic buffers
  -- are stable.
  , allSettled :: Bool
  -- ^ Joint "being-settled" indicator signaling that all elastic
  -- buffers have been settled.
  , reframingState :: ReframingState
  -- ^ State of the Reframing detector
  }
  deriving (Generic, NFDataX)

{- | State of the state machine for realizing the "detect, store, and
wait" approach of [arXiv:2303.11467](https://arxiv.org/abs/2303.11467)
-}
data ReframingState
  = -- | The controller remains in this state until stability has been
    -- detected.
    Detect
  | -- | The controller remains in this state for the predefined
    -- number of cycles with the assumption that the elastic buffers
    -- of all other nodes are sufficiently stable after that time.
    Wait
      { targetCorrection :: !Float
      -- ^ Stored correction value to be applied at reframing time.
      , curWaitTime :: !(Unsigned 32)
      -- ^ Number of cycles to wait until reframing takes place.
      }
  | -- | Reframing has taken place. There is nothing more to do.
    Done
  deriving (Generic, NFDataX, BitPack, Show, BitPackC, ToFieldType)
