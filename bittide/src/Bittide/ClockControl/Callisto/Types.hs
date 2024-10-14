-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Bittide.ClockControl.Callisto.Types (
  CallistoResult (..),
  ReframingState (..),
  ControlConfig (..),
  ControlSt (..),
) where

import Clash.Prelude

import Bittide.ClockControl
import Bittide.ClockControl.StabilityChecker (StabilityIndication)
import Clash.Signal.TH.Extra (deriveSignalHasFields)

-- | Result of the clock control algorithm.
data CallistoResult (n :: Nat) = CallistoResult
  { maybeSpeedChange :: Maybe SpeedChange
  -- ^ Speed change requested for clock multiplier. This is 'Just' for a single
  -- cycle.
  , stability :: Vec n StabilityIndication
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

-- | Callisto specific control configuration options.
data ControlConfig (m :: Nat) = ControlConfig
  { reframingEnabled :: Bool
  -- ^ Enable reframing. Reframing allows a system to resettle buffers around
  -- their midpoints, without dropping any frames. For more information, see
  -- [arXiv:2303.11467](https://arxiv.org/abs/2303.11467).
  , waitTime :: Unsigned 32
  -- ^ Number of cycles to wait until reframing takes place after
  -- stability has been detected.
  , targetCount :: RelDataCount m
  -- ^ Target data count. See 'targetDataCount'.
  }

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
  deriving (Generic, NFDataX)

-- | Callisto's internal state used in 'callisto'
data ControlSt = ControlSt
  { _z_k :: !(Signed 32)
  -- ^ Accumulated speed change requests, where speedup ~ 1, slowdown ~ -1.
  , _b_k :: !SpeedChange
  -- ^ Previously submitted speed change request. Used to determine the estimated
  -- clock frequency.
  , _steadyStateTarget :: !Float
  -- ^ Steady-state value (determined when stability is detected for
  -- the first time).
  , rfState :: !ReframingState
  -- ^ finite state machine for reframing detection
  }
  deriving (Generic, NFDataX)

deriveSignalHasFields ''CallistoResult
deriveSignalHasFields ''ControlConfig
deriveSignalHasFields ''ControlSt
