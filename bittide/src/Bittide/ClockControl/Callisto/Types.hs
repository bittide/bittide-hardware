-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=20 #-}

module Bittide.ClockControl.Callisto.Types (
  CallistoResult (..),
  Stability (..),
) where

import Clash.Prelude

import Bittide.ClockControl

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
  }
  deriving (Generic, NFDataX)
