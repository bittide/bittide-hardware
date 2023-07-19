-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Bittide.ClockControl.Callisto.Rust
  ( rustyCallisto
  ) where

import Clash.Prelude

import Foreign.C.Types (CUInt)
import Foreign.Marshal.Alloc
import Foreign.Ptr (castPtr)

import Foreign.Storable (Storable(..))
import System.IO.Unsafe

import Bittide.ClockControl (DataCount)
import Bittide.ClockControl.Callisto.Types
import Bittide.ClockControl.StabilityChecker

import Bittide.Simulate.RustFFI

import Data.Constraint
import Data.Constraint.Nat.Extra (isOne)

-- | Variant of 'Bittide.ClockControl.Callisto.callisto', which is
-- implemented in Rust and uses the Rust FFI for being simulated.
rustyCallisto ::
  forall m n dom.
  ( HiddenClockResetEnable dom
  , KnownNat n
  , KnownNat m
  , 1 <= n
  , 1 <= m
  , n + m <= 32
  , n <= BitsOf CUInt
  , m <= BitsOf CUInt
  ) =>
  -- | Configuration parameters.
  ControlConfig m ->
  -- | Link availability mask.
  Signal dom (BitVector n) ->
  -- | Stability indicators for each of the elastic buffers.
  Signal dom (Vec n StabilityIndication) ->
  -- | Data counts from elastic buffers.
  Signal dom (Vec n (DataCount m)) ->
  -- | Current state.
  Signal dom ControlSt ->
  -- | Updated state.
  Signal dom ControlSt
rustyCallisto config m scs counts st =
  callisto <$> m <*> scs <*> counts <*> st
 where
  callisto mask stabilityChecks dataCounts state =
    case isOne @m @(BitsOf Int) of
      Dict ->  unsafePerformIO $ do
        pState      <- malloc
        pVSI        <- malloc
        pDataCounts <- malloc
        pConfig     <- malloc

        poke pVSI $ VecS stabilityChecks
        poke pState state
        poke pDataCounts $ VecS (DataCountS <$> dataCounts)
        poke pConfig config

        callisto_rust
          (castPtr pConfig)
          (fromInteger $ toInteger mask)
          (castPtr pVSI)
          (castPtr pDataCounts)
          (castPtr pState)

        state' <- peek pState

        free pState
        free pVSI
        free pDataCounts
        free pConfig

        return state'
