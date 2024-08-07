-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

#ifdef RUSTY_CALLISTO
{-# LANGUAGE ForeignFunctionInterface #-}
#endif

module Bittide.ClockControl.Foreign.Rust.Callisto
  ( rustyCallisto
  ) where

import Clash.Prelude

import Bittide.ClockControl (RelDataCount)
import Bittide.ClockControl.Callisto.Types
import Bittide.ClockControl.StabilityChecker

import Foreign.C.Types (CUInt(..))

#ifdef RUSTY_CALLISTO
import Data.Word (Word32)
import Data.Constraint
import Data.Constraint.Nat.Extra (isOne)

import Foreign.Marshal.Alloc
import Foreign.Ptr (Ptr)

import Foreign.Storable (Storable(..))
import System.IO.Unsafe

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
  Signal dom (Vec n (RelDataCount m)) ->
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
        poke pDataCounts $ VecS (RelDataCountS <$> dataCounts)
        poke pConfig config

        callisto_rust
          pConfig
          (fromInteger $ toInteger mask)
          pVSI
          pDataCounts
          pState

        state' <- peek pState

        free pState
        free pVSI
        free pDataCounts
        free pConfig

        return state'

foreign import ccall safe "__c_callisto_rust" callisto_rust ::
  Ptr (ControlConfig m) ->
  Word32  ->
  Ptr (VecS n StabilityIndication) ->
  Ptr (VecS n (RelDataCountS m)) ->
  Ptr (ControlSt) ->
  IO ()
#else
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
  Signal dom (Vec n (RelDataCount m)) ->
  -- | Current state.
  Signal dom ControlSt ->
  -- | Updated state.
  Signal dom ControlSt
rustyCallisto !_config !_m !_scs !_counts !_st = errorX "Rusty Callisto not enabled"
#endif
