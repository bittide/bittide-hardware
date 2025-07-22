-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=20 #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

module Bittide.ClockControl.DebugRegister (
  DebugRegisterCfg (..),
  DebugRegisterData (..),
  debugRegisterWb,
) where

import Clash.Prelude hiding (PeriodToCycles)

import Protocols
import Protocols.MemoryMap
import Protocols.Wishbone

import Bittide.ClockControl (SpeedChange)
import Clash.Class.BitPackC
import Clash.Explicit.Signal.Extra (changepoints)
import Data.Maybe (isJust)
import GHC.Stack (HasCallStack)
import Protocols.MemoryMap.Registers.WishboneStandard (
  RegisterConfig (access),
  deviceWbC,
  registerConfig,
  registerWbCI,
  registerWbCI_,
 )

import qualified Bittide.ClockControl.Callisto.Types as T

data DebugRegisterCfg = DebugRegisterCfg
  { reframingEnabled :: Bool
  }

{- | Debugging information that is reported from the CPU or calculated from its
outputs.
-}
data DebugRegisterData = DebugRegisterData
  { reframingState :: T.ReframingState
  -- ^ The current reframing state the CPU is in. This is included for ILA
  -- debugging purposes, and may be removed at a later date. This will be
  -- @Just state@ if the last state the CPU wrote was valid, or @Nothing@ if it
  -- was not.
  , updatePeriod :: Unsigned 32
  -- ^ The last update period from the CPU. Determined by the number of cycles
  -- between a rising edge on what amounts to the expression
  -- @isJust <$> callistoResult.maybeSpeedChange@
  , updatePeriodMin :: Unsigned 32
  -- ^ The minimum update period reported by 'updatePeriod'
  , updatePeriodMax :: Unsigned 32
  -- ^ The maximum update period reported by 'updatePeriod'
  }

{- | A Wishbone accessible debug register
This interface holds values that the CPU should be reporting to the FPGA for debugging
purposes. It also reports the update period coming from the CPU, as well as the
minimum and maximum of the update period.
-}
debugRegisterWb ::
  forall dom addrW.
  ( HiddenClockResetEnable dom
  , HasCallStack
  , KnownNat addrW
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
  Signal dom DebugRegisterCfg ->
  Circuit
    ( (ConstBwd MM, Wishbone dom 'Standard addrW (BitVector 32))
    , CSignal dom (Maybe SpeedChange)
    )
    (CSignal dom DebugRegisterData)
debugRegisterWb cfg = circuit $ \((mm, wb), Fwd maybeSpeedChange) -> do
  [wbRfState, wbRfEnabled] <- deviceWbC "DebugRegister" -< (mm, wb)

  (Fwd rfState, _r0) <- registerWbCI rfStateConfig T.Detect -< (wbRfState, Fwd noWrite)

  let rfEnabledWrite = Just <$> cfg.reframingEnabled
  registerWbCI_ rfEnabledConfig False -< (wbRfEnabled, Fwd rfEnabledWrite)

  let
    updated :: Signal dom Bool
    updated = isJust <$> maybeSpeedChange

    -- Indicates whether one or more period updates have been observed. Used to throw
    -- away the first period value, since that one is tracked through clock setup.
    receivedFirstUpdate :: Signal dom Bool
    receivedFirstUpdate = register False (receivedFirstUpdate .||. updated)

    -- Tracks whether the update period value has changed.
    upChanged = hideClockResetEnable changepoints up

    -- Tracks whether the min/max registers should update their values or not.
    updateExtrema = receivedFirstUpdate .&&. upChanged

    -- Tracks the most recent update period.
    up :: Signal dom (Unsigned 32)
    up = moore go2 snd (0, 0) updated
     where
      go2 (cntr, _cntrPrev) True = (0, cntr)
      go2 (cntr, cntrPrev) False = (cntr + 1, cntrPrev)

    -- Tracks the minimum update period cycles.
    upMin :: Signal dom (Unsigned 32)
    upMin =
      regEn
        maxBound
        updateExtrema
        (liftA2 min upMin up)

    -- Tracks the maximum update period cycles.
    upMax :: Signal dom (Unsigned 32)
    upMax =
      regEn
        minBound
        updateExtrema
        (liftA2 max upMax up)

  idC
    -< Fwd
      ( DebugRegisterData
          <$> rfState
          <*> up
          <*> upMin
          <*> upMax
      )
 where
  noWrite = pure Nothing

  rfStateConfig :: RegisterConfig
  rfStateConfig = registerConfig "reframing_state"

  rfEnabledConfig :: RegisterConfig
  rfEnabledConfig = (registerConfig "reframing_enabled"){access = ReadOnly}
