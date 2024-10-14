-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}

module Bittide.ClockControl.DebugRegister (
  DebugRegisterData (..),
  debugRegisterWb,
) where

import Clash.Prelude hiding (PeriodToCycles)

import Protocols
import Protocols.Wishbone

import Data.Maybe (isJust)

import Bittide.ClockControl (SpeedChange)
import qualified Bittide.ClockControl.Callisto.Types as T
import Bittide.Wishbone
import Clash.Explicit.Signal.Extra (changepoints)
import Clash.Signal.TH.Extra (deriveSignalHasFields)
import Clash.Sized.Vector.ToTuple (vecToTuple)

data DebugRegisterCfg = DebugRegisterCfg
  { reframingEnabled :: Bool
  }

deriveSignalHasFields ''DebugRegisterCfg

{- | Debug register data
This record type contains debugging information that is reported from the CPU or
calculated from its outputs. This includes:
  - The current reframing state the CPU is in. This is included for ILA debugging
    purposes, and may be removed at a later date. This will be `Just state` if the
    last state the CPU wrote was valid, or `Nothing` if it was not.
  - The last update period from the CPU. Determined by the number of cycles between
    a rising edge on what amounts to the expression
    @isJust <$> callistoResult.maybeSpeedChange@
  - The minimum update period reported by @updatePeriod@
  - The maximum update period reported by @updatePeriod@
-}
data DebugRegisterData = DebugRegisterData
  { reframingState :: Maybe T.ReframingState
  , updatePeriod :: Unsigned 32
  , updatePeriodMin :: Unsigned 32
  , updatePeriodMax :: Unsigned 32
  }

deriveSignalHasFields ''DebugRegisterData

-- | Used just to match the discriminants of the 'ReframingState' type.
data ReframingStateKind = Detect | Wait | Done deriving (Generic, NFDataX, BitPack)

debugRegisterWb ::
  forall dom addrW.
  ( HiddenClockResetEnable dom
  , KnownNat addrW
  ) =>
  Circuit
    (Wishbone dom 'Standard addrW (BitVector 32), CSignal dom (Maybe SpeedChange))
    (CSignal dom DebugRegisterData)
debugRegisterWb = Circuit go
 where
  go ((wbM2S, clockMod), _) = ((wbS2M, pure ()), debugData)
   where
    debugData =
      DebugRegisterData
        <$> rfs
        <*> up
        <*> upMin
        <*> upMax

    readVec :: Vec 3 (Signal dom (BitVector 32))
    readVec =
      dflipflop
        <$> ( (resize . pack <$> rfsKind1)
                :> (pack <$> targetCorrection)
                :> (pack <$> targetCount)
                :> Nil
            )

    (f0, f1, f2) = unbundle $ vecToTuple <$> writeVec

    -- Read in the parts of a 'ReframingState' and assemble them into an instance.
    rfsKind :: Signal dom (Maybe ReframingStateKind)
    rfsKind = readState . unpack . resize <$> f0
     where
      readState :: Unsigned 2 -> Maybe ReframingStateKind
      readState 0 = Just Detect
      readState 1 = Just Wait
      readState 2 = Just Done
      readState _ = Nothing

    targetCorrection :: Signal dom Float
    targetCorrection = unpack <$> regMaybe 0 f1

    targetCount :: Signal dom (Unsigned 32)
    targetCount = unpack <$> regMaybe 0 f2

    rfs = liftA3 go1 rfsKind1 targetCorrection targetCount
     where
      go1 tCor tCou = fmap go2
       where
        go2 = \case
          Detect -> T.Detect
          Wait -> T.Wait tCor tCou
          Done -> T.Done

    (writeVec, wbS2M) = unbundle $ wbToVec <$> bundle readVec <*> wbM2S

    -- Any time 'clockMod' is 'Just _', that indicates the clock controller has sent out
    -- a speed change pulse.
    updated :: Signal dom Bool
    updated = isJust <$> clockMod

    -- Tracks the most recent update period.
    up :: Signal dom (Unsigned 32)
    up = moore go2 snd (0, 0) updated
     where
      go2 (cntr, cntrPrev) update = if update then (0, cntr) else (cntr + 1, cntrPrev)

    -- Indicates whether one or more period updates have been observed. Used to throw
    -- away the first period value, since that one is tracked through clock setup.
    receivedFirstUpdate :: Signal dom Bool
    receivedFirstUpdate = register False (receivedFirstUpdate .||. updated)

    -- Tracks whether the update period value has changed.
    upChanged = hideClockResetEnable changepoints up
    -- Tracks whether the min/max registers should update their values or not.
    updateExtrema = receivedFirstUpdate .&&. upChanged

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
