-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -fconstraint-solver-iterations=6 #-}

module Bittide.ClockControl.Registers where

import Clash.Prelude

import Protocols
import Protocols.Wishbone

import Bittide.ClockControl
import qualified Bittide.ClockControl.Callisto.Types as T
import Bittide.ClockControl.Callisto.Util (FDEC, FINC, speedChangeToPins, stickyBits)
import Bittide.ClockControl.StabilityChecker
import Bittide.Wishbone
import Clash.Functor.Extra
import Clash.Sized.Vector.ToTuple (vecToTuple)

import Data.Maybe (fromMaybe, isJust)

type StableBool = Bool
type SettledBool = Bool

{- | A wishbone accessible clock control interface.
This interface receives the link mask and 'RelDataCount's from all links.
Furthermore it produces FINC/FDEC pulses for the clock control boards.

The word-aligned address layout of the Wishbone interface is as follows:

- Address 0: Number of links
- Address 1: Link mask
- Address 2: FINC/FDEC
- Address 3: Link stables
- Address 4: Link settles
- Addresses 5 to (5 + nLinks): Data counts
-}
clockControlWb ::
  forall dom addrW nLinks m margin framesize.
  ( HiddenClockResetEnable dom
  , KnownNat addrW
  , 1 <= framesize
  , 1 <= nLinks
  , KnownNat nLinks
  , KnownNat m
  , m <= 32
  , nLinks <= 32
  ) =>
  -- | Maximum number of elements the incoming buffer occupancy is
  -- allowed to deviate from the current @target@ for it to be
  -- considered "stable".
  SNat margin ->
  -- | Minimum number of clock cycles the incoming buffer occupancy
  -- must remain within the @margin@ for it to be considered "stable".
  SNat framesize ->
  -- | Link mask
  Signal dom (BitVector nLinks) ->
  -- | Counters
  Vec nLinks (Signal dom (RelDataCount m)) ->
  -- | Wishbone accessible clock control circuitry
  Circuit
    (Wishbone dom 'Standard addrW (BitVector 32))
    ( CSignal dom (FINC, FDEC)
    , CSignal dom ("ALL_STABLE" ::: Bool)
    , "updatePeriod" ::: CSignal dom Int
    )
clockControlWb margin framesize linkMask counters = Circuit go
 where
  go (wbM2S, _) =
    ( wbS2M
    , (fIncDec3, all (== True) <$> (fmap stable <$> stabilityIndications), updatePeriod)
    )
   where
    stabilityIndications = bundle $ stabilityChecker margin framesize <$> counters
    readVec =
      dflipflop
        <$> ( pure (natToNum @nLinks)
                :> (zeroExtend @_ @_ @(32 - nLinks) <$> linkMask)
                :> (resize . pack <$> fIncDec1)
                :> (resize . pack . fmap stable <$> stabilityIndications)
                :> (resize . pack . fmap settled <$> stabilityIndications)
                :> (pack . (extend @_ @_ @(32 - m)) <<$>> counters)
            )
    fIncDec0 :: Signal dom (Maybe SpeedChange)
    fIncDec0 = (\v -> unpack . resize <$> v !! (2 :: Unsigned 2)) <$> writeVec
    fIncDec1 :: Signal dom (Maybe SpeedChange)
    fIncDec1 = register Nothing fIncDec0

    fIncDec2 :: Signal dom SpeedChange
    fIncDec2 = fromMaybe NoChange <$> fIncDec1
    fIncDec3 :: Signal dom (FINC, FDEC)
    fIncDec3 =
      delay minBound {- glitch filter -}
        $ stickyBits d20 (speedChangeToPins <$> fIncDec2)
    (writeVec, wbS2M) = unbundle $ wbToVec <$> bundle readVec <*> wbM2S
    updated :: Signal dom Bool
    updated = fmap isJust fIncDec0
    updatePeriod :: Signal dom Int
    updatePeriod = moore go2 snd (0, 0) updated
     where
      go2 (cntr, cntrPrev) update = case update of
        False -> (cntr + 1, cntrPrev)
        True -> (0, cntr)

data ReframingStateKind = Detect | Wait | Done deriving (Generic, NFDataX, BitPack)

{- | A wishbone accessible clock control interface.
This interface receives the link mask and 'RelDataCount's from all links.
Furthermore it produces FINC/FDEC pulses for the clock control boards.

The word-aligned address layout of the Wishbone interface is as follows:

- Address 0: Reframing kind                 -- Reframing register
- Address 1: Wait target correction         |
- Address 2: Wait target count              |
- Address 3: Number of links                -- Clock control register
- Address 4: Link mask                      |
- Address 5: Reframing enabled?             |
- Address 6: FINC/FDEC                      |
- Address 7: Link stables                   |
- Address 8: Link settles                   |
- Addresses 9 to (9 + nLinks): Data counts  --
-}
clockControlWb2 ::
  forall dom addrW nLinks m margin framesize.
  ( HiddenClockResetEnable dom
  , KnownNat addrW
  , 2 <= addrW
  , 1 <= framesize
  , 1 <= nLinks
  , KnownNat nLinks
  , KnownNat m
  , m <= 32
  , nLinks <= 32
  ) =>
  -- | Maximum number of elements the incoming buffer occupancy is
  -- allowed to deviate from the current @target@ for it to be
  -- considered "stable".
  SNat margin ->
  -- | Minimum number of clock cycles the incoming buffer occupancy
  -- must remain within the @margin@ for it to be considered "stable".
  SNat framesize ->
  -- | Link mask
  Signal dom (BitVector nLinks) ->
  -- | Reframing enabled?
  Signal dom Bool ->
  -- | Counters
  Vec nLinks (Signal dom (RelDataCount m)) ->
  -- | Wishbone accessible clock control circuitry
  Circuit
    (Wishbone dom 'Standard addrW (BitVector 32))
    ( CSignal dom (Maybe SpeedChange)
    , CSignal dom T.ReframingState
    , CSignal dom (Vec nLinks StabilityIndication)
    , CSignal dom ("ALL_STABLE" ::: Bool)
    , CSignal dom ("ALL_SETTLED" ::: Bool)
    , "updatePeriod" ::: CSignal dom Int
    )
clockControlWb2 mgn fsz linkMask reframing counters = Circuit go
 where
  go (wbM2S, _) =
    ( wbS2M
    , (fIncDec2, reframingState, stabilityIndications, allStable, allSettled, updatePeriod)
    )
   where
    filterCounters vMask vCounts = flip map (zip vMask vCounts)
      $ \(isActive, count) -> if isActive == high then count else 0
    filteredCounters = filterCounters <$> fmap bv2v linkMask <*> bundle counters
    stabilityIndications = bundle $ stabilityChecker mgn fsz <$> unbundle filteredCounters
    readVec =
      dflipflop
        <$> ( (resize . pack <$> rfsKind1)
                :> (pack <$> targetCorrection1)
                :> (pack <$> targetCount1)
                :> pure (natToNum @nLinks)
                :> (zeroExtend @_ @_ @(32 - nLinks) <$> linkMask)
                :> (zeroExtend @_ @_ @31 <$> (boolToBV <$> reframing))
                :> (resize . pack <$> fIncDec1)
                :> (resize . pack . fmap stable <$> stabilityIndications)
                :> (resize . pack . fmap settled <$> stabilityIndications)
                :> (pack . (extend @_ @_ @(32 - m)) <<$>> counters)
            )
    allStable = allAvailable stable <$> linkMask <*> stabilityIndications
    allSettled = allAvailable settled <$> linkMask <*> stabilityIndications

    allAvailable f x y =
      and $ zipWith ((||) . not) (bitToBool <$> bv2v x) (f <$> y)

    -- Pull out the write-able fields
    (f0, f1, f2, _, _, _, f6, _, _) = unbundle $ vecToTuple . take (SNat :: SNat 9) <$> writeVec

    fIncDec0 :: Signal dom (Maybe SpeedChange)
    fIncDec0 = unpack . resize <<$>> f6
    fIncDec1 :: Signal dom (Maybe SpeedChange)
    fIncDec1 = register Nothing fIncDec0
    fIncDec2 :: Signal dom (Maybe SpeedChange)
    fIncDec2 = delay Nothing $ stickyBits d20 fIncDec1

    rfsKind0 :: Signal dom (Maybe ReframingStateKind)
    rfsKind0 = unpack . resize <<$>> f0
    rfsKind1 :: Signal dom (Maybe ReframingStateKind)
    rfsKind1 = register Nothing rfsKind0
    rfsKind2 :: Signal dom (Maybe ReframingStateKind)
    rfsKind2 = delay Nothing $ stickyBits d20 rfsKind1

    targetCorrection0 :: Signal dom (Maybe Float)
    targetCorrection0 = unpack . resize <<$>> f1
    targetCorrection1 :: Signal dom Float
    targetCorrection1 = register 0.0 (fromMaybe 0.0 <$> targetCorrection0)
    targetCorrection2 :: Signal dom Float
    targetCorrection2 = delay 0.0 $ stickyBits d20 targetCorrection1

    targetCount0 :: Signal dom (Maybe (Unsigned 32))
    targetCount0 = unpack . resize <<$>> f2
    targetCount1 :: Signal dom (Unsigned 32)
    targetCount1 = register 0 (fromMaybe 0 <$> targetCount0)
    targetCount2 :: Signal dom (Unsigned 32)
    targetCount2 = delay 0 $ stickyBits d20 targetCount1

    reframingState = liftA3 go1 rfsKind2 targetCorrection2 targetCount2
     where
      go1 rfsk1 tCor tCou = go2 rfsk1
       where
        go2 (Just rfsk2) = case rfsk2 of
          Detect -> T.Detect
          Wait -> T.Wait tCor tCou
          Done -> T.Done
        go2 _ = T.Done

    (writeVec, wbS2M) = unbundle $ wbToVec <$> bundle readVec <*> wbM2S

    updated :: Signal dom Bool
    updated = isJust <$> fIncDec0
    updatePeriod :: Signal dom Int
    updatePeriod = moore go2 snd (0, 0) updated
     where
      go2 (cntr, cntrPrev) update = if update then (0, cntr) else (cntr + 1, cntrPrev)
