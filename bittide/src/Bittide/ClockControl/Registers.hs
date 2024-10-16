-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -fconstraint-solver-iterations=6 #-}

module Bittide.ClockControl.Registers where

import Clash.Prelude hiding (PeriodToCycles)

import Protocols
import Protocols.Wishbone

import Bittide.ClockControl
import Bittide.ClockControl.StabilityChecker
import Bittide.Wishbone
import Clash.Functor.Extra
import Clash.Sized.Vector.ToTuple (vecToTuple)

type StableBool = Bool
type SettledBool = Bool

{- | A wishbone accessible clock control interface.
This interface receives the link mask and 'RelDataCount's from all links.
Furthermore it produces FINC/FDEC pulses for the clock control boards.

The word-aligned address layout of the Wishbone interface is as follows:

- Address 0: Number of links                | 0x00
- Address 1: Link mask                      | 0x04
- Address 2: Link mask popcnt               | 0x08
- Address 3: Reframing enabled?             | 0x0C
- Address 4: FINC/FDEC                      | 0x10
- Address 5: Link stables                   | 0x14
- Address 6: Link settles                   | 0x18
- Addresses 7 to (7 + nLinks): Data counts  | 0x1C

__NB__: the `Maybe SpeedChange` part of the output is only asserted for a single cycle.
This must be stickied or otherwise held for the minimum pulse width specified by the
clock board this register is controlling.
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
  , 1 <= DomainPeriod dom
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
    , CSignal dom (Vec nLinks StabilityIndication)
    , CSignal dom ("ALL_STABLE" ::: Bool)
    , CSignal dom ("ALL_SETTLED" ::: Bool)
    )
clockControlWb mgn fsz linkMask reframing counters = Circuit go
 where
  go (wbM2S, _) =
    ( wbS2M
    , (fIncDec1, stabilityIndications, allStable, allSettled)
    )
   where
    filterCounters vMask vCounts = flip map (zip vMask vCounts)
      $ \(isActive, count) -> if isActive == high then count else 0
    filteredCounters = unbundle $ filterCounters <$> fmap bv2v linkMask <*> bundle counters
    stabilityIndications = bundle $ stabilityChecker mgn fsz <$> filteredCounters

    -- ▗▖ ▗▖▗▖ ▗▖▗▄▄▄▖▗▖  ▗▖    ▗▖  ▗▖▗▄▖ ▗▖ ▗▖     ▗▄▄▖▗▖ ▗▖ ▗▄▖ ▗▖  ▗▖ ▗▄▄▖▗▄▄▄▖    ▗▄▄▄▖▗▖ ▗▖▗▄▄▄▖ ▗▄▄▖
    -- ▐▌ ▐▌▐▌ ▐▌▐▌   ▐▛▚▖▐▌     ▝▚▞▘▐▌ ▐▌▐▌ ▐▌    ▐▌   ▐▌ ▐▌▐▌ ▐▌▐▛▚▖▐▌▐▌   ▐▌         █  ▐▌ ▐▌  █  ▐▌
    -- ▐▌ ▐▌▐▛▀▜▌▐▛▀▀▘▐▌ ▝▜▌      ▐▌ ▐▌ ▐▌▐▌ ▐▌    ▐▌   ▐▛▀▜▌▐▛▀▜▌▐▌ ▝▜▌▐▌▝▜▌▐▛▀▀▘      █  ▐▛▀▜▌  █   ▝▀▚▖
    -- ▐▙█▟▌▐▌ ▐▌▐▙▄▄▖▐▌  ▐▌      ▐▌ ▝▚▄▞▘▝▚▄▞▘    ▝▚▄▄▖▐▌ ▐▌▐▌ ▐▌▐▌  ▐▌▝▚▄▞▘▐▙▄▄▖      █  ▐▌ ▐▌▗▄█▄▖▗▄▄▞▘
    --
    -- ▗▖ ▗▖▗▄▄▖ ▗▄▄▄  ▗▄▖▗▄▄▄▖▗▄▄▄▖    ▗▄▄▄▖▗▖ ▗▖▗▄▄▄▖    ▗▄▄▖ ▗▖ ▗▖ ▗▄▄▖
    -- ▐▌ ▐▌▐▌ ▐▌▐▌  █▐▌ ▐▌ █  ▐▌         █  ▐▌ ▐▌▐▌       ▐▌ ▐▌▐▌ ▐▌▐▌
    -- ▐▌ ▐▌▐▛▀▘ ▐▌  █▐▛▀▜▌ █  ▐▛▀▀▘      █  ▐▛▀▜▌▐▛▀▀▘    ▐▛▀▚▖▐▌ ▐▌ ▝▀▚▖
    -- ▝▚▄▞▘▐▌   ▐▙▄▄▀▐▌ ▐▌ █  ▐▙▄▄▖      █  ▐▌ ▐▌▐▙▄▄▖    ▐▙▄▞▘▝▚▄▞▘▗▄▄▞▘
    readVec =
      dflipflop
        <$> ( pure (natToNum @nLinks)
                :> (zeroExtend @_ @_ @(32 - nLinks) <$> linkMask)
                :> (resize . pack . popCount <$> linkMask)
                :> (zeroExtend @_ @_ @31 <$> (boolToBV <$> reframing))
                :> (resize . pack <$> fIncDec1)
                :> (resize . pack . fmap boolToBit <$> linksStable)
                :> (resize . pack . fmap boolToBit <$> linksSettled)
                :> (pack . (extend @_ @_ @(32 - m)) <<$>> filteredCounters)
            )
    allStable = allAvailable stable <$> linkMask <*> stabilityIndications
    allSettled = allAvailable settled <$> linkMask <*> stabilityIndications

    linksStable = mapAvailable stable False <$> linkMask <*> stabilityIndications
    linksSettled = mapAvailable settled False <$> linkMask <*> stabilityIndications

    mapAvailable fn itemDefault mask = zipWith go1 (bitToBool <$> bv2v mask)
     where
      go1 avail item = if avail then fn item else itemDefault

    allAvailable f x y =
      and $ zipWith ((||) . not) (bitToBool <$> bv2v x) (f <$> y)

    -- ▗▖ ▗▖▗▄▄▖ ▗▄▄▄  ▗▄▖▗▄▄▄▖▗▄▄▄▖    ▗▄▄▄▖▗▖ ▗▖▗▄▄▄▖ ▗▄▄▖    ▗▖ ▗▖▗▖ ▗▖▗▄▄▄▖▗▖  ▗▖
    -- ▐▌ ▐▌▐▌ ▐▌▐▌  █▐▌ ▐▌ █  ▐▌         █  ▐▌ ▐▌  █  ▐▌       ▐▌ ▐▌▐▌ ▐▌▐▌   ▐▛▚▖▐▌
    -- ▐▌ ▐▌▐▛▀▘ ▐▌  █▐▛▀▜▌ █  ▐▛▀▀▘      █  ▐▛▀▜▌  █   ▝▀▚▖    ▐▌ ▐▌▐▛▀▜▌▐▛▀▀▘▐▌ ▝▜▌
    -- ▝▚▄▞▘▐▌   ▐▙▄▄▀▐▌ ▐▌ █  ▐▙▄▄▖      █  ▐▌ ▐▌▗▄█▄▖▗▄▄▞▘    ▐▙█▟▌▐▌ ▐▌▐▙▄▄▖▐▌  ▐▌
    --
    -- ▗▖  ▗▖▗▄▖ ▗▖ ▗▖     ▗▄▄▖▗▖ ▗▖ ▗▄▖ ▗▖  ▗▖ ▗▄▄▖▗▄▄▄▖    ▗▄▄▄▖▗▖ ▗▖▗▄▄▄▖    ▗▄▄▖ ▗▖ ▗▖ ▗▄▄▖
    --  ▝▚▞▘▐▌ ▐▌▐▌ ▐▌    ▐▌   ▐▌ ▐▌▐▌ ▐▌▐▛▚▖▐▌▐▌   ▐▌         █  ▐▌ ▐▌▐▌       ▐▌ ▐▌▐▌ ▐▌▐▌
    --   ▐▌ ▐▌ ▐▌▐▌ ▐▌    ▐▌   ▐▛▀▜▌▐▛▀▜▌▐▌ ▝▜▌▐▌▝▜▌▐▛▀▀▘      █  ▐▛▀▜▌▐▛▀▀▘    ▐▛▀▚▖▐▌ ▐▌ ▝▀▚▖
    --   ▐▌ ▝▚▄▞▘▝▚▄▞▘    ▝▚▄▄▖▐▌ ▐▌▐▌ ▐▌▐▌  ▐▌▝▚▄▞▘▐▙▄▄▖      █  ▐▌ ▐▌▐▙▄▄▖    ▐▙▄▞▘▝▚▄▞▘▗▄▄▞▘
    --
    -- Pull out the write-able fields
    (_, _, _, _, f4, _, _) = unbundle $ vecToTuple . take (SNat :: SNat 7) <$> writeVec

    fIncDec0 :: Signal dom (Maybe SpeedChange)
    fIncDec0 = unpack . resize <<$>> f4
    fIncDec1 :: Signal dom (Maybe SpeedChange)
    fIncDec1 = register Nothing fIncDec0

    (writeVec, wbS2M) = unbundle $ wbToVec @4 @_ <$> bundle readVec <*> wbM2S
