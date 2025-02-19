-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE NamedFieldPuns #-}

{- | Utilities dealing with clock ticks and edges (and absolute+relative
  event timings) in Clash.

This file was added when we were working on adding JTAG support on a
separate domain. For that, handling two different clocks and their edges
(and timing for simulation) was important.

We decided to keep JTAG on the CPU domain, so all functions except
'singleClockEdgesRelative' and 'singleClockEdgesAbsolute' are unused.


TODO: Figure out whether we want to upstream as is, or whether we want to
      generalize to /N/ clocks first.
-}
module VexRiscv.ClockTicks (
  ClockEdgeAB (..),
  clockTicksAbsolute,
  clockTicksRelative,
  clockEdgesAbsolute,
  clockEdgesRelative,
  singleClockEdgesAbsolute,
  singleClockEdgesRelative,
) where

import Clash.Promoted.Nat (snatToNum)
import Clash.Signal (
  ActiveEdge (..),
  Clock,
  KnownDomain,
  SActiveEdge (..),
  SDomainConfiguration (..),
  activeEdge,
  knownDomain,
 )
import Clash.Signal.Internal (Clock (..), ClockAB (..), Femtoseconds (..), Signal ((:-)))
import Data.Coerce (coerce)
import Data.Int (Int64)
import Data.List (mapAccumL)
import Data.Ord ()
import Prelude

{- | Given two clocks, produce a list of clock ticks indicating which clock
(or both) ticked. Can be used in components handling multiple clocks, such
as @unsafeSynchronizer@ or dual clock FIFOs. In contrast to 'clockTicks',
this version also produces the absolute time at which the tick happened.

If your primitive does not care about coincided clock edges, it should - by
convention - replace it by @ClockB:ClockA:@.

Returned time is in /femtoseconds/.
-}
clockTicksAbsolute ::
  (KnownDomain domA, KnownDomain domB) =>
  Clock domA ->
  Clock domB ->
  [(Int64, ClockAB)]
clockTicksAbsolute clkA clkB =
  clockTicksEitherAbsolute (toEither clkA) (toEither clkB)

{- | Given two clocks, produce a list of clock ticks indicating which clock
(or both) ticked. Can be used in components handling multiple clocks, such
as @unsafeSynchronizer@ or dual clock FIFOs. In contrast to 'clockTicks',
this version also produces the time since the last tick. Note that the first
"time since last tick" is always zero.

If your primitive does not care about coincided clock edges, it should - by
convention - replace it by @ClockB:ClockA:@.

Returned time is in /femtoseconds/.
-}
clockTicksRelative ::
  (KnownDomain domA, KnownDomain domB) =>
  Clock domA ->
  Clock domB ->
  [(Int64, ClockAB)]
clockTicksRelative clkA clkB =
  clockTicksEitherRelative (toEither clkA) (toEither clkB)

{- | Given two clocks, produce a list of clock ticks indicating which clock
(or both) ticked. Can be used in components handling multiple clocks, such
as @unsafeSynchronizer@ or dual clock FIFOs.

If your primitive does not care about coincided clock edges, it should - by
convention - replace it by @ClockEdgeB edgeB : ClockEdgeA edgeA:@.

Returned time is in /femtoseconds/.
-}
clockEdgesAbsolute ::
  forall domA domB.
  (KnownDomain domA, KnownDomain domB) =>
  Clock domA ->
  Clock domB ->
  [(Int64, ClockEdgeAB)]
clockEdgesAbsolute clkA clkB =
  clockEdgesEitherAbsolute
    (toActiveEdge (activeEdge @domA))
    (toActiveEdge (activeEdge @domB))
    (toEither clkA)
    (toEither clkB)

{- | Given two clocks, produce a list of clock ticks indicating which clock
(or both) ticked. Can be used in components handling multiple clocks, such
as @unsafeSynchronizer@ or dual clock FIFOs.

If your primitive does not care about coincided clock edges, it should - by
convention - replace it by @ClockEdgeB edgeB : ClockEdgeA edgeA:@.

Returned time is in /femtoseconds/.
-}
clockEdgesRelative ::
  forall domA domB.
  (KnownDomain domA, KnownDomain domB) =>
  Clock domA ->
  Clock domB ->
  [(Int64, ClockEdgeAB)]
clockEdgesRelative clkA clkB =
  clockEdgesEitherRelative
    (toActiveEdge (activeEdge @domA))
    (toActiveEdge (activeEdge @domB))
    (toEither clkA)
    (toEither clkB)

{- | Given a clock, produce a list of clock edges and the time at which the
edge occurs.

This can be used for simulating designs that need to advance time to get
accurate traces.

Returned time is in /femotseconds/.
-}
singleClockEdgesAbsolute ::
  forall dom.
  (KnownDomain dom) =>
  Clock dom ->
  [(Int64, ActiveEdge)]
singleClockEdgesAbsolute clk =
  singleClockEdgesEitherAbsolute
    (toActiveEdge (activeEdge @dom))
    (toEither clk)

{- | Given a clock, produce a list of clock edges and the time since the last
edge.

This can be used for simulating designs that need to advance time to get
accurate traces.

Returned time is in /femotseconds/.
-}
singleClockEdgesRelative ::
  forall dom.
  (KnownDomain dom) =>
  Clock dom ->
  [(Int64, ActiveEdge)]
singleClockEdgesRelative clk =
  singleClockEdgesEitherRelative
    (toActiveEdge (activeEdge @dom))
    (toEither clk)

-- | GADT version of 'ActiveEdge' to 'ActiveEdge' conversion
toActiveEdge :: SActiveEdge edge -> ActiveEdge
toActiveEdge SRising = Rising
toActiveEdge SFalling = Falling

toEither ::
  forall dom.
  (KnownDomain dom) =>
  Clock dom ->
  Either Int64 (Signal dom Int64)
toEither (Clock _ maybePeriods)
  | Just periods <- maybePeriods =
      Right (unFemtosecondsSignal periods)
  | SDomainConfiguration{sPeriod} <- knownDomain @dom =
      -- Convert to femtoseconds - dynamic clocks use them
      Left (1000 * snatToNum sPeriod)
 where
  -- Coerce whole signal instead of `fmap coerce` to prevent useless constructor
  -- packing and unpacking.
  unFemtosecondsSignal :: Signal dom Femtoseconds -> Signal dom Int64
  unFemtosecondsSignal = coerce

{- | Given two clock periods, produce a list of clock ticks indicating which clock
(or both) ticked. Can be used in components handling multiple clocks, such
as @unsafeSynchronizer@ or dual clock FIFOs. In contrast to 'clockTicksEither',
this version also produces the absolute time at which the event happened.

If your primitive does not care about coincided clock edges, it should - by
convention - replace it by @ClockB:ClockA:@.
-}
clockTicksEitherAbsolute ::
  Either Int64 (Signal domA Int64) ->
  Either Int64 (Signal domB Int64) ->
  [(Int64, ClockAB)]
clockTicksEitherAbsolute clkA clkB =
  case (clkA, clkB) of
    (Left tA, Left tB) | tA == tB -> zip (iterate (+ tA) 0) (repeat ClockAB)
    (Left tA, Left tB) -> goStatic 0 0 tA tB
    (Right tA, Right tB) -> goDynamic 0 0 tA tB
    (Left tA, Right tB) -> clockTicksEitherAbsolute (Right (pure tA)) (Right tB)
    (Right tA, Left tB) -> clockTicksEitherAbsolute (Right tA) (Right (pure tB))
 where
{- FOURMOLU_DISABLE -}
  goStatic :: Int64 -> Int64 -> Int64 -> Int64 -> [(Int64, ClockAB)]
  goStatic absTimeA absTimeB tA tB =
    case compare absTimeA absTimeB of
      LT -> (absTimeA, ClockA)  : goStatic (absTimeA + tA) absTimeB        tA tB
      EQ -> (absTimeA, ClockAB) : goStatic (absTimeA + tA) (absTimeB + tB) tA tB
      GT -> (absTimeB, ClockB)  : goStatic absTimeA        (absTimeB + tB) tA tB

  goDynamic :: Int64 -> Int64 -> Signal domA Int64 -> Signal domB Int64 -> [(Int64, ClockAB)]
  goDynamic absTimeA absTimeB tsA@(~(tA :- tsA0)) tsB@(~(tB :- tsB0)) =
    -- Even though we lazily match on the signal's constructor, this shouldn't
    -- build up a significant chain of chunks as 'absTimeX' gets evaluated
    -- every iteration.
    case compare absTimeA absTimeB of
      LT -> (absTimeA, ClockA)  : goDynamic (absTimeA + tA) absTimeB        tsA0 tsB
      EQ -> (absTimeA, ClockAB) : goDynamic (absTimeA + tA) (absTimeB + tB) tsA0 tsB0
      GT -> (absTimeB, ClockB)  : goDynamic absTimeA        (absTimeB + tB) tsA  tsB0
{- FOURMOLU_ENABLE -}

{- | Given two clock periods, produce a list of clock ticks indicating which clock
(or both) ticked. Can be used in components handling multiple clocks, such
as @unsafeSynchronizer@ or dual clock FIFOs. In contrast to 'clockTicksEither',
this version also produces the time since the last event.

If your primitive does not care about coincided clock edges, it should - by
convention - replace it by @ClockB:ClockA:@.
-}
clockTicksEitherRelative ::
  Either Int64 (Signal domA Int64) ->
  Either Int64 (Signal domB Int64) ->
  [(Int64, ClockAB)]
clockTicksEitherRelative clkA clkB = zip relativeTimestamps ticks
 where
  relativeTimestamps = 0 : zipWith (-) (tail timestamps) timestamps
  (timestamps, ticks) = unzip (clockTicksEitherAbsolute clkA clkB)

-- | Flip edge from rising to falling, and vice versa
oppositeEdge :: ActiveEdge -> ActiveEdge
oppositeEdge Rising = Falling
oppositeEdge Falling = Rising

data ClockEdgeAB
  = ClockEdgeA !ActiveEdge
  | ClockEdgeB !ActiveEdge
  | ClockEdgeAB !ActiveEdge !ActiveEdge
  deriving (Show, Eq)

{- | Given two clock periods, produce a list of clock ticks indicating which clock
(or both) ticked. Can be used in components handling multiple clocks, such
as @unsafeSynchronizer@ or dual clock FIFOs. In contrast to 'clockTicksEither',
this version also produces the absolute time at which the event happened.

If your primitive does not care about coincided clock edges, it should - by
convention - replace it by @ClockEdgeB edgeB : ClockEdgeA edgeA:@.
-}
clockEdgesEitherAbsolute ::
  -- | First active edge for clock A
  ActiveEdge ->
  -- | First active edge for clock B
  ActiveEdge ->
  -- | Clock periods for clock A
  Either Int64 (Signal domA Int64) ->
  -- | Clock periods for clock B
  Either Int64 (Signal domB Int64) ->
  [(Int64, ClockEdgeAB)]
clockEdgesEitherAbsolute firstEdgeA firstEdgeB clkA clkB =
  case (clkA, clkB) of
    (Left tA, Left tB) | tA == tB -> goSame (halve tA)
    (Left tA, Left tB) -> goStatic 0 0 firstEdgeA firstEdgeB (halve tA) (halve tB)
    (Right tA, Right tB) -> goDynamic 0 0 firstEdgeA firstEdgeB (halves tA) (halves tB)
    (Left tA, Right tB) ->
      clockEdgesEitherAbsolute firstEdgeA firstEdgeB (Right (pure tA)) (Right tB)
    (Right tA, Left tB) ->
      clockEdgesEitherAbsolute firstEdgeA firstEdgeB (Right tA) (Right (pure tB))
 where
  halves = go . fmap halve
   where
    go ((t0, t1) :- ts) = t0 :- t1 :- go ts

  halve t =
    ( t `div` 2
    , t - (t `div` 2)
    )

  goSame :: (Int64, Int64) -> [(Int64, ClockEdgeAB)]
  goSame (t0, t1) =
    zip
      (snd $ mapAccumL (\acc t -> (acc + t, acc)) 0 (cycle [t0, t1]))
      ( cycle
          [ ClockEdgeAB firstEdgeA firstEdgeB
          , ClockEdgeAB (oppositeEdge firstEdgeA) (oppositeEdge firstEdgeB)
          ]
      )

{- FOURMOLU_DISABLE -}
  goStatic ::
    Int64 ->
    Int64 ->
    ActiveEdge ->
    ActiveEdge ->
    (Int64, Int64) ->
    (Int64, Int64) ->
    [(Int64, ClockEdgeAB)]
  goStatic absTimeA absTimeB !edgeA !edgeB (tA0, tA1) (tB0, tB1) =
    case compare absTimeA absTimeB of
      -- XXX: Sorry for breaking the 80/90 limit. I have no idea how to break this
      --      over multiple lines without sacrificing readability.
      LT -> (absTimeA, ClockEdgeA edgeA)        : goStatic (absTimeA + tA0) absTimeB         (oppositeEdge edgeA) edgeB                (tA1, tA0) (tB0, tB1)
      EQ -> (absTimeA, ClockEdgeAB edgeA edgeB) : goStatic (absTimeA + tA0) (absTimeB + tB0) (oppositeEdge edgeA) (oppositeEdge edgeB) (tA1, tA0) (tB1, tB0)
      GT -> (absTimeB, ClockEdgeB edgeB)        : goStatic absTimeA         (absTimeB + tB0) edgeA                (oppositeEdge edgeB) (tA0, tA1) (tB1, tB0)

  goDynamic ::
    Int64 ->
    Int64 ->
    ActiveEdge ->
    ActiveEdge ->
    Signal domA Int64 ->
    Signal domB Int64 ->
    [(Int64, ClockEdgeAB)]
  goDynamic absTimeA absTimeB edgeA edgeB tsA@(~(tA :- tsA0)) tsB@(~(tB :- tsB0)) =
    -- Even though we lazily match on the signal's constructor, this shouldn't
    -- build up a significant chain of chunks as 'absTimeX' gets evaluated
    -- every iteration.
    case compare absTimeA absTimeB of
      -- XXX: Sorry for breaking the 80/90 limit. I have no idea how to break this
      --      over multiple lines without sacrificing readability.
      LT -> (absTimeA, ClockEdgeA edgeA)        : goDynamic (absTimeA + tA) absTimeB        (oppositeEdge edgeA) edgeB                tsA0 tsB
      EQ -> (absTimeA, ClockEdgeAB edgeA edgeB) : goDynamic (absTimeA + tA) (absTimeB + tB) (oppositeEdge edgeA) (oppositeEdge edgeB) tsA0 tsB0
      GT -> (absTimeB, ClockEdgeB edgeB)        : goDynamic absTimeA        (absTimeB + tB) edgeA                (oppositeEdge edgeB) tsA  tsB0
{- FOURMOLU_ENABLE -}

{- | Given two clock periods, produce a list of clock ticks indicating which clock
(or both) ticked. Can be used in components handling multiple clocks, such
as @unsafeSynchronizer@ or dual clock FIFOs. In contrast to 'clockTicksEither',
this version also produces the time since the last event. For the first edge
the time since the last event is set to zero.

If your primitive does not care about coincided clock edges, it should - by
convention - replace it by @ClockEdgeB edgeB : ClockEdgeA edgeA:@.
-}
clockEdgesEitherRelative ::
  -- | First active edge for clock A
  ActiveEdge ->
  -- | First active edge for clock B
  ActiveEdge ->
  Either Int64 (Signal domA Int64) ->
  Either Int64 (Signal domB Int64) ->
  [(Int64, ClockEdgeAB)]
clockEdgesEitherRelative firstEdgeA firstEdgeB clkA clkB = zip relativeTimestamps ticks
 where
  relativeTimestamps = 0 : zipWith (-) (tail timestamps) timestamps
  (timestamps, ticks) = unzip (clockEdgesEitherAbsolute firstEdgeA firstEdgeB clkA clkB)

{- | Given the clock periods of a single clock, produce a list of clock edges
and the (absolute) times when these edges are occuring.

Same as 'singleClockEdgesEitherRelative', but produces absolute times
(in /femotseconds/).
-}
singleClockEdgesEitherAbsolute ::
  ActiveEdge ->
  Either Int64 (Signal dom Int64) ->
  [(Int64, ActiveEdge)]
singleClockEdgesEitherAbsolute firstEdge clk =
  case clk of
    Left t -> goStatic 0 firstEdge (halve t)
    Right t -> goDynamic 0 firstEdge (halves t)
 where
  halves = go . fmap halve
   where
    go ((t0, t1) :- ts) = t0 :- t1 :- go ts

  halve t =
    ( t `div` 2
    , t - (t `div` 2)
    )

  goStatic ::
    Int64 ->
    ActiveEdge ->
    (Int64, Int64) ->
    [(Int64, ActiveEdge)]
  goStatic absTime !currentEdge (t0, t1) =
    (absTime, currentEdge) : goStatic (absTime + t0) (oppositeEdge currentEdge) (t1, t0)

  goDynamic ::
    Int64 ->
    ActiveEdge ->
    Signal domA Int64 ->
    [(Int64, ActiveEdge)]
  goDynamic absTime !currentEdge ~(t :- ts1) =
    (absTime, currentEdge) : goDynamic (absTime + t) (oppositeEdge currentEdge) ts1

{- | Given the clock periods of a single clock, produce a list of clock edges
and the (relative) times since the last edge occured.

Same as 'singleClockEdgesEitherAbsolute', but produces relative times
(in /femotseconds/).
-}
singleClockEdgesEitherRelative ::
  ActiveEdge ->
  Either Int64 (Signal dom Int64) ->
  [(Int64, ActiveEdge)]
singleClockEdgesEitherRelative firstEdge clk = zip relativeTimestamps edges
 where
  relativeTimestamps = 0 : zipWith (-) (tail timestamps) timestamps
  (timestamps, edges) = unzip (singleClockEdgesEitherAbsolute firstEdge clk)
