{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}

-- Suppress Clash domain warnings
{-# OPTIONS_GHC -Wno-orphans #-}

-- Clock definitions aren't much more readable with top level signatures..
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Tests.Clash.Signal.Extra where

import qualified Prelude as P
import Clash.Explicit.Prelude hiding (d122, d107, d61)

import Clash.Signal.Extra
  ( ClockEdgeAB(..), clockTicksAbsolute, clockTicksRelative
  , clockEdgesAbsolute, clockEdgesRelative
  )
import Clash.Signal.Internal (ClockAB(..), Femtoseconds(..), clockTicks, dynamicClockGen)

import qualified Data.List as L
import Data.Maybe (catMaybes)
import Data.Int (Int64)

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.HUnit

import Tests.Extra (carthesianProductTests)

createDomain vSystem{vName="R61", vPeriod=61}
createDomain vSystem{vName="R107", vPeriod=107}
createDomain vSystem{vName="R122", vPeriod=122}

-- TODO: Test with falling edges
createDomain vSystem{vName="F61", vPeriod=61, vActiveEdge=Falling}
createDomain vSystem{vName="F107", vPeriod=107, vActiveEdge=Falling}
createDomain vSystem{vName="F122", vPeriod=122, vActiveEdge=Falling}

createDomain vSystem{vName="D61", vPeriod=61}
createDomain vSystem{vName="D107", vPeriod=107}
createDomain vSystem{vName="D122", vPeriod=122}

r61  = clockGen @R61
r107 = clockGen @R107
r122 = clockGen @R122

f61  = clockGen @F61
f107 = clockGen @F107
f122 = clockGen @F122

-- | Clock whose clock period differs slightly from 61 ps every tick
d61 :: Clock D61
d61 = dynamicClockGen (fromList periods)
 where
  periods = P.cycle $ (\r -> Femtoseconds (1000 * (61 + r))) <$> rands
  rands  = [2, 3, 5, 8,-1, 7, -8, -2, -5, -7, -10, -9, 1, -3, 10, 0, 6,-6, 9, -4, 4]

-- | Clock whose clock period differs slightly from 107 ps every tick
d107 :: Clock D107
d107 = dynamicClockGen (fromList periods)
 where
  periods = P.cycle $ (\r -> Femtoseconds (1000 * (107 + r))) <$> rands
  rands = [-1, -5, -3, 2, -8, -4, 8, -9, 9, 5, -6, 1, 6, 4, 0, 3, 7, -2, -7, 10, -10]

-- | Clock whose clock period differs slightly from 122 ps every tick
d122 :: Clock D122
d122 = dynamicClockGen (fromList periods)
 where
  periods = P.cycle $ (\r -> Femtoseconds (1000 * (122 + r))) <$> rands
  rands = [0, 3, -8, -6, 10, -9, -4, -3, 5, 1, -10, 8, -1, 4, 6, -5, 2, -7, -2, 9, 7]

-- | Compare to "infinite" lists, by comparing the first /N/ samples. See
-- implemenation for the value of /N/.
infEq :: (Eq a, Show a) => [a] -> [a] -> Assertion
infEq as bs = let n = 10 in P.take n as @=? P.take n bs

-- | Convert specific edges of 'ClockEdgeAB' to 'ClockAB'.
toClockAB :: ActiveEdge -> ActiveEdge -> ClockEdgeAB -> Maybe ClockAB
toClockAB filterA filterB = go
 where
  go (ClockEdgeA edge) | edge == filterA = Just ClockA
  go (ClockEdgeB edge) | edge == filterB = Just ClockB
  go (ClockEdgeAB edgeA edgeB)
    | edgeA == filterA && edgeB == filterB = Just ClockAB
    | edgeA == filterA = Just ClockA
    | edgeB == filterB = Just ClockB
  go _ = Nothing

clockToActiveEdge :: forall dom. KnownDomain dom => Clock dom -> ActiveEdge
clockToActiveEdge _clk = case activeEdge @dom of
  SRising -> Rising
  SFalling -> Falling

-- | Convert specific edges of 'ClockEdgeAB' to 'ClockAB'.
toClockABs :: ActiveEdge -> ActiveEdge -> [ClockEdgeAB] -> [ClockAB]
toClockABs filterA filterB = catMaybes . P.map (toClockAB filterA filterB)

-- | Convert a list of relative event timestamps to a list of absolute timestamps
relativeToAbsolute :: [Int64] -> [Int64]
relativeToAbsolute = snd . L.mapAccumL (\acc t -> let new = acc + t in (new, new)) 0

-- | Convert a list of absolute event timestamps to a list of relative timestamps
absoluteToRelative :: [Int64] -> [Int64]
absoluteToRelative absoluteTimestamps =
  0 : P.zipWith (-) (P.tail absoluteTimestamps) absoluteTimestamps

unzipFirst :: ([a] -> [b]) -> [(a, c)] -> [(b, c)]
unzipFirst f (P.unzip -> (as, cs)) = P.zip (f as) cs

unzipSecond :: ([a] -> [b]) -> [(c, a)] -> [(c, b)]
unzipSecond f (P.unzip -> (cs, as)) = P.zip cs (f as)

-- | Check that 'clockTicksAbsolute' produces the same ratio of clock ticks as
-- @clash-prelude@'s 'clockTicks'
case_eqClockTicksAbsolute :: Assertion
case_eqClockTicksAbsolute =
  $(carthesianProductTests ["r61", "r107", "r122", "d61", "d107", "d122", "f61", "f107", "f122"])
 where
  test a b = fmap snd (clockTicksAbsolute a b) `infEq` clockTicks a b

-- | Check that 'clockTicksRelative' produces the same ratio of clock ticks as
-- @clash-prelude@'s 'clockTicks'
case_eqClockTicksRelative :: Assertion
case_eqClockTicksRelative =
  $(carthesianProductTests ["r61", "r107", "r122", "d61", "d107", "d122", "f61", "f107", "f122"])
 where
  test a b = fmap snd (clockTicksRelative a b) `infEq` clockTicks a b

-- | Check that 'clockEdgesAbsolute' produces the same ratio of clock ticks as
-- @clash-prelude@'s 'clockTicks'
case_eqClockEdgesAbsolute :: Assertion
case_eqClockEdgesAbsolute =
  $(carthesianProductTests ["r61", "r107", "r122", "d61", "d107", "d122", "f61", "f107", "f122"])
 where
  test a b = go a b (clockEdgesAbsolute a b) `infEq` clockTicks a b
  go a b = toClockABs (clockToActiveEdge a) (clockToActiveEdge b) . fmap snd

-- | Check that 'clockEdgesRelative' produces the same ratio of clock ticks as
-- @clash-prelude@'s 'clockTicks'
case_eqClockEdgesRelative :: Assertion
case_eqClockEdgesRelative =
  $(carthesianProductTests ["r61", "r107", "r122", "d61", "d107", "d122", "f61", "f107", "f122"])
 where
  test a b = go a b (clockEdgesRelative a b) `infEq` clockTicks a b
  go a b = toClockABs (clockToActiveEdge a) (clockToActiveEdge b) . fmap snd

-- | Check that 'clockEdgesAbsolute' produces the same ratio of clock ticks and
-- same timestamps as 'clockTicksAbsolute'.
case_eqClockEdgesTicksAbsolute :: Assertion
case_eqClockEdgesTicksAbsolute =
  $(carthesianProductTests ["r61", "r107", "r122", "d61", "d107", "d122", "f61", "f107", "f122"])
 where
  test a b = go a b (clockEdgesAbsolute a b) `infEq` clockTicksAbsolute a b

  go a b (P.unzip -> (times, edges)) =
    catMaybes (P.zipWith (liftA2 (,)) maybeTimes maybeEdges)
   where
    maybeTimes = Just <$> times
    maybeEdges = toClockAB (clockToActiveEdge a) (clockToActiveEdge b) <$> edges

-- | Check that 'clockEdgesRelative' produces the same ratio of clock ticks and
-- same timestamps as 'clockTicksRelative'.
case_eqClockEdgesTicksRelative :: Assertion
case_eqClockEdgesTicksRelative =
  $(carthesianProductTests ["r61", "r107", "r122", "d61", "d107", "d122", "f61", "f107", "f122"])
 where
  test a b = go a b (clockEdgesRelative a b) `infEq` clockTicksRelative a b

  go a b (P.unzip -> (relativeTimes, edges)) =
    unzipFirst
      absoluteToRelative
      (catMaybes (P.zipWith (liftA2 (,)) maybeAbsoluteTimes maybeEdges))
   where
    maybeAbsoluteTimes :: [Maybe Int64]
    maybeAbsoluteTimes = Just <$> absoluteTimes

    absoluteTimes :: [Int64]
    absoluteTimes = relativeToAbsolute relativeTimes

    maybeEdges :: [Maybe ClockAB]
    maybeEdges = toClockAB (clockToActiveEdge a) (clockToActiveEdge b) <$> edges

tests :: TestTree
tests = $(testGroupGenerator)
