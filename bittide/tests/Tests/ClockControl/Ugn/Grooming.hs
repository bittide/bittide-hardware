-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Tests.ClockControl.Ugn.Grooming where

import Prelude

import Bittide.ClockControl.Ugn.Grooming
import Bittide.Graph.Weighted (Graph, edges, fromEdges, weight)

import Hedgehog
import Test.Tasty
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.TH (testGroupGenerator)

import qualified Data.Map.Strict as Map
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- | A directed 3-cycle (slide-5 style): allowed iff the cycle sum is nonnegative.
triangle :: Integer -> Integer -> Integer -> Graph Int Integer
triangle a b c = fromEdges [(0, 1, a), (1, 2, b), (2, 0, c)]

{- | A bidirectional 3-node graph (slide-5 style): edges @0->1@, @1->2@, @2->0@ and
their reverses @1->0@, @2->1@, @0->2@. Allowed iff every cycle sum (both 2-cycles
and the two 3-cycles) is nonnegative.
-}
bidirTriangle ::
  Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Graph Int Integer
bidirTriangle a b c d e f =
  fromEdges [(0, 1, a), (1, 2, b), (2, 0, c), (1, 0, d), (2, 1, e), (0, 2, f)]

case_triangle_allowed :: Assertion
case_triangle_allowed = isAllowed (triangle 1 1 1) @?= True

case_triangle_allowed_mixed :: Assertion
case_triangle_allowed_mixed = isAllowed (triangle 5 (-2) (-1)) @?= True

case_triangle_disallowed :: Assertion
case_triangle_disallowed = isAllowed (triangle (-1) (-1) (-1)) @?= False

case_bidir_triangle_allowed :: Assertion
case_bidir_triangle_allowed =
  -- Every 2-cycle sum (a+d, b+e, c+f) and both 3-cycles are >= 0.
  isAllowed (bidirTriangle 0 1 0 1 0 1) @?= True

case_bidir_triangle_disallowed :: Assertion
case_bidir_triangle_disallowed =
  -- The 2-cycle 0<->1 sums to 1 + (-3) = -2 < 0.
  isAllowed (bidirTriangle 1 1 1 (-3) 1 1) @?= False

-- | Complete digraph over @0 .. k-1@ with the given per-edge weights.
completeFrom :: Int -> [Integer] -> Graph Int Integer
completeFrom k ws = fromEdges (zipWith (\(i, j) w -> (i, j, w)) (pairs k) ws)

pairs :: Int -> [(Int, Int)]
pairs k = [(i, j) | i <- [0 .. k - 1], j <- [0 .. k - 1], i /= j]

prop_groom_reaches_safe :: Property
prop_groom_reaches_safe = property $ do
  k <- forAll $ Gen.int (Range.linear 2 5)
  let ps = pairs k
      n = length ps
  lams <- forAll $ Gen.list (Range.singleton n) (Gen.integral (Range.linearFrom 0 (-50) 50))
  eps <- forAll $ Gen.list (Range.singleton n) (Gen.integral (Range.linear 0 20))
  let
    measured = completeFrom k lams
    safe = completeFrom k (zipWith (+) lams eps)
  case groomCorrection measured safe of
    Infeasible c -> annotateShow c >> failure
    Feasible{correction = q, framesToInsert = frames} -> do
      -- Every padding is nonnegative.
      assert (all (\(_, _, f) -> f >= 0) frames)
      -- Groom-then-pad reaches lambda^safe on every edge.
      let qOf m = Map.findWithDefault 0 m q
      frames === reaches measured safe q
      mapM_
        ( \(i, j, f) -> do
            lam <- maybe failure pure (weight i j measured)
            safeW <- maybe failure pure (weight i j safe)
            lam + qOf j - qOf i + f === safeW
        )
        frames
 where
  reaches measured safe q =
    [ (i, j, safeW - (lam + qf j - qf i))
    | (i, j, lam) <- edges measured
    , Just safeW <- [weight i j safe]
    ]
   where
    qf m = Map.findWithDefault 0 m q

prop_idempotent :: Property
prop_idempotent = property $ do
  -- lambda == lambda^safe: feasible with all-zero frames (no relabeling needed).
  k <- forAll $ Gen.int (Range.linear 2 5)
  let ps = pairs k
      n = length ps
  lams <- forAll $ Gen.list (Range.singleton n) (Gen.integral (Range.linearFrom 0 (-50) 50))
  let g = completeFrom k lams
  case groomCorrection g g of
    Infeasible c -> annotateShow c >> failure
    Feasible{framesToInsert = frames} ->
      assert (all (\(_, _, f) -> f == 0) frames)

case_infeasible_when_changed_too_much :: Assertion
case_infeasible_when_changed_too_much = do
  -- safe requires a cycle sum that is smaller than measured's: infeasible.
  let
    measured = triangle 0 0 0 -- cycle sum 0
    safe = triangle (-1) (-1) (-1) -- would need cycle sum -3 >= 0: impossible
  case groomCorrection measured safe of
    Infeasible _ -> pure ()
    Feasible{} -> fail "expected Infeasible"

tests :: TestTree
tests = $(testGroupGenerator)
