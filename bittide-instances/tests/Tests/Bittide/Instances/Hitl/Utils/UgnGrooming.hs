-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Tests.Bittide.Instances.Hitl.Utils.UgnGrooming where

import Prelude

import Clash.Prelude (BitVector, Index, Signed)

import Bittide.Graph.Weighted (edges, weight)
import Bittide.Instances.Hitl.Setup (LinkCount)
import Bittide.Instances.Hitl.Utils.Ugn (UgnEdge (..))
import Bittide.Instances.Hitl.Utils.UgnGrooming

import Data.List (sort)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH (testGroupGenerator)

-- * Helpers

mkEdge :: BitVector 32 -> Index LinkCount -> BitVector 32 -> Index LinkCount -> Signed 64 -> UgnEdge
mkEdge = UgnEdge

-- * Adapter unit tests

case_safeMargin_adds_eps :: Assertion
case_safeMargin_adds_eps = do
  let es = [mkEdge 0 0 1 0 10, mkEdge 1 0 0 0 (-4)]
  map (.ugn) (safeMargin 5 es) @?= [15, 1]

case_ugnGraph_roundtrip :: Assertion
case_ugnGraph_roundtrip = do
  let
    es = [mkEdge 0 0 1 0 7, mkEdge 1 0 0 0 (-3)]
    g = ugnGraph es
  sort (edges g) @?= sort [(0, 1, 7), (1, 0, -3)]
  weight 0 1 g @?= Just 7
  weight 1 0 g @?= Just (-3)

case_groomToSafe_feasible :: Assertion
case_groomToSafe_feasible = do
  -- A 2-cycle with sum 2 (allowed); pad to a uniform +5 margin.
  let
    measured = [mkEdge 0 0 1 0 3, mkEdge 1 0 0 0 (-1)]
    safe = safeMargin 5 measured
  case groomToSafe measured safe of
    UgnsChangedTooMuch ns -> assertFailure ("unexpected infeasible: " <> show ns)
    Groomed{frames} -> do
      assertBool "all frames >= 0" (all ((>= 0) . snd) frames)
      -- No relabeling needed for a uniform positive margin, so every pad == eps.
      map snd frames @?= [5, 5]

case_groomToSafe_infeasible :: Assertion
case_groomToSafe_infeasible = do
  -- measured 2-cycle sums to 0; the stored "safe" demands sum -2: impossible.
  let
    measured = [mkEdge 0 0 1 0 1, mkEdge 1 0 0 0 (-1)]
    safe = [mkEdge 0 0 1 0 0, mkEdge 1 0 0 0 (-2)]
  case groomToSafe measured safe of
    UgnsChangedTooMuch _ -> pure ()
    Groomed{} -> assertFailure "expected UgnsChangedTooMuch"

tests :: TestTree
tests = $(testGroupGenerator)
