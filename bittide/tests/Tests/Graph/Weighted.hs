-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Tests.Graph.Weighted where

import Prelude

import Bittide.Graph.Weighted

import Data.List (sort)
import Data.Maybe (isJust)
import Hedgehog
import Test.Tasty
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.TH (testGroupGenerator)

import qualified Data.Map.Strict as Map
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- | A small directed weighted graph used across several tests.
sampleEdges :: [(Int, Int, Int)]
sampleEdges = [(0, 1, 5), (1, 2, 3), (2, 0, 1), (0, 2, 9)]

sampleGraph :: Graph Int Int
sampleGraph = fromEdges sampleEdges

case_fromEdges_edges_roundtrip :: Assertion
case_fromEdges_edges_roundtrip = sort (edges sampleGraph) @?= sort sampleEdges

case_counts :: Assertion
case_counts = (nodeCount sampleGraph, edgeCount sampleGraph) @?= (3, 4)

case_addEdge_overwrites :: Assertion
case_addEdge_overwrites =
  weight 0 1 (addEdge 0 1 42 sampleGraph) @?= Just 42

case_outgoing_incoming :: Assertion
case_outgoing_incoming = do
  sort (outgoing 0 sampleGraph) @?= [(1, 5), (2, 9)]
  sort (incoming 0 sampleGraph) @?= [(2, 1)]

case_weight_hasEdge :: Assertion
case_weight_hasEdge = do
  weight 1 2 sampleGraph @?= Just 3
  weight 2 1 sampleGraph @?= Nothing
  hasEdge 2 0 sampleGraph @?= True
  hasEdge 0 0 sampleGraph @?= False

case_isolated_node_survives_transpose :: Assertion
case_isolated_node_survives_transpose =
  -- An edge 0 -> 1 makes node 1 a sink; after transpose it must still appear.
  sort (nodes (transpose (fromEdges [(0 :: Int, 1 :: Int, 7 :: Int)]))) @?= [0, 1]

-- | Generate a random directed graph over nodes @0 .. k-1@ (no self loops).
genGraph :: Gen (Graph Int Int)
genGraph = do
  k <- Gen.int (Range.linear 1 6)
  let allPairs = [(i, j) | i <- [0 .. k - 1], j <- [0 .. k - 1], i /= j]
  chosen <- Gen.subsequence allPairs
  fmap fromEdges $
    traverse
      (\(i, j) -> (\w -> (i, j, w)) <$> Gen.int (Range.linearFrom 0 (-50) 50))
      chosen

prop_transpose_involution :: Property
prop_transpose_involution = property $ do
  g <- forAll genGraph
  transpose (transpose g) === g

prop_relabel_preserves_cycle_sums :: Property
prop_relabel_preserves_cycle_sums = property $ do
  -- A fixed 3-cycle whose every edge exists, with random weights and potentials.
  let cyc = [0, 1, 2] :: [Int]
  ws <- forAll $ Gen.list (Range.singleton 3) (Gen.int (Range.linearFrom 0 (-50) 50))
  let g = fromEdges [(0, 1, ws !! 0), (1, 2, ws !! 1), (2, 0, ws !! 2)]
  qs <- forAll $ Gen.list (Range.singleton 3) (Gen.int (Range.linearFrom 0 (-50) 50))
  let q = Map.fromList (zip [0, 1, 2] qs)
  cycleSum (relabel q g) cyc === cycleSum g cyc

case_negative_cycle_detected :: Assertion
case_negative_cycle_detected = do
  let g = fromEdges [(0 :: Int, 1 :: Int, -1 :: Int), (1, 2, -1), (2, 0, -1)]
  case negativeCycle g of
    Nothing -> fail "expected a negative cycle"
    Just c -> do
      -- The witness is a genuine cycle, and its sum is negative.
      sort c @?= [0, 1, 2]
      ((< 0) <$> cycleSum g c) @?= Just True
  isAllowedSanity g @?= False
 where
  -- mirror of Grooming.isAllowed at the graph level
  isAllowedSanity gr = case negativeCycle gr of Nothing -> True; Just _ -> False

case_positive_cycle_has_potentials :: Assertion
case_positive_cycle_has_potentials = do
  let g = fromEdges [(0 :: Int, 1 :: Int, 1 :: Int), (1, 2, 1), (2, 0, 1)]
  isJust (potentials g) @?= True

case_disconnected_graph_has_potentials :: Assertion
case_disconnected_graph_has_potentials = do
  -- Two independent components; the super-source handles each one.
  let g = fromEdges [(0 :: Int, 1 :: Int, 3 :: Int), (10, 11, -4), (11, 10, 9)]
  case potentials g of
    Nothing -> fail "expected potentials to exist"
    Just q -> sort (Map.keys q) @?= [0, 1, 10, 11]

case_zipEdgesWith_intersection :: Assertion
case_zipEdgesWith_intersection = do
  let
    a = fromEdges [(0 :: Int, 1 :: Int, 10 :: Int), (1, 2, 20)]
    b = fromEdges [(0, 1, 1), (2, 0, 99)]
    c = zipEdgesWith (-) a b
  -- Only the shared edge 0 -> 1 survives, with combined weight.
  sort (edges c) @?= [(0, 1, 9)]
  -- Node set is the union.
  sort (nodes c) @?= [0, 1, 2]

tests :: TestTree
tests = $(testGroupGenerator)
