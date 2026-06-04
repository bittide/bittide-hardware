-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{- |
UGN grooming, expressed on top of "Bittide.Graph.Weighted".

UGNs (@\lambda_{i->j}@) are signed per-link clock/frame offsets, i.e. edge weights
on a directed graph of nodes. They cannot be chosen independently: the sum around
every directed cycle must be nonnegative (the physical round-trip constraint).

This module provides:

  * 'isAllowed' — whether a set of measured UGNs is physically realisable
    (equivalently: no negative cycle on weights = @\lambda@).

  * 'groomCorrection' — given a freshly measured @\lambda@ and a previously stored
    @\lambda^{safe} = \lambda^{obs} + \varepsilon@, find a relabeling @q@ and the
    frames to insert so the system is restored to @\lambda^{safe}@, or report that
    the UGNs changed too much.

The math (see the UGN grooming slides): with slack
@c_{i->j} = \lambda^{safe}_{i->j} - \lambda_{i->j}@, the feasibility condition
@q_j - q_i <= c_{i->j}@ is a difference-constraint system, solved by Bellman-Ford
from a virtual super-source. It is feasible iff the slack graph has no negative
cycle, and the resulting potentials @q@ satisfy the per-edge constraint, so the
padding @\lambda^{safe} - (\lambda + q_j - q_i)@ is guaranteed nonnegative.
-}
module Bittide.ClockControl.Ugn.Grooming (
  isAllowed,
  GroomResult (..),
  groomCorrection,
) where

import Prelude

import Data.Map.Strict (Map)
import Data.Maybe (isNothing)

import Bittide.Graph.Weighted

import qualified Data.Map.Strict as Map

{- | Are these UGNs allowed? UGNs are allowed iff the sum around every directed
cycle is nonnegative, equivalently iff the graph (weights = @\lambda@) has no
negative cycle.
-}
isAllowed :: (Ord n, Ord w, Num w) => Graph n w -> Bool
isAllowed = isNothing . negativeCycle

-- | Outcome of 'groomCorrection'.
data GroomResult n w
  = {- | The slack graph has a negative cycle (UGNs changed too much); the
    witnessed cycle is included for diagnostics.
    -}
    Infeasible [n]
  | {- | A feasible restore: relabel by 'correction', then insert
    'framesToInsert' frames per edge to reach @\lambda^{safe}@.
    -}
    Feasible
      { correction :: Map n w
      , framesToInsert :: [(n, n, w)]
      }
  deriving (Show, Eq)

{- | Find the correction restoring measured UGNs @\lambda@ to the stored safe UGNs
@\lambda^{safe}@.

The slack graph has edge weights @\lambda^{safe}_{i->j} - \lambda_{i->j}@; its
Bellman-Ford potentials give the relabeling @q@. The frames to insert on each edge
are @\lambda^{safe}_{i->j} - (\lambda_{i->j} + q_j - q_i)@, which are @>= 0@ by the
shortest-path relaxation invariant.

Edges are matched between the two graphs by 'zipEdgesWith'; only edges present in
both are considered.
-}
groomCorrection ::
  (Ord n, Ord w, Num w) =>
  -- | Measured @\lambda@
  Graph n w ->
  -- | Stored @\lambda^{safe}@
  Graph n w ->
  GroomResult n w
groomCorrection measured safe =
  case bellmanFord slack of
    NegativeCycle c -> Infeasible c
    Distances q ->
      Feasible
        { correction = q
        , framesToInsert =
            [ (i, j, safeW - (lam + qOf q j - qOf q i))
            | (i, j, lam) <- edges measured
            , Just safeW <- [weight i j safe]
            ]
        }
 where
  slack = zipEdgesWith (\lam safeW -> safeW - lam) measured safe
  qOf q n = Map.findWithDefault 0 n q
