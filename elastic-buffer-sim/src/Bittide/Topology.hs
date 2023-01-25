-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module defines the top entity for simulation, where every
--  non-empty topology is supported. The top entity can be simulated
--  using 'simulate'.
module Bittide.Topology
  ( simulationEntity
  , simulate
  , allStable
  )
where

import Clash.Prelude hiding (simulate)
import Clash.Signal.Internal
  ( Signal(..)
  , Clock(..)
  , Femtoseconds(..)
  )

import Data.Maybe (catMaybes)
import Data.Proxy (Proxy(..))
import Data.List qualified as L (take, drop)
import Control.DeepSeq (NFData, force)

import Bittide.Simulate
import Bittide.ClockControl
import Bittide.ClockControl.Callisto
import Bittide.ClockControl.ElasticBuffer
import Bittide.ClockControl.StabilityChecker
import Bittide.Topology.Graph

-- | The entity to be simulated consisting of the tunable clock
-- generators, the elastic buffers, the stability checkers, and the
-- clock controls, wired according to the given topology.
--
-- NOTE: All clocks are implicitly synchronized to the same clock
-- domain at this point, which is ok as long as the dynamic clock
-- generator is used. Otherwise the domains have to be differentiated
-- at the type level, which is not straightforward to archive for
-- fully connected topologies.
simulationEntity ::
  ( KnownDomain dom
  -- ^ domain
  , KnownNat nodes
  -- ^ the size of the topology is known
  , KnownNat dcount
  -- ^ the size of the data counts is known
  , KnownNat margin
  -- ^ the margins of the stability checker are known
  , KnownNat framesize
  -- ^ the frame size of cycles within the margins required is known
  , 1 <= nodes
  -- ^ the topology consists of at least one node
  , 1 <= dcount
  -- ^ data counts must contain data
  , nodes + dcount <= 32
  -- ^ computational limit of the clock control
  , 1 + nodes <= 32
  -- ^ computational limit of the clock control
  , 1 <= framesize
  -- ^ frames must at least cover one element
  ) =>
  Graph nodes ->
  -- ^ the topology
  ClockControlConfig dom dcount ->
  -- ^ clock control configuration
  SNat margin ->
  -- ^ margin of the stability checker
  SNat framesize ->
  -- ^ frame size of cycles within the margins required
  Vec nodes Offset ->
  -- ^ initial clock offsets
  Signal dom (Vec nodes (Period, Vec nodes (DataCount dcount, Bool)))
  -- ^ simulation entity
simulationEntity topology ccc margin framesize !offsets =
    bundle
  $ zipWith (curry bundle) clkSignals
  $ bundle <$> zipWith (zipWith (curry bundle)) ebs scs
 where
  -- elastic buffers
  !ebs = imap ebv clocks
  ebv x = flip imap clocks . eb x
  eb x xClk y yClk
    | hasEdge topology x y = elasticBuffer Error xClk yClk
    | otherwise            = pure 0
  -- stability checkers
  !scs = imap (\i (v, clk) -> imap (sc i clk) v) $ zip ebs clocks
  sc x clk y
    | hasEdge topology x y =
        withClockResetEnable clk resetGen enableGen $
          stabilityChecker margin framesize
    | otherwise     = const $ pure False
  -- clock generators
  !clocks = clock <$> offsets <*> clockControls
  clock offset =
    tunableClockGen
      (cccSettlePeriod ccc)
      offset
      (cccStepSize ccc)
      resetGen
  -- clock controls
  !clockControls = clockControl <$> clocks <*> masks <*> ebs
  clockControl clk =
    callistoClockControl
      clk
      resetGen
      enableGen
      ccc
    . pure
  -- clock signals
  !clkSignals = extractPeriods <$> clocks
  -- available link mask vectors
  !masks = nVec (v2bv . nVec . avail)
  avail x y = if hasEdge topology x y then high else low
  nVec = flip map indicesI

-- | Simulates some topology simulation entity.
simulate ::
  ( KnownDomain dom
  -- ^ domain
  , KnownNat nodes
  -- ^ the size of the topology is known
  , KnownNat dcount
  -- ^ the size of the data counts is known
  ) =>
  Graph nodes ->
  -- ^ the topology
  Bool ->
  -- ^ stop simulation as soon as all buffers get stable
  Int ->
  -- ^ number of samples to keep & pass
  Int ->
  -- ^ number of cycles in one sample period
  Signal dom (Vec nodes (Period, Vec nodes (DataCount dcount, Bool))) ->
  -- ^ simulation entity
  Vec nodes [(Period, Period, [(DataCount dcount, Bool)])]
simulate topology stopWhenStable samples periodsize =
    transposeLV
  . takeWhilePlus unstable
  . L.take samples
  . takeEveryN periodsize
  . absTimes topology
 where
  unstable
    | stopWhenStable = not . allStable
    | otherwise      = const True

  takeWhilePlus p = \case
    []   -> []
    x:xs -> if p x then x : takeWhilePlus p xs else [x]

-- | Checks whether all stability checkers report a stable result.
allStable :: KnownNat n => Vec n (a, b, [(c, Bool)]) -> Bool
allStable = and . toList . map ((\(_,_,xs) -> all snd xs))

-- | Absolute time unfolding of the produced signal for generating the
-- simulation data.
absTimes ::
  (KnownNat nodes, NFData a) =>
  Graph nodes ->
  -- ^ the topology
  Signal dom (Vec nodes (Period, Vec nodes a)) ->
  -- ^ The signal holding the the simulation result
  [Vec nodes (Period, Period, [a])]
  -- ^ The same data as in the input signal, only lazily unfolded as
  -- an infinite data stream and with the unavailable links
  -- already thrown out from the last tuple member.
absTimes topology = go $ replicate SNat (Femtoseconds 0)
 where
  go !ts (v :- vs) =
    force (izipWith (\i t (p, es) -> (t, p, filterAvailable i es)) ts v)
      : go (force $ zipWith addFs ts $ map fst v) vs
  -- turns a fixed sized vector of data corresponding to the topology
  -- links to a list of data entries, reduced to the available links
  filterAvailable i =
    catMaybes . toList . imap (asMaybe . hasEdge topology i)
  asMaybe = \case
    True  -> Just
    False -> const Nothing

-- | 'L.transpose' for (possibly infinite) lists of vectors
transposeLV :: KnownNat n => [Vec n a] -> Vec n [a]
transposeLV = \case
  []   -> replicate SNat []
  x:xs -> (:) <$> x <*> transposeLV xs

-- | Extracts the time periods from a clock
extractPeriods ::
  forall dom. KnownDomain dom =>
  Clock dom ->
  Signal dom Period
extractPeriods = \case
  (Clock _ (Just s)) -> s
  _                  -> pure (clockPeriodFs @dom Proxy)

-- | As an example:
--
-- >>> takeEveryN 3 [1..10]
-- [1,4,7,10]
takeEveryN :: Int -> [a] -> [a]
takeEveryN n = \case
  []     -> []
  (x:xs) -> x : takeEveryN n (L.drop (n - 1) xs)
