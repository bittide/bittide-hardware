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
  , allSettled
  )
where

import Clash.Prelude hiding (simulate)
import Clash.Signal.Internal
  ( Signal(..)
  , Clock(..)
  , Femtoseconds(..)
  )

import Data.Maybe
import Data.Proxy (Proxy(..))
import Data.List qualified as L (take, drop, repeat, replicate)

import Bittide.Simulate
import Bittide.ClockControl
import Bittide.ClockControl.Callisto hiding (allSettled)
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
  forall dom nodes dcount margin framesize.
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
  ClockControlConfig dom dcount margin framesize ->
  -- ^ clock control configuration
  Vec nodes Offset ->
  -- ^ initial clock offsets
  Vec nodes Int ->
  -- ^ initial startup offsets
  Signal dom
    ( Vec nodes
        ( Period
        , ReframingState
        , Vec nodes
            ( DataCount dcount
            , StabilityIndication
            )
        )
    )
  -- ^ simulation entity
simulationEntity topology ccc !clockOffsets !startupOffsets =
  bundle
  $ zipWith3 (\x y z -> bundle (x, y, z))
      clkSignals
      (fmap reframingState <$> callistoResults)
      $ zipWith
          (liftA2 zip)
          (bundle <$> ebs)
          (fmap stability <$> callistoResults)
 where
  -- node specific resets according to the startup offsets
  rsts :: Vec nodes (Reset dom)
  !rsts = resetGenN' <$> startupOffsets

  -- elastic buffers
  ebs :: Vec nodes (Vec nodes (Signal dom (DataCount dcount)))
  !ebs = imap ebv clocks
  ebv x = flip imap clocks . eb x
  eb x xClk y yClk
    | hasEdge topology x y = elasticBuffer xClk yClk
    | otherwise            = pure 0

  -- clock generators
  !clocks = clock <$> clockOffsets <*> rsts <*> (fmap (fromMaybe NoChange . maybeSpeedChange) <$> callistoResults)
  clock offset rst =
    tunableClockGen
      (cccSettlePeriod ccc)
      offset
      (cccStepSize ccc)
      rst

  -- clock controls
  callistoResults :: Vec nodes (Signal dom (CallistoResult nodes))
  !callistoResults =
     clockControl <$> clocks <*> rsts <*> masks <*> ebs
  clockControl clk rst =
    callistoClockControl
      clk
      rst
      enableGen
      ccc
    . pure

  -- clock signals
  clkSignals :: Vec nodes (Signal dom Period)
  !clkSignals = extractPeriods <$> clocks

  -- available link mask vectors
  masks :: Vec nodes (BitVector nodes)
  !masks = nVec (v2bv . nVec . avail)
  avail x y = if hasEdge topology x y then high else low
  nVec = flip map indicesI

  -- value level version of 'Clash.Signal.Internal.resetGenN' without
  -- a the need for a blackbox
  resetGenN' n = unsafeFromActiveHigh $
    fromList (L.replicate n True <> L.repeat False)

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
  Maybe Int ->
  -- ^ stop simulation after all buffers have been stable for @n@ steps
  Int ->
  -- ^ number of samples to keep & pass
  Int ->
  -- ^ number of cycles in one sample period
  Signal dom
    ( Vec nodes
        ( Period
        , ReframingState
        , Vec nodes
            ( DataCount dcount
            , StabilityIndication
            )
        )
    ) ->
  -- ^ simulation entity
  Vec nodes
    [ ( Period
      , Period
      , ReframingState
      , [ ( DataCount dcount
          , StabilityIndication
          )
        ]
      )
    ]
simulate topology stopStable samples periodsize =
    transposeLV
  . takeWhileDelay stopStable (-1)
  . L.take samples
  . takeEveryN periodsize
  . absTimes topology
 where
  takeWhileDelay = \case
    Nothing -> const id
    Just n  -> \m -> \case
      []     -> []
      x : xr ->
        let m' | not (allSettled x) = -1
               | m < 0                        = n
               | m > 0                        = m - 1
               | otherwise                    = 0
        in x : if m' == 0 then [] else takeWhileDelay (Just n) m' xr

-- | Checks whether all stability checkers report a stable result.
allSettled :: KnownNat n => Vec n (a, b, c, [(d, StabilityIndication)]) -> Bool
allSettled = and . toList . map ((\(_,_,_,xs) -> all (settled . snd) xs))

-- | Absolute time unfolding of the produced signal for generating the
-- simulation data.
absTimes ::
  (KnownNat nodes, NFDataX a, NFDataX b) =>
  Graph nodes ->
  -- ^ the topology
  Signal dom (Vec nodes (Period, b, Vec nodes a)) ->
  -- ^ The signal holding the the simulation result
  [Vec nodes (Period, Period, b, [a])]
  -- ^ The same data as in the input signal, only lazily unfolded as
  -- an infinite data stream and with the unavailable links
  -- already thrown out from the last tuple member.
absTimes topology = go $ replicate SNat (Femtoseconds 0)
 where
  go !ts (v :- vs) =
    forceX (izipWith (\i t (p, s, es) -> (t, p, s, filterAvailable i es)) ts v)
      : go (forceX $ zipWith addFs ts $ map (\(x,_,_) -> x) v) vs
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
