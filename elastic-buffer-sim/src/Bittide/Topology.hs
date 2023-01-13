-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MagicHash #-}

-- | This module generates a static topology using template haskell and then
-- dumps clock periods and elastic buffer occupancy to csv.
module Bittide.Topology
  ( dumpCsv
  , plotEbs
  , plotHypercube
  , plotHypercube4
  , plotTorus34
  , plotK2
  , plotK3
  , plotK6
  , plotC12
  , plotDiamond
  , plotTree32
  , plotTree23
  , plotStar7
  , simulateMesh
  , plotMesh
  , absTimes
  )
where

import Data.Maybe (catMaybes)
import Control.Monad (void, zipWithM_, forM_)
import Data.Bifunctor (bimap)
import Data.Csv (toField, encode)
import Data.Graph (edges)
import Data.Proxy
import System.Directory (createDirectoryIfMissing)
import Graphics.Matplotlib (Matplotlib, (%), file, plot, xlabel, ylabel)
import System.Random (randomRIO)

import Prelude qualified as P
import Data.List qualified as L
import Data.Array qualified as A
import Data.ByteString.Lazy qualified as BSL

import Clash.Explicit.Prelude
import Clash.Explicit.Prelude qualified as Clash
import Clash.Signal.Internal qualified as Clash

import Bittide.Simulate
import Bittide.Arithmetic.Ppm
import Bittide.ClockControl
import Bittide.ClockControl.Callisto
import Bittide.ClockControl.ElasticBuffer
import Bittide.Topology.Graph
import Graphics.Matplotlib.Ext
import Bittide.Topology.TH.Domain (defBittideClockConfig)
import Control.DeepSeq

-- | A 'Graph' with a name and a link availability check
data Mesh (n :: Nat) =
  Mesh
    { name :: String
    , graph :: Graph n
    , available :: Index n -> Index n -> Bool
    }

-- | Smart 'Mesh' constructor
mesh :: KnownNat n => String -> Graph n -> Mesh n
mesh name graph = Mesh name graph available
 where
  available = curry $ (A.!) $
    A.accumArray
      (const id)
      False
      ((minBound, minBound), (maxBound, maxBound))
      (P.zip (filter (uncurry (/=)) edgeIndices) [True, True ..])
  edgeIndices =
    fmap (bimap fromIntegral fromIntegral)
      $ edges
      $ unboundGraph graph

-- | This samples @n@ steps, taking every @k@th datum, and plots clock speeds
-- and elastic buffer occupancy
plotEbs :: Int -> Int -> IO ()
plotEbs = plotC12

plotDiamond :: Int -> Int -> IO ()
plotDiamond = plotMesh $ mesh "diamond" diamond

plotHypercube :: Int -> Int -> IO ()
plotHypercube = plotMesh $ mesh "hypercube3" $ hypercube (SNat @3)

plotHypercube4 :: Int -> Int -> IO ()
plotHypercube4 = plotMesh $ mesh "hypercube4" $ hypercube (SNat @4)

plotTorus34 :: Int -> Int -> IO ()
plotTorus34 = plotMesh $ mesh "torus34" $ torus2d (SNat @3) (SNat @4)

plotK2 :: Int -> Int -> IO ()
plotK2 = plotMesh $ mesh "complete2" $ complete (SNat @2)

plotK3 :: Int -> Int -> IO ()
plotK3 = plotMesh $ mesh "complete3" $ complete (SNat @3)

plotK6 :: Int -> Int -> IO ()
plotK6 = plotMesh $ mesh "complete6" $ complete (SNat @6)

plotC12 :: Int -> Int -> IO ()
plotC12 = plotMesh $ mesh "cyclic12" $ cyclic (SNat @12)

plotStar7 :: Int -> Int -> IO ()
plotStar7 = plotMesh $ mesh "star7" $ star (SNat @7)

plotTree32 :: Int -> Int -> IO ()
plotTree32 = plotMesh $ mesh "tree32" $ tree (SNat @3) (SNat @2)

plotTree23 :: Int -> Int -> IO ()
plotTree23 = plotMesh $ mesh "tree23" $ tree (SNat @2) (SNat @3)

-- | This samples @n@ steps, taking every @k@th datum; the result can be fed to
-- @script.py@
dumpCsv :: Int -> Int -> IO ()
dumpCsv m k = do
  offs <- genOffs
  createDirectoryIfMissing True "_build"
  forM_ [0..n] $ \i ->
    let eb = unboundGraph (graph g) A.! i in
    writeFile
      ("_build/clocks" <> show i <> ".csv")
      ("t,clk" <> show i <> P.concatMap (\j -> ",eb" <> show i <> show j) eb <>  "\n")
  let dats = map (encode . fmap flatten) $ simulateMesh defBittideClockConfig g m k offs
  zipWithM_
    (\dat i -> BSL.appendFile ("_build/clocks" <> show i <> ".csv") dat)
    (toList dats)
    [(0 :: Int)..]
 where
  flatten (a, b, v) = toField a : toField b : (toField <$> v)
  (0, n) = A.bounds $ unboundGraph $ graph g
  g = mesh "complete" $ complete (SNat @6)

-- | Creates a and write the plots for a given mesh of type
--
-- > Int -> Int -> IO ()
--
-- for a given mesh, which writes/dumps simulation results for a
-- particular graph.
plotMesh ::
  forall n.
  (KnownNat n, 1 <= n, n <= 20) =>
  Mesh n ->
  Int ->
  Int ->
  IO ()
plotMesh api m k = do
  offs <- genOffs
  uncurry (matplotWrite api)
    $ unzip
    $ map plotDats
    $ simulateMesh defBittideClockConfig api m k offs
 where
  plotDats =
      bimap
        (uncurry plot . P.unzip)
        (foldPlots . P.fmap (uncurry plot . P.unzip) . L.transpose)
    . P.unzip
    . fmap (\(x,y,z) -> ((x,y), (x,) <$> z))

-- | Given a mesh with \(n\) vertices, creates a function which takes
-- a bound @m@ for the number of samples, a bound @k@ for the sampling
-- period, a vector of \(n\) offsets (divergence from spec) and a
-- vertex @i@ selected for simulation.
--
-- NOTE: All clocks are implicitly synchronized to the same clock
-- domain at this point, which is ok as long as the dynamic clock
-- generator is used. Otherwise the domains have to be differentiated
-- at the type level, which is not straightforward to archive for fully
-- connected topologies.
simulateMesh ::
  forall n m dom.
  ( Clash.KnownDomain dom
  , KnownNat n
  , KnownNat m
  , 1 <= n
  , 1 <= m
  , n + m <= 32
  , 1 + n <= 32
  ) =>
  ClockControlConfig dom m ->
  Mesh n ->
  Int ->
  Int ->
  Vec n Offset ->
  Vec n [(Period, Period, [DataCount m])]
simulateMesh ccc Mesh{..} m k !offsets =
    transposeLV
  $ P.take m
  $ takeEveryN k
  $ absTimes available
  $ bundle
  $ zipWith (curry bundle) clkSignals (bundle <$> ebs)
 where
  -- elastic buffers
  !ebs = imap ebv clocks
  ebv x = flip imap clocks . eb x
  eb x xClk y yClk
    | available x y = elasticBuffer Error xClk yClk
    | otherwise     = pure 0
  -- clocks
  !clocks = clock <$> offsets <*> clockControls
  clock offset =
    tunableClockGen
      (cccSettlePeriod ccc)
      offset
      (cccStepSize ccc)
      Clash.resetGen
  -- clock controls
  !clockControls = clockControl <$> clocks <*> masks <*> ebs
  clockControl clk =
    callistoClockControl
      clk
      Clash.resetGen
      Clash.enableGen
      ccc
    . pure
  -- clock signals
  !clkSignals = extractPeriods <$> clocks
  -- available link mask vectors
  !masks = nVec (v2bv . nVec . avail)
  avail x y = if available x y then high else low
  nVec = flip map indicesI

-- | Absolute time unfolding of the produced signal for generating the
-- simulation data.
absTimes ::
  (KnownNat n, NFData a) =>
  (Index n -> Index n -> Bool) ->
  -- ^ Available links according to the graph topology
  Signal dom (Vec n (Period, Vec n a)) ->
  -- ^ The signal holding the the simulation result
  [Vec n (Period, Period, [a])]
  -- ^ The same data as in the input signal, only lazily unfolded as
  -- an infinite data stream and with the unavailable links
  -- already thrown out from the last tuple member.
absTimes available = go $ replicate SNat (Clash.Femtoseconds 0)
 where
  go !ts (v Clash.:- vs) =
    force (izipWith (\i t (p, es) -> (t, p, filterAvailable i es)) ts v)
      : go (force $ zipWith addFs ts $ map fst v) vs
  -- turns a fixed sized vector of data corresponding to the topology
  -- links to a list data entries reduced to the available links
  filterAvailable i = catMaybes . toList . imap (asMaybe . available i)
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
  forall dom. Clash.KnownDomain dom =>
  Clash.Clock dom ->
  Clash.Signal dom Period
extractPeriods = \case
  (Clash.Clock _ (Just s)) -> s
  _                        -> pure (clockPeriodFs @dom Proxy)

-- | Randomly generate a 'Offset', how much a real clock's period may differ
-- from its spec.
genOffsets ::
  forall dom n.
  Clash.KnownDomain dom =>
  ClockControlConfig dom n ->
  IO Offset
genOffsets ClockControlConfig{cccDeviation} = do
  offsetPpm <- randomRIO (-cccDeviation, cccDeviation)
  pure (diffPeriod offsetPpm (clockPeriodFs @dom Proxy))

-- | Folds the vectors of generated plots and writes the results to
-- the disk.
matplotWrite ::
  KnownNat n =>
  Mesh n ->
  -- ^ the `Mesh` the data is generated for
  Vec n Matplotlib ->
  -- ^ clock plots
  Vec n Matplotlib ->
  -- ^ elastic buffer plots
  IO ()
matplotWrite Mesh{..} clockDats ebDats = do
  createDirectoryIfMissing True "_build"
  void $
    file
      ("_build/clocks" P.++ name P.++ ".pdf")
      ( xlabel "Time (fs)"
      % ylabel "Relative period (fs) [0 = ideal frequency]"
      % foldPlots (toList clockDats)
      )
  void $
    file
      ("_build/elasticbuffers" P.++ name P.++ ".pdf")
      (xlabel "Time (fs)" % foldPlots (toList ebDats))

-- | As an example:
--
-- >>> takeEveryN 3 [1..10]
-- [1,4,7,10]
takeEveryN :: Int -> [a] -> [a]
takeEveryN n = \case
  []     -> []
  (x:xs) -> x : takeEveryN n (P.drop (n - 1) xs)

genOffs :: forall n. KnownNat n => IO (Vec n Offset)
genOffs = traverse# genOffsets $ replicate SNat defBittideClockConfig
