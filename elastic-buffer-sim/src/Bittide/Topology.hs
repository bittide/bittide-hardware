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
  , simNodesFromGraph
  , plotEbsAPI
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

-- | 'Graph' with a name for
data GraphAPI (n :: Nat) =
  GraphAPI
    { name :: String
    , graph :: Graph n
    , available :: Index n -> Index n -> Bool
    }

setupGraph :: KnownNat n => String -> Graph n -> GraphAPI n
setupGraph name graph = GraphAPI name graph available
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
      $ unboundedGraph graph

-- | This samples @n@ steps, taking every @k@th datum, and plots clock speeds
-- and elastic buffer occupancy
plotEbs :: Int -> Int -> IO ()
plotEbs = plotC12

plotDiamond :: Int -> Int -> IO ()
plotDiamond = plotEbsAPI $ setupGraph "diamond" diamond

plotHypercube :: Int -> Int -> IO ()
plotHypercube = plotEbsAPI $ setupGraph "hypercube3" $ hypercube (SNat @3)

plotHypercube4 :: Int -> Int -> IO ()
plotHypercube4 = plotEbsAPI $ setupGraph "hypercube4" $ hypercube (SNat @4)

plotTorus34 :: Int -> Int -> IO ()
plotTorus34 = plotEbsAPI $ setupGraph "torus34" $ torus2d (SNat @3) (SNat @4)

plotK2 :: Int -> Int -> IO ()
plotK2 = plotEbsAPI $ setupGraph "complete2" $ complete (SNat @2)

plotK3 :: Int -> Int -> IO ()
plotK3 = plotEbsAPI $ setupGraph "complete3" $ complete (SNat @3)

plotK6 :: Int -> Int -> IO ()
plotK6 = plotEbsAPI $ setupGraph "complete6" $ complete (SNat @6)

plotC12 :: Int -> Int -> IO ()
plotC12 = plotEbsAPI $ setupGraph "cyclic12" $ cyclic (SNat @12)

plotStar7 :: Int -> Int -> IO ()
plotStar7 = plotEbsAPI $ setupGraph "star7" $ star (SNat @7)

plotTree32 :: Int -> Int -> IO ()
plotTree32 = plotEbsAPI $ setupGraph "tree32" $ tree (SNat @3) (SNat @2)

plotTree23 :: Int -> Int -> IO ()
plotTree23 = plotEbsAPI $ setupGraph "tree23" $ tree (SNat @2) (SNat @3)

-- | This samples @n@ steps, taking every @k@th datum; the result can be fed to
-- @script.py@
dumpCsv :: Int -> Int -> IO ()
dumpCsv m k = do
  offs <- genOffs
  createDirectoryIfMissing True "_build"
  forM_ [0..n] $ \i ->
    let eb = unboundedGraph (graph g) A.! i in
    writeFile
      ("_build/clocks" <> show i <> ".csv")
      ("t,clk" <> show i <> P.concatMap (\j -> ",eb" <> show i <> show j) eb <>  "\n")
  let
    dats = flip map indicesI
      $ encode . fmap flatten . simNodesFromGraph defBittideClockConfig g m k offs
  zipWithM_
    (\dat i -> BSL.appendFile ("_build/clocks" <> show i <> ".csv") dat)
    (toList dats)
    [(0 :: Int)..]
 where
  flatten (a, b, v) = toField a : toField b : (toField <$> v)
  (0, n) = A.bounds $ unboundedGraph $ graph g
  g = setupGraph "complete" $ complete (SNat @6)

-- | Given a 'Graph', create an expression of type
--
-- > Int -> Int -> IO ()
--
-- which writes/dumps simulation results for a particular graph.
plotEbsAPI ::
  forall n.
  (KnownNat n, 1 <= n, n <= 20) =>
  GraphAPI n ->
  Int ->
  Int ->
  IO ()
plotEbsAPI api@GraphAPI{..} m k = do
  offs <- genOffs
  uncurry (matplotWrite name)
    $ unzip
    $ flip map indicesI
    $ plotDats . simNodesFromGraph defBittideClockConfig api m k (force offs)
 where
  plotDats =
      force
    . bimap
        (uncurry plot . P.unzip)
        (foldPlots . P.fmap (uncurry plot . P.unzip) . L.transpose)
    . P.unzip
    . fmap (\(x,y,z) -> ((x,y), (x,) <$> z))

-- | Given a graph with \(n\) nodes, generate a function which takes
-- an array of \(n\) offsets (divergence from spec) and returns a
-- tuple of signals for each clock domain
--
-- NOTE: All clocks are implicitly synchronized to the same clock
-- domain at this point, which is ok as long as the dynamic clock
-- generator is used. Otherwise the domains have to be differentiated
-- at the type level, which is not straightforward to archive for fully
-- connected topologies.
simNodesFromGraph ::
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
  GraphAPI n ->
  Int ->
  Int ->
  Vec n Offset ->
  Index n ->
  [(Period, Period, [DataCount m])]
simNodesFromGraph ccc GraphAPI{..} m k !offsets i =
    P.take m
  $ takeEveryN k
  $ absTimes (available i)
  $ bundle (clkSignals !! i, bundle $ ebs !! i)
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

-- absolute time simulation
absTimes ::
  (KnownNat n, NFData a) =>
  (Index n -> Bool) ->
  Signal dom (Period, Vec n a) ->
  [(Period, Period, [a])]
absTimes !available = go $ Clash.Femtoseconds 0
 where
  go !t ((p, es) Clash.:- xs) =
    force (t, p, filterAvailable es) : go (addFs t p) xs
  filterAvailable = catMaybes . toList . imap (asMaybe . available)
  asMaybe = \case
    True  -> Just
    False -> const Nothing
--  absTimes =
--    go $ replicate SNat (Clash.Femtoseconds 0)
--  go ts v =
--    force (izipWith (\i t (p, es) -> (t, p, filterAvailable i es)) ts (map Clash.head# v)) :
--    go (zipWith addFs (force ts) (map (fst . Clash.head#) v))
--      (fmap Clash.tail# v)



--pointwise :: (KnownNat n, NFData a) => [Vec n a] -> Vec n [a]
--pointwise = pw $ replicate SNat []
--  where
--    pw !a = \case
--      []   -> map P.reverse a
--      x:xr -> pw ((:) <$> force x <*> a) xr
--  \case
--    []   -> replicate SNat []
--    x:xs -> (:) <$> force x <*> pointwise xs

extractPeriods ::
  forall dom. Clash.KnownDomain dom =>
  Clash.Clock dom ->
  Clash.Signal dom Clash.Femtoseconds
extractPeriods (Clash.Clock _ (Just s)) = s
extractPeriods _ = pure (clockPeriodFs @dom Proxy)

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

matplotWrite ::
  KnownNat n =>
  String ->
  Vec n Matplotlib ->
  Vec n Matplotlib ->
  IO ()
matplotWrite nm clockDats ebDats = do
  createDirectoryIfMissing True "_build"
  void $
    file
      ("_build/clocks" P.++ nm P.++ ".pdf")
      ( xlabel "Time (fs)"
      % ylabel "Relative period (fs) [0 = ideal frequency]"
      % foldPlots (toList clockDats)
      )
  void $
    file
      ("_build/elasticbuffers" P.++ nm P.++ ".pdf")
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
