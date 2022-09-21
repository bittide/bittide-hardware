-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

-- | This module contains template haskell functions which lay out circuits
-- using parts from "Bittide.Simulate"
module Bittide.Topology.TH
  ( cross
  , absTimes
  , encodeDats
  , encodeQ
  , onTup
  , onN
  , plotEbsAPI
  , simNodesFromGraph
  , timeN
  , unzipN
  , takeEveryN
  , genOffsN
  )
where

import Prelude

import Control.Monad (replicateM, void, zipWithM)
import Data.Bifunctor (bimap)
import Data.Csv (encode)
import Data.Graph (Graph)
import Graphics.Matplotlib (Matplotlib, (%), file, plot, xlabel, ylabel)
import Language.Haskell.TH (Q, Body (..), Clause (..), Exp (..), Pat (..), Dec (..), Lit (..), Stmt (..), Type (..), newName)
import Language.Haskell.TH.Syntax (lift)
import Numeric.Natural (Natural)
import System.Directory (createDirectoryIfMissing)
import System.Random (randomRIO)

import Bittide.Simulate
import Bittide.Simulate.Ppm
import Bittide.ClockControl
import Bittide.ClockControl.Strategies
import Bittide.Topology.TH.Domain
import Graphics.Matplotlib.Ext

import Data.Array qualified as A
import Data.List qualified as L

import Clash.Explicit.Prelude qualified as Clash
import Clash.Signal.Internal qualified as Clash

-- | As an example:
--
-- >>> takeEveryN 3 [1..10]
-- [1,4,7,10]
takeEveryN :: Int -> [a] -> [a]
takeEveryN _ [] = []
takeEveryN n (x:xs) = x : takeEveryN n (drop (n-1) xs)

matplotWrite :: String -> [Matplotlib] -> [Matplotlib] -> IO ()
matplotWrite nm clockDats ebDats = do
  createDirectoryIfMissing True "_build"
  void $
    file
      ("_build/clocks" ++ nm ++ ".pdf")
      (xlabel "Time (ps)" % ylabel "Period (ps)" % foldPlots clockDats)
  void $
    file
      ("_build/elasticbuffers" ++ nm ++ ".pdf")
      (xlabel "Time (ps)" % foldPlots ebDats)

genOffsN :: Int -> IO [Offset]
genOffsN n = replicateM (n+1) genOffsets

-- | 'Graph' with a name for
type GraphAPI = (String, Graph)

-- | Given a 'Graph', generate an expression of type
--
-- > Int -> Int -> IO ()
--
-- which writes/dumps simulation results for a particular graph.
plotEbsAPI :: GraphAPI -> Q Exp
plotEbsAPI (nm, g) = do
  mplots <- plotEbsQ g
  offs <- newName "offs"
  m <- newName "m"
  k <- newName "k"
  res <- newName "res"
  let mV = VarE m
      kV = VarE k
  graphName <- lift nm
  pure $
    LamE [VarP m, VarP k]
      (DoE Nothing
        [ BindS (VarP offs) (VarE 'genOffsN `AppE` nE)
        , LetS
            [ValD
              (VarP res)
              (NormalB (mplots `AppE` VarE offs `AppE` mV `AppE` kV))
              []
            ]
        , NoBindS (VarE 'uncurry
                    `AppE` (VarE 'matplotWrite `AppE` graphName)
                    `AppE` VarE res)
        ])
 where
  (0, n) = A.bounds g
  nE = LitE (IntegerL (toInteger n))

-- | Given a 'Graph', generate an expression of type
--
-- > [Offset] -> Int -> Int -> ([Matplotlib], [Matplotlib])
plotEbsQ :: Graph -> Q Exp
plotEbsQ g = do
  m <- newName "m"
  k <- newName "k"
  offs <- newName "offs"
  let mV = VarE m
      kV = VarE k
      offsV = VarE offs
  sim <- simNodesFromGraph defClockConfig g
  pd <- plotDats g
  let discardIntermediate = VarE 'takeEveryN `AppE` kV
  onNQ <- onN (n+1)
  pure $
    LamE
      [VarP offs, VarP m, VarP k]
      (unzipV
        `AppE` ((onNQ `AppE` (pd `AppE` mV))
        `AppE` (discardIntermediate `AppE` (sim `AppE` offsV))))
 where
  (0, n) = A.bounds g
  unzipV = VarE 'unzip

-- | Like the Cartesian product.
--
-- >>> cross [1..2] ['a'..'c']
-- [(1,'a'),(1,'b'),(1,'c'),(2,'a'),(2,'b'),(2,'c')]
cross :: [a] -> [b] -> [(a, b)]
cross xs ys = (,) <$> xs <*> ys

-- | For @n=2@ the type will be:
--
-- > ([a] -> b, [c] -> b) -> [(a, c)] -> [b]
onN :: Int -> Q Exp
onN n = do
  onTupQ <- onTup n
  unzipNQ <- unzipN n
  f <- newName "f"
  pure (LamE [VarP f] ((onTupQ `AppE` VarE f) `compose` unzipNQ))

-- | For @n=3@:
--
-- > on3 (f, g, h) (x, y, z) = [f x, g y, h z]
onTup :: Int -> Q Exp
onTup n = do
  fs <- traverse (\i -> newName ("f" ++ show i)) [1..n]
  x_is <- traverse (\i -> newName ("x" ++ show i)) [1..n]
  pure $
    LamE
      [TupP (VarP <$> fs), TupP (VarP <$> x_is)]
      (ListE (zipWith AppE (VarE <$> fs) (VarE <$> x_is)))

-- | Example: @extrClocks 2@ will give a function of type
--
-- @(Ps, PeriodPs, DataCount, DataCount) -> ((Ps, PeriodPs), [(Ps, DataCount)])@
extrClocks :: Int -> Q Exp
extrClocks i = do
  x <- newName "x"
  y <- newName "y"
  zs <- traverse newName (replicate i "z")
  pure $
    LamE
      [TupP (VarP <$> x:y:zs)]
      (tup [tup [VarE x, VarE y], ListE (fmap (\z -> tup [VarE x, VarE z]) zs)])

-- | Arrange 'asPlotDat'-generated 'Exp' for each node, in order. Concretely:
--
-- > \m -> ($(asPlotN d0) m, $(asPlotN d1) m, ...)
--
-- where @d0, d1, ...@ are the degrees of the vertices.
plotDats :: Graph -> Q Exp
plotDats g = do
  m <- newName "m"
  LamE [VarP m]
    . tup <$> traverse (\i -> (`AppE` VarE m) <$> asPlotN i) degs
 where
  degs = A.elems (fmap length g)

-- | Example: @encodeDats 4@ will generate:
--
-- > encodeAll = \m -> ($(encodeQ) m, $(encodeQ) m, $(encodeQ) m, $(encodeQ) m)
--
-- i.e. apply 'encodeQ' to all elements of a 4-tuple.
encodeDats :: Int -> Q Exp
encodeDats i = do
  m <- newName "m"
  LamE [VarP m]
    . tup <$> traverse (\_ -> (`AppE` VarE m) <$> encodeQ) [1..i]

-- | @encodeQ@ will generate:
--
-- @\m -> encode . take m@
--
-- of type
--
-- @Int -> [(Ps, PeriodPs, DataCount, DataCount, ...)] -> BS.ByteString@
encodeQ :: Q Exp
encodeQ = do
  m <- newName "m"
  pure $
    LamE [VarP m] $
              VarE 'encode
    `compose` (VarE 'take `AppE` VarE m)

-- | Based on 'unzip', 'unzip3' from the "Prelude".
unzipN :: Int -> Q Exp
unzipN n = do
  go <- op
  pure (VarE 'foldr `AppE` go `AppE` seedN)
 where
  seedN = tup (replicate n (ConE '[]))
  op = do
    a_i <- traverse (\i -> newName ("a" ++ show i)) [1..n]
    as_i <- traverse (\i -> newName ("as" ++ show i)) [1..n]
    -- note the lazy pattern match, conforming to 'unzip' defined in base
    pure $
      LamE
        [TupP (VarP <$> a_i), TildeP (TupP (VarP <$> as_i))]
        (tup
          (zipWith
            (\a as -> consList `AppE` a `AppE` as)
            (VarE <$> a_i)
            (VarE <$> as_i)))
  consList = ConE '(:)

-- | This generates a function to process the output of 'simNodesFromGraph'; it
-- takes a tuple of signals and returns a list of tuples of data associated with
-- each node (including absolute time).
--
-- The generated function will have type:
--
-- > absTimes :: (Signal dom0 (PeriodPs, a_1, ...), Signal dom1 (PeriodPs, b_1, ...), ...) -> [((Ps, PeriodPs, a_1, ...), (Ps, PeriodPs, b_1, ...), ...)]
absTimes :: Graph -> Q Exp
absTimes g = do
  nm <- newName "go"
  tNames <- traverse (\j -> newName ("t" ++ show j)) [0..i]
  xss <- traverse (\j -> newName ("xs" ++ show j)) [0..i]
  periodNames <- traverse (\j -> newName ("period" ++ show j)) [0..i]
  x_ns <-
    zipWithM
      (\k n ->
        traverse (\j -> newName ("x_" ++ show j ++ "_" ++ show k)) [0..(n-1)])
      (A.indices n_i)
      (A.elems n_i)
  let
    goE = VarE nm
    ts = VarE <$> tNames
    periods = VarE <$> periodNames
    tNext = zipWith (\period t -> plusE `AppE` t `AppE` period) periods ts
    goD =
      FunD nm
        [ Clause
          (fmap VarP tNames
            ++ [TupP (zipWith3
                        (\periodName ns xs ->
                          InfixP
                            (TupP (VarP <$> periodName : ns))
                            consSignal
                            (VarP xs))
                        periodNames x_ns xss)])
          (NormalB
            (AppE
              (AppE
                consList
                (tup
                  (zipWith3
                    (\t period xs -> tup (t:period:fmap VarE xs))
                    ts periods x_ns)))
              (AppE (apply goE tNext) (tup (VarE <$> xss)))))
          []
        ]
  pure $ LetE [goD] (apply goE (replicate (i+1) zeroE))
 where
  zeroE = LitE (IntegerL 0)
  consSignal = '(Clash.:-)
  consList = ConE '(:)
  plusE = VarE '(+)
  n_i = fmap length g
  (0, i) = A.bounds g

apply :: Exp -> [Exp] -> Exp
apply = foldl AppE

-- | Given a @Signal dom (PeriodPs, a_1, ...)@, make a @[(Ps, PeriodPs, a_1, ...)]@.
--
-- For @n=2@:
--
-- > timeClock :: Signal dom (PeriodPs, a, b) -> [(Ps, PeriodPs, a, b)]
-- > timeClock = go 0
-- >  where
-- >   go t ((period, x, y) :- xs) = (t, period, x, y) : go (t+period) xs
timeN :: Int -> Q Exp
timeN n = do
  nm <- newName "go"
  tName <- newName "t"
  xs <- newName "xs"
  periodName <- newName "period"
  x_is <- traverse (\i -> newName ("x" ++ show i)) [1..n]
  let goE = VarE nm
      t = VarE tName
      period = VarE periodName
      goD =
        FunD nm
          [ Clause
              [VarP tName, InfixP (TupP (VarP <$> periodName : x_is)) consSignal (VarP xs)]
              (NormalB
                (AppE
                  (AppE consList (tup (t:period:fmap VarE x_is)))
                  (AppE (AppE goE (plusE `AppE` t `AppE` period)) (VarE xs))))
              []
          ]
  pure $ LetE [goD] (goE `AppE` LitE (IntegerL 0))
 where
  consSignal = '(Clash.:-)
  consList = ConE '(:)
  plusE = VarE '(+)

tup :: [Exp] -> Exp
tup es = TupE (Just <$> es)

-- | Example: @extrClocks 2@ will give a function of type
--
-- @Int -> [(Ps, PeriodPs, DataCount, DataCount)] -> (Matplotlib, Matplotlib)@
--
-- which takes @m@ from an infinite stream of measurements (second argument).
--
-- The result will be elastic buffer occupancy vs. time and period vs. time
-- (respectively)
asPlotN :: Int -> Q Exp
asPlotN i = do
  g <- extrClocks i
  m <- newName "m"
  pure $
    LamE [VarP m] $
              bimapV
                plotPairs
                (VarE 'foldPlots `compose` mapV plotPairs `compose` VarE 'L.transpose)
    `compose` VarE 'unzip
    `compose` mapV g
    `compose` (VarE 'take `AppE` VarE m)
 where
  mapV = AppE (VarE 'fmap)
  unzipV = VarE 'unzip
  bimapV f = AppE (AppE (VarE 'bimap) f)
  plotPairs = AppE (VarE 'uncurry) (VarE 'plot) `compose` unzipV

compose :: Exp -> Exp -> Exp
compose e0 = AppE (AppE (VarE '(.)) e0)

extractPeriods ::
  forall dom. Clash.KnownDomain dom =>
  Clash.Clock dom ->
  Clash.Signal dom Natural
extractPeriods (Clash.Clock _ (Just s)) = s
extractPeriods _ = pure (Clash.snatToNum (Clash.clockPeriod @dom))

-- | Given a graph with \(n\) nodes, generate a function which takes a list of \(n\)
-- offsets (divergence from spec) and returns a tuple of signals for each clock
-- domain
simNodesFromGraph :: ClockControlConfig -> Graph -> Q Exp
simNodesFromGraph ccc g = do
  offsets <- traverse (\i -> newName ("offsets" ++ show i)) indices
  clockNames <- traverse (\i -> newName ("clock" ++ show i)) indicesArr
  clockControlNames <- traverse (\i -> newName ("clockControl" ++ show i)) indicesArr
  clockSignalNames <- traverse (\i -> newName ("clk" ++ show i ++ "Signal")) indicesArr
  ebNames <- traverse (\(i, j) -> newName ("eb" ++ show i ++ show j)) ebA
  cccE <- lift ccc
  let
    ebE i j =
        AppE
          (AppE ebClkClk (VarE (clockNames A.! i)))
          (VarE (clockNames A.! j))
    ebD i j = valD (VarP (ebNames A.! (i, j))) (ebE i j)

    clkE i =
      AppE
        (AppE (AppE (AppE (AppE tunableClockGenV settlePeriod) (VarE (offsets !! i))) step) resetGenV)
        (VarE (clockControlNames A.! i))
    clkD i = valD (VarP (clockNames A.! i)) (clkE i)
    clkSignalD i = valD (VarP (clockSignalNames A.! i)) (VarE 'extractPeriods `AppE` VarE (clockNames A.! i))

    clockControlE k = mapV `AppE` fstV `AppE`
      AppE
        (callistoClockControlV
          `AppE` VarE (clockNames A.! k)
          `AppE` resetGenV
          `AppE` enableGenV
          `AppE` cccE)
        (mkVecE [ VarE (ebNames A.! (k, i)) | i <- g A.! k ])
    clockControlD k = valD (VarP (clockControlNames A.! k)) (clockControlE k)

    ebs = fmap (uncurry ebD) [ (i, j) | i <- indices, j <- g A.! i ]
    clockControls = clockControlD <$> indices
    clkDs = clkD <$> indices
    clkSignalDs = clkSignalD <$> indices

    res k =
      SigE
        (AppE
          bundleV
          (tup (VarE (clockSignalNames A.! k):[ VarE (ebNames A.! (k, i)) | i <- g A.! k ])))
        signalType

  postprocess <- absTimes g
  pure $
    LamE
      [ListP (VarP <$> offsets)]
      (LetE
        (ebs ++ clkDs ++ clkSignalDs ++ clockControls)
        (postprocess `AppE` tup (fmap res indices)))
 where
  indices = [0..n]
  bounds@(0, n) = A.bounds g
  indicesArr = A.listArray bounds indices
  ebIndices = cross .$ indices
  ebA = A.array ((0, 0), (n, n)) (zip .$ ebIndices)
  signalType =
    ConT ''Clash.Signal `AppT` ConT ''Bittide `AppT` WildCardT

  infixl 3 .$
  (.$) f x = f x x

  valD p e = ValD p (NormalB e) []

  bundleV = VarE 'Clash.bundle
  consC = ConE 'Clash.Cons
  nilC = ConE 'Clash.Nil
  errC = ConE 'Error
  ebV = VarE 'elasticBuffer
  tunableClockGenV = VarE 'tunableClockGen
  resetGenV = VarE 'Clash.resetGen
  enableGenV = VarE 'Clash.enableGen
  ebClkClk = ebV `AppE` errC `AppE` ebSize
  callistoClockControlV = VarE 'callistoClockControl
  mkVecE = foldr (\x -> AppE (AppE consC x)) nilC
  mapV = VarE 'fmap
  fstV = VarE 'fst

  ebSize = LitE (IntegerL (toInteger (cccBufferSize ccc)))
  step = LitE (IntegerL (cccStepSize ccc))
  settlePeriod = LitE (IntegerL (toInteger (cccSettlePeriod ccc)))

-- | Randomly generate a 'Offset', how much a real clock's period may differ
-- from its spec.
genOffsets :: IO Offset
genOffsets =
  (`subtract` toInteger specPeriod)
    <$> randomRIO (toInteger minT, toInteger maxT)
 where
  minT = speedUpPeriod specPpm specPeriod
  maxT = slowDownPeriod specPpm specPeriod

-- | Clocks uncertainty is ±100 ppm
specPpm :: Ppm
specPpm = Ppm 100
