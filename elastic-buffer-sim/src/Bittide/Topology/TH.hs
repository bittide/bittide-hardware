-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

-- | This module contains template haskell functions which lay out circuits
-- using parts from "Bittide.Simulate"
module Bittide.Topology.TH
  ( cross
  , encodeDats
  , onTup
  , plotDats
  , simNodesFromGraph
  , timeN
  )
where

import Prelude

import Data.Bifunctor (bimap)
import Data.Csv (encode)
import Data.Graph (Graph)
import Graphics.Matplotlib (plot)
import Language.Haskell.TH (Q, Body (..), Clause (..), Exp (..), Pat (..), Dec (..), Lit (..), Type (..), newName)
import Language.Haskell.TH.Syntax (lift)
import Numeric.Natural (Natural)

import Bittide.Simulate
import Bittide.Topology.TH.Domain
import Graphics.Matplotlib.Ext

import Data.Array qualified as A
import Data.List qualified as L

import Clash.Explicit.Prelude qualified as Clash
import Clash.Signal.Internal qualified as Clash

-- | Like the Cartesian product.
--
-- >>> cross [1..2] ['a'..'c']
-- [(1,'a'),(1,'b'),(1,'c'),(2,'a'),(2,'b'),(2,'c')]
cross :: [a] -> [b] -> [(a, b)]
cross xs ys = (,) <$> xs <*> ys

-- | For @n=3@:
--
-- > on3 (f, g, h) (x, y, z) = [f x, f y, f z]
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

encodeDats :: Int -> Q Exp
encodeDats i = do
  m <- newName "m"
  LamE [VarP m]
    . tup <$> traverse (\_ -> (`AppE` VarE m) <$> encodeQ) [1..i]

-- | Arrange 'asPlotDat'-generated 'Exp' for each node, in order.
plotDats :: Graph -> Q Exp
plotDats g = do
  m <- newName "m"
  LamE [VarP m]
    . tup <$> traverse (\i -> (`AppE` VarE m) <$> asPlotN i) degs
 where
  degs = A.elems (fmap length g)

-- | Example: @encodeQ@ will return a function of type
--
-- @Int -> [(Ps, PeriodPs, DataCount, DataCount, ...)] -> BS.ByteString@
encodeQ :: Q Exp
encodeQ = do
  m <- newName "m"
  pure $
    LamE [VarP m] $
              VarE 'encode
    `compose` (VarE 'take `AppE` VarE m)

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
                  (AppE (AppE goE (AppE (AppE plusE t) period)) (VarE xs))))
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
-- The result (of type 'Matplotlib') will be period vs. time
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
-- offsets (divergence from spec) and returns a tuple of signals for each clock domain
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

    clockControlE k =
      AppE
        (AppE clockControlV cccE)
        (mkVecE [ VarE (ebNames A.! (k, i)) | i <- g A.! k ])
    clockControlD k = valD (VarP (clockControlNames A.! k)) (clockControlE k)

    ebs = fmap (uncurry ebD) [ (i, j) | i <- indices, j <- g A.! i ]
    clockControls = clockControlD <$> indices
    clkDs = clkD <$> indices
    clkSignalDs = clkSignalD <$> indices

    res k = do
      let ebN = length (g A.! k)
      postprocess <- timeN ebN
      pure $
        AppE
          postprocess
          (SigE
            (AppE bundleV (tup (VarE (clockSignalNames A.! k):[ VarE (ebNames A.! (k, i)) | i <- g A.! k ])))
            signalType)
  ress <- traverse res indices
  pure $
    LamE
      [ListP (VarP <$> offsets)]
      (LetE (ebs ++ clkDs ++ clkSignalDs ++ clockControls) (tup ress))
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
  ebClkClk = ebV `AppE` errC `AppE` ebSize
  clockControlV = VarE 'clockControl
  mkVecE = foldr (\x -> AppE (AppE consC x)) nilC

  ebSize = LitE (IntegerL (toInteger (cccBufferSize ccc)))
  step = LitE (IntegerL (cccStepSize ccc))
  settlePeriod = LitE (IntegerL (toInteger (cccSettlePeriod ccc)))
