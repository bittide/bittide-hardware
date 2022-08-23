-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

-- | This module contains template haskell functions which lay out circuits
-- using parts from "Bittide.Simulate"
module Bittide.Topology.TH ( cross, onTup, simNodesFromGraph, timeN ) where

import Prelude

import Data.Array qualified as A
import Data.Graph (Graph)
import Language.Haskell.TH (Q, Body (..), Clause (..), Exp (..), Pat (..), Dec (..), Lit (..), Type (..), newName)
import Language.Haskell.TH.Syntax (lift)
import Numeric.Natural (Natural)

import Clash.Explicit.Prelude qualified as Clash
import Clash.Signal.Internal qualified as Clash

import Bittide.Simulate
import Bittide.Topology.TH.Domain

-- | Like the Cartesian product.
--
-- >>> cross [1..2] ['a'..'c']
-- [(1,'a'),(1,'b'),(1,'c'),(2,'a'),(2,'b'),(2,'c')]
cross :: [a] -> [b] -> [(a, b)]
cross xs ys = (,) <$> xs <*> ys

-- | For @n=3@:
--
-- > on3 f (x, y, z) = [f x, f y, f z]
onTup :: Int -> Q Exp
onTup n = do
  f <- newName "f"
  x_is <- traverse (\i -> newName ("x" ++ show i)) [1..n]
  pure $ LamE [VarP f, TupP (VarP <$> x_is)] (ListE [VarE f `AppE` VarE x | x <- x_is ])

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

extrPeriods ::
  forall dom. Clash.KnownDomain dom =>
  Clash.Clock dom ->
  Clash.Signal dom Natural
extrPeriods (Clash.Clock _ (Just s)) = s
extrPeriods _ =
  let
    Clash.SDomainConfiguration _ (Clash.snatToNum -> period) _ _ _ _  = Clash.knownDomain @dom
  in pure period

-- | Given a graph with \(n\) nodes, generate a function which takes a list of \(n\)
-- offsets (divergence from spec) and returns a tuple of signals for each clock domain
simNodesFromGraph :: ClockControlConfig -> Graph -> Q Exp
simNodesFromGraph ccc g = do
  offs <- traverse (\i -> newName ("offs" ++ show i)) is
  clockNames <- traverse (\i -> newName ("clock" ++ show i)) isA
  clockControlNames <- traverse (\i -> newName ("clockControl" ++ show i)) isA
  clockSignalNames <- traverse (\i -> newName ("clk" ++ show i ++ "Signal")) isA
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
        (AppE (AppE (AppE (AppE tunableClockGenV settlePeriod) (VarE (offs !! i))) step) resetGenV)
        (VarE (clockControlNames A.! i))
    clkD i = valD (VarP (clockNames A.! i)) (clkE i)
    clkSignalD i = valD (VarP (clockSignalNames A.! i)) (VarE 'extrPeriods `AppE` VarE (clockNames A.! i))

    clockControlE k =
      AppE
        (AppE clockControlV cccE)
        (mkVecE [ VarE (ebNames A.! (k, i)) | i <- g A.! k ])
    clockControlD k = valD (VarP (clockControlNames A.! k)) (clockControlE k)

    ebs = fmap (uncurry ebD) [ (i, j) | i <- is, j <- g A.! i ]
    clockControls = clockControlD <$> is
    clkDs = clkD <$> is
    clkSignalDs = clkSignalD <$> is

    res k = do
      let ebN = length (g A.! k)
      postprocess <- timeN ebN
      pure $
        AppE
          postprocess
          (SigE
            (AppE bundleV (tup (VarE (clockSignalNames A.! k):[ VarE (ebNames A.! (k, i)) | i <- g A.! k ])))
            signalType)
  ress <- traverse res is
  pure $
    LamE
      [ListP (VarP <$> offs)]
      (LetE (ebs ++ clkDs ++ clkSignalDs ++ clockControls) (tup ress))
 where
  is = [0..n]
  bounds@(0, n) = A.bounds g
  isA = A.listArray bounds is
  ebIxes = cross .$ is
  ebA = A.array ((0, 0), (n, n)) (zip .$ ebIxes)
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
  ebClkClk = ebV `AppE` errC `AppE` ebSz
  clockControlV = VarE 'clockControl
  mkVecE = foldr (\x -> AppE (AppE consC x)) nilC

  ebSz = LitE (IntegerL (toInteger (cccBufferSize ccc)))
  step = LitE (IntegerL (cccStepSize ccc))
  settlePeriod = LitE (IntegerL (toInteger (cccSettlePeriod ccc)))
