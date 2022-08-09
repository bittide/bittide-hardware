module Bittide.Topology.TH ( graph ) where

import Prelude

import Data.Array qualified as A
import Data.Function (on)
import Data.Graph (Graph, graphFromEdges)
import Data.List (sort, groupBy)
import Data.Maybe (mapMaybe)
import Data.Tuple (swap)
import Language.Haskell.TH (Q, Body (..), Exp (..), Pat (..), Dec (..), Lit (..), newName)

import Clash.Explicit.Prelude qualified as Clash

import Bittide.Simulate
import Bittide.Simulate.Ppm

cross :: [a] -> [b] -> [(a, b)]
cross xs ys = (,) <$> xs <*> ys

graph :: Graph -> Q Exp
graph g = do
  offs <- traverse (\i -> newName ("offs" ++ show i)) is
  clockNames <- traverse (\i -> newName ("clock" ++ show i)) isA
  clockControlNames <- traverse (\i -> newName ("clockControl" ++ show i)) isA
  clockSignalNames <- traverse (\i -> newName ("clk" ++ show i ++ "Signal")) isA
  ebNames <- traverse (\(i, j) -> newName ("eb" ++ show i ++ show j)) ebA
  let ebE i j = AppE (AppE ebClkClk (VarE (clockNames A.! i))) (VarE (clockNames A.! j))
      ebD i j = valD (VarP (ebNames A.! (i, j))) (ebE i j)

      clkE i = AppE (AppE (AppE (AppE (AppE tunableClockGenQ settlePeriod) (VarE (offs !! i))) step) resetGenQ) (VarE (clockControlNames A.! i))
      clkD i = valD (TupP [VarP (clockSignalNames A.! i), VarP (clockNames A.! i)]) (clkE i)

      cccE = AppE (AppE (AppE (AppE (AppE ccc pessimisticPeriodL) settlePeriod) dynamicRange) step) ebSz
      clockControlE k = AppE (AppE clockControlQ cccE) (mkVecE [ VarE (ebNames A.! (k, i)) | i <- g A.! k ]) -- FIXME: take assoc list...
      clockControlD k = valD (VarP (clockControlNames A.! k)) (clockControlE k)

      ebs = fmap (uncurry ebD) [ (i, j) | i <- is, j <- g A.! i ]
      clockControls = clockControlD <$> is
      clkDs = clkD <$> is

      res k = AppE bundleQ (tup (VarE (clockSignalNames A.! k):[ VarE (ebNames A.! (k, i)) | i <- g A.! k ]))
  pure $ LamE (VarP <$> offs) (LetE (ebs ++ clkDs ++ clockControls) (tup (fmap res is)))
 where
  is = [0..n]
  bounds@(0, n) = A.bounds g
  isA = A.listArray bounds is
  ebIxes = cross .$ is
  ebA = A.array ((0, 0), (n, n)) (zip .$ ebIxes)

  infixl 3 .$
  (.$) f x = f x x

  tup es = TupE (Just <$> es)
  valD p e = ValD p (NormalB e) []

  bundleQ = VarE 'Clash.bundle
  cons = ConE 'Clash.Cons
  nil = ConE 'Clash.Nil
  err = ConE 'Error
  ccc = ConE 'ClockControlConfig
  ppm = ConE 'Ppm
  ebQ = VarE 'elasticBuffer
  tunableClockGenQ = VarE 'tunableClockGen
  resetGenQ = VarE 'Clash.resetGen
  ebClkClk = AppE (AppE ebQ err) ebSz
  clockControlQ = VarE 'clockControl
  mkVecE = foldr (\x -> AppE (AppE cons x)) nil

  ebSz = LitE (IntegerL 128)
  step = LitE (IntegerL 1)
  pessimisticPeriodL = LitE (IntegerL (toInteger pessimisticPeriod))
  pessimisticPeriod = speedUpPeriod maxPpm (Clash.hzToPeriod 200e3)
  settlePeriod = LitE (IntegerL (toInteger (pessimisticPeriod * 200)))
  dynamicRange = AppE ppm (LitE (IntegerL 150))
  maxPpm = Ppm 150
