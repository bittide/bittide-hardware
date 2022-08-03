module Bittide.Topology.TH ( kn ) where

import Prelude

import Data.Array qualified as A
import Language.Haskell.TH (Q, Body (..), Exp (..), Pat (..), Dec (..), Lit (..), newName)

import Clash.Explicit.Prelude qualified as Clash

import Bittide.Simulate
import Bittide.Simulate.Ppm

cross :: [a] -> [b] -> [(a, b)]
cross xs ys = (,) <$> xs <*> ys

-- | [Complete graph](https://mathworld.wolfram.com/CompleteGraph.html) with @n@
-- vertices.
kn :: Int -> Q Exp
kn n = do
  offs <- traverse (\i -> newName ("offs" ++ show i)) is
  clockNames <- traverse (\i -> newName ("clock" ++ show i)) isA
  clockControlNames <- traverse (\i -> newName ("clockControl" ++ show i)) isA
  clockSignalNames <- traverse (\i -> newName ("clk" ++ show i ++ "Signal")) isA
  ebNames <- traverse (\(i, j) -> newName ("eb" ++ show i ++ show j)) ebA
  let ebE i j = AppE (AppE ebClkClk (VarE (clockNames A.! i))) (VarE (clockNames A.! j))
      ebD i j = valD (VarP (ebNames A.! (i, j))) (ebE i j)

      clkE i = AppE (AppE (AppE (AppE (AppE tunableClockGenQ settlePeriod) (VarE (offs ! i))) step) resetGenQ) (VarE (clockControlNames A.! i))
      clkD i = valD (TupP [VarP (clockSignalNames A.! i), VarP (clockNames A.! i)]) (clkE i)

      cccE = AppE (AppE (AppE (AppE (AppE ccc pessimisticPeriodL) settlePeriod) dynamicRange) step) ebSz
      clockControlE k = AppE (AppE clockControlQ cccE) (mkVecE [ VarE (ebNames A.! (k, i)) | i <- is, i /= k ])
      clockControlD k = valD (VarP (clockControlNames A.! k)) (clockControlE k)

      ebs = fmap (uncurry ebD) [ (i, j) | (i, j) <- ebIxes, i /= j ]
      clockControls = clockControlD <$> is
      clkDs = clkD <$> is

      res k = AppE bundleQ (tup (VarE (clockSignalNames A.! k):[ VarE (ebNames A.! (k, i)) | i <- is, i /= k ]))
  pure $ LamE (VarP <$> offs) (LetE (ebs ++ clkDs ++ clockControls) (tup (fmap res is)))
 where
  is = [1..n]
  bounds = (1, n)
  isA = A.listArray bounds is
  ebIxes = cross .$ is
  ebA = A.array ((1, 1), (n, n)) (zip .$ ebIxes)

  infixl 3 .$
  (.$) f x = f x x

  (!) xs ix = xs !! (ix-1)

  tup es = TupE (Just <$> es)
  valD p e = ValD p (NormalB e) []

  bundleQ = VarE 'Clash.bundle
  cons = ConE 'Clash.Cons
  nil = ConE 'Clash.Nil
  err = ConE 'Error
  ccc = ConE 'ClockControlConfig
  ebQ = VarE 'elasticBuffer
  tunableClockGenQ = VarE 'tunableClockGen
  resetGenQ = VarE 'Clash.resetGen
  ebClkClk = AppE (AppE ebQ err) ebSz
  clockControlQ = VarE 'clockControl
  mkVecE = foldr (\x -> AppE (AppE cons x)) nil

  ebSz = LitE (IntegerL 128)
  step = LitE (IntegerL 1)
  pessimisticPeriodL = LitE (IntegerL (toInteger pessimisticPeriod))
  pessimisticPeriod = speedUpPeriod (Ppm 150) (Clash.hzToPeriod 200e3)
  settlePeriod = LitE (IntegerL (toInteger (pessimisticPeriod * 200)))
  dynamicRange = LitE (IntegerL (150*2))
