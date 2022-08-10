module Bittide.Topology.TH ( onTup, timeN, simNodesFromGraph ) where

import Prelude

import Data.Array qualified as A
import Data.Graph (Graph)
import Language.Haskell.TH (Q, Body (..), Clause (..), Exp (..), Pat (..), Dec (..), Lit (..), newName)

import Clash.Explicit.Prelude qualified as Clash
import Clash.Signal.Internal qualified as Clash

import Bittide.Simulate
import Bittide.Simulate.Ppm

cross :: [a] -> [b] -> [(a, b)]
cross xs ys = (,) <$> xs <*> ys

-- AppTypeE -> type applications

-- | For @n=3@:
--
-- > on3 f (x, y, z) = (f x, f y, f z)
onTup :: Int -> Q Exp
onTup n = do
  f <- newName "f"
  x_is <- traverse (\i -> newName ("x" ++ show i)) [1..n]
  pure $ LamE [VarP f, TupP (VarP <$> x_is)] (tup [ AppE (VarE f) (VarE x) | x <- x_is ])

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
  pure $ LetE [goD] (AppE goE (LitE (IntegerL 0)))
 where
  consSignal = '(Clash.:-)
  consList = ConE '(:)
  plusE = VarE '(+)

tup :: [Exp] -> Exp
tup es = TupE (Just <$> es)

-- TODO: output after TH stage should be lists

-- | Given a graph with \(n\) nodes, generate a function which takes \(n\)
-- offsets and return a tuple of signals per clock-domain
simNodesFromGraph :: Graph -> Q Exp
simNodesFromGraph g = do
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
      clockControlE k = AppE (AppE clockControlQ cccE) (mkVecE [ VarE (ebNames A.! (k, i)) | i <- g A.! k ])
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
