module Bittide.Topology.TH ( star, tree, cn, kn, graph ) where

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

-- | Tree of depth @d@ with @c@ children
tree :: Int -> Int -> Graph
tree d c = treeGraph
 where
  -- | At depth @d_i@, child node @i@ is connected to the @(i-1) `div` c + 1@st
  -- node at depth @d_i - 1@
  pairs = [ (d_i, i, (i-1) `div` c + 1) | d_i <- [0..d], i <- [1..(c^d_i)] ]
  mkEdges (0, _, _)           = Nothing
  mkEdges (lvl, node, p_node) = Just ((lvl, node), (lvl-1, p_node))
  directedEdges = mapMaybe mkEdges pairs
  edges = directedEdges ++ fmap swap directedEdges
  adjList = g <$> groupBy ((==) `on` fst) (sort edges)
  g ps@((x,_):_) = (x, snd <$> ps)
  (treeGraph, _, _) = graphFromEdges ((\(key, keys) -> (undefined, key, keys)) <$> adjList)

-- | [Star graph](https://mathworld.wolfram.com/StarGraph.html)
star :: Int -> Graph
star = tree 1

-- | [Cyclic graph](https://mathworld.wolfram.com/CycleGraph.html) with @n@
-- vertices.
cn :: Int -> Graph
cn n = A.array bounds (fmap (\i -> (i, neighbors i)) [0..(n-1)])
 where
  bounds = (0, n-1)
  neighbors i = [(i-1) `mod` n, (i+1) `mod` n]

-- | [Complete graph](https://mathworld.wolfram.com/CompleteGraph.html) with @n@
-- vertices.
kn :: Int -> Graph
kn n = A.array bounds (fmap (\i -> (i, others i)) [0..(n-1)])
 where
  bounds = (0, n-1)
  others i = [ j | j <- [0..(n-1)], j /= i ]

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
