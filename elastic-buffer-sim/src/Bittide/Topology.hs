{-# LANGUAGE PartialTypeSignatures #-}

-- | This module contains static topologies and machinery to
module Bittide.Topology ( dumpCsv, genOffs ) where

import Clash.Explicit.Prelude
import Control.Monad (replicateM, forM_, zipWithM_)
import Numeric.Natural
import Prelude qualified as P

import Data.Array qualified as A
import Data.ByteString.Lazy qualified as BSL
import Data.Csv
import System.Random (randomRIO)

import Bittide.Simulate.Ppm
import Bittide.Simulate
import Bittide.Topology.TH
import Bittide.Topology.Graph

type Ps = Natural

-- | This samples @n@ steps; the result can be fed to @script.py@
dumpCsv :: Int -> IO ()
dumpCsv m = do
  offs <- replicateM (n+1) genOffs
  forM_ [0..n] $ \i ->
    let eb = g A.! i in
    writeFile
      ("clocks" <> show i <> ".csv")
      ("t,clk" <> show i <> P.concatMap (\j -> ",eb" <> show i <> show j) eb <>  "\n")
  let dats =
          onN (encode . P.take m)
        $ $(simNodesFromGraph (kn 6)) offs
  zipWithM_ (\dat i ->
    BSL.appendFile ("clocks" <> show i <> ".csv") dat) dats [(0::Int)..]
 where
  onN = $(onTup 6)
  (0, n) = A.bounds g
  g = kn 6

genOffs :: IO Offset
genOffs =
  (`subtract` toInteger specPeriod)
    <$> randomRIO (toInteger minT, toInteger maxT)
 where
  minT = speedUpPeriod specPpm specPeriod
  maxT = slowDownPeriod specPpm specPeriod

-- we use 200kHz in simulation because otherwise the periods are so small that
-- deviations can't be expressed using 'Natural's
specPeriod :: PeriodPs
specPeriod = hzToPeriod 200e3

-- | Clocks uncertainty is ±100 ppm
specPpm :: Ppm
specPpm = Ppm 100
