{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module contains static topologies and machinery to
module Bittide.Topology ( dumpCsv, genOffs ) where

import Clash.Explicit.Prelude
import Control.Monad (replicateM, forM_)
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

-- 200kHz instead of 200MHz; otherwise the periods are so small that deviations
-- can't be expressed as 'Natural's
createDomain vSystem{vName="Bittide", vPeriod=hzToPeriod 200e3}

type Ps = Natural

-- | Create tuples which contain times alongside data.
timeClock :: Signal dom (PeriodPs, a, b) -> [(Ps, PeriodPs, a, b)]
timeClock = $(timeN 2)

-- | This can be used inside a REPL and fed to @script.py@
dumpCsv :: Int -> IO ()
dumpCsv m = do
  offs <- replicateM (n+1) genOffs
  forM_ [0..n] $ \i ->
      let eb = g A.! i in
      writeFile ("clocks" <> show i <> ".csv") ("t,clk" <> show i <> P.concatMap (\j -> ",eb" <> show i <> show j) eb <>  "\n")
  -- the below can be done without TH: output of TH expression should be a list
  -- of 'ByteString's
  let (dat0, dat1, dat2) =
          on3 (encode . P.take m)
        $ k3 offs
  BSL.appendFile "clocks0.csv" dat0
  BSL.appendFile "clocks1.csv" dat1
  BSL.appendFile "clocks2.csv" dat2
 where
  on3 = $(onTup 3)
  (0, n) = A.bounds g
  g = kn 3

genOffs :: IO Offset
genOffs =
  (`subtract` toInteger specPeriod)
    <$> randomRIO (toInteger minT, toInteger maxT)
 where
  minT = speedUpPeriod specPpm specPeriod
  maxT = slowDownPeriod specPpm specPeriod

-- we use 200kHz in simulation
specPeriod :: PeriodPs
specPeriod = hzToPeriod 200e3

specPpm :: Ppm
specPpm = Ppm 100

-- tree23 = $(graph (tree 2 3))

-- RETURN VALUE: maybe graph of signals??
--
-- ez: graph of signals of PeriodPs, maybe a list of DataCounts?

c4 ::
  [Offset] ->
  ( [(Ps, PeriodPs, DataCount, DataCount)]
  , [(Ps, PeriodPs, DataCount, DataCount)]
  , [(Ps, PeriodPs, DataCount, DataCount)]
  , [(Ps, PeriodPs, DataCount, DataCount)]
  )
c4 =
  $(simNodesFromGraph (cn 4))

-- | Three nodes, all connected to one another
k3 ::
  [Offset] ->
  ( [(Ps, PeriodPs, DataCount, DataCount)]
  , [(Ps, PeriodPs, DataCount, DataCount)]
  , [(Ps, PeriodPs, DataCount, DataCount)]
  )
k3 =
  $(simNodesFromGraph (kn 3))
