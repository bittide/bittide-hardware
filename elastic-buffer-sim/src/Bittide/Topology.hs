-- | This module contains static topologies.
module Bittide.Topology ( dumpCsv, genOffs, k3, c4 ) where

import Clash.Explicit.Prelude
import Control.Monad (replicateM, forM_)
import Numeric.Natural
import Prelude qualified as P

import Clash.Signal.Internal (Signal (..))

import Data.Array qualified as A
import Data.ByteString.Lazy qualified as BSL
import Data.Csv
import Data.Graph (Graph)
import System.Random (randomRIO)

import Bittide.Simulate.Ppm
import Bittide.Simulate
import Bittide.Topology.TH
import Bittide.Topology.Graph

type Ps = Natural

timeClock :: Signal dom (PeriodPs, a, b) -> [(Ps, PeriodPs, a, b)]
timeClock = go 0
 where
  go t ((period, x, y) :- xs) = (t, period, x, y) : go (t+period) xs

-- | This can be used inside a REPL and fed to @script.py@
dumpCsv :: Int -> IO ()
dumpCsv m = do
  [o1, o2, o3] <- replicateM (n+1) genOffs
  forM_ [0..n] $ \i ->
      let eb = g A.! i in
      writeFile ("clocks" <> show i <> ".csv") ("t,clk" <> show i <> P.concatMap (\j -> ",eb" <> show i <> show j) eb <>  "\n")
  let (dat0, dat1, dat2) =
          on3 (encode . P.take m . timeClock)
        $ k3 @Bittide @Bittide @Bittide o1 o2 o3
  BSL.appendFile "clocks0.csv" dat0
  BSL.appendFile "clocks1.csv" dat1
  BSL.appendFile "clocks2.csv" dat2
 where
  on3 f (x, y, z) = (f x, f y, f z)
  (0, n) = A.bounds g
  g = kn 3

  -- TODO: this would require more TH??

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
  ( KnownDomain dom0
  , KnownDomain dom1
  , KnownDomain dom2
  , KnownDomain dom3
  ) =>
  Offset ->
  Offset ->
  Offset ->
  Offset ->
  ( Signal dom0 (PeriodPs, DataCount, DataCount)
  , Signal dom1 (PeriodPs, DataCount, DataCount)
  , Signal dom2 (PeriodPs, DataCount, DataCount)
  , Signal dom3 (PeriodPs, DataCount, DataCount)
  )
c4 =
  $(graph (cn 4))

-- | Three nodes, all connected to one another
k3 ::
  ( KnownDomain dom1
  , KnownDomain dom2
  , KnownDomain dom3
  ) =>
  Offset ->
  Offset ->
  Offset ->
  ( Signal dom1 (PeriodPs, DataCount, DataCount)
  , Signal dom2 (PeriodPs, DataCount, DataCount)
  , Signal dom3 (PeriodPs, DataCount, DataCount)
  )
k3 =
  $(graph (kn 3))
