-- | This module contains static topologies.
module Bittide.Topology ( dumpCsv, genOffs, threeNodes ) where

import Clash.Explicit.Prelude
import Numeric.Natural
import Prelude qualified as P

import Clash.Signal.Internal (Signal (..))

import Data.ByteString.Lazy qualified as BSL
import Data.Csv
import System.Random (randomRIO)

import Bittide.Simulate.Ppm
import Bittide.Simulate
import Bittide.Topology.TH

type Ps = Natural

timeClock :: Signal dom (PeriodPs, a, b) -> [(Ps, PeriodPs, a, b)]
timeClock = go 0
 where
  go t ((period, x, y) :- xs) = (t, period, x, y) : go (t+period) xs

-- | This can be used inside a REPL and fed to @script.py@
dumpCsv :: Int -> IO ()
dumpCsv n = do
  o1 <- genOffs
  o2 <- genOffs
  o3 <- genOffs
  writeFile "clocks0.csv" "t,clk0,eb01,eb02\n"
  writeFile "clocks1.csv" "t,clk1,eb10,eb12\n"
  writeFile "clocks2.csv" "t,clk2,eb20,eb21\n"
  let (dat0, dat1, dat2) =
          on3 (encode . P.take n . timeClock)
        $ threeNodes @Bittide @Bittide @Bittide o1 o2 o3
  BSL.appendFile "clocks0.csv" dat0
  BSL.appendFile "clocks1.csv" dat1
  BSL.appendFile "clocks2.csv" dat2
 where
  on3 f (x, y, z) = (f x, f y, f z)

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
specPpm = Ppm 150

-- | Three nodes, all connected to one another
threeNodes ::
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
threeNodes =
  $(graph (kn 3))
