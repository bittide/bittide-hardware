-- | This module contains static topologies.
module Bittide.Layout ( dumpCsv, genOffs, twoNodes, threeNodes ) where

import Clash.Explicit.Prelude
import Numeric.Natural
import Prelude qualified as P

import Clash.Signal.Internal (Signal (..))

import Data.ByteString.Lazy qualified as BSL
import Data.Csv
import System.Random (randomRIO)

import Bittide.Simulate.Arithmetic
import Bittide.Simulate

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
  (maxT, minT) = slowFastPeriod specPpm specPeriod
  specPpm = 100

-- we use 200kHz in simulation
specPeriod :: PeriodPs
specPeriod = hzToPeriod 200e3

maxPpm :: Ppm
maxPpm = 150

-- | Two bittide nodes connected to one another.
twoNodes ::
  ( KnownDomain dom1
  , KnownDomain dom2
  ) =>
  Offset ->
  Offset ->
  ( Signal dom1 (PeriodPs, DataCount, SpeedChange)
  , Signal dom2 (PeriodPs, DataCount, SpeedChange)
  )
twoNodes offs0 offs1 =
  ( bundle (clk0Signal, eb01, clockControl0)
  , bundle (clk1Signal, eb10, clockControl1)
  )
 where

  (clk0Signal, clock0) = tunableClockGen offs0 step resetGen clockControl0
  eb01 = elasticBuffer ebSz clock0 clock1
  clockControl0 = clockControl offs0 step maxPpm ebSz (eb01 :> Nil)

  (clk1Signal, clock1) = tunableClockGen offs1 step resetGen clockControl1
  eb10 = elasticBuffer ebSz clock1 clock0
  clockControl1 = clockControl offs1 step maxPpm ebSz (eb10 :> Nil)

  ebSz = 128
  step = 1

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
threeNodes offs0 offs1 offs2 =
  ( bundle (clk0Signal, eb01, eb02)
  , bundle (clk1Signal, eb10, eb12)
  , bundle (clk2Signal, eb20, eb21)
  )
 where

  (clk0Signal, clock0) = tunableClockGen offs0 step resetGen clockControl0
  eb01 = elasticBuffer ebSz clock0 clock1
  eb02 = elasticBuffer ebSz clock0 clock2
  clockControl0 = clockControl offs0 step maxPpm ebSz (eb01 :> eb02 :> Nil)

  (clk1Signal, clock1) = tunableClockGen offs1 step resetGen clockControl1
  eb10 = elasticBuffer ebSz clock1 clock0
  eb12 = elasticBuffer ebSz clock1 clock2
  clockControl1 = clockControl offs1 step maxPpm ebSz (eb10 :> eb12 :> Nil)

  (clk2Signal, clock2) = tunableClockGen offs2 step resetGen clockControl2
  eb20 = elasticBuffer ebSz clock2 clock0
  eb21 = elasticBuffer ebSz clock2 clock1
  clockControl2 = clockControl offs2 step maxPpm ebSz (eb20 :> eb21 :> Nil)

  ebSz = 128
  step = 1
