-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
-- Don't warn about partial functions: this is a test, so we'll see it fail.
{-# OPTIONS_GHC -Wno-x-partial #-}

module Wishbone.Watchdog where

-- Preludes
import Clash.Prelude

-- Local
import Bittide.Instances.Common (PeConfigElfSource (NameOnly), peConfigFromElf)
import Bittide.Instances.Domains
import Bittide.ProcessingElement
import Bittide.SharedTypes (withLittleEndian)
import Bittide.Wishbone
import Project.FilePath

-- Other
import Data.Char
import Data.Maybe
import Protocols
import Protocols.Idle
import Protocols.MemoryMap
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH
import VexRiscv (DumpVcd (NoDumpVcd))

-- Qualified

import qualified Bittide.Cpus.Riscv32imc as Riscv32imc
import qualified Data.List as L
import qualified Protocols.ToConst as ToConst

sim :: IO ()
sim = do
  peConfig <- peConfigSim
  putStrLn $ simResult peConfig

simResult :: PeConfig 6 -> String
simResult peConfig = chr . fromIntegral <$> catMaybes uartStream
 where
  uartStream = sampleC def $ dut peConfig

{- | Run the timing module self test with processingElement and inspect it's uart output.
The test returns names of tests and a boolean indicating if the test passed.
-}
case_time_rust_self_test :: Assertion
case_time_rust_self_test = do
  peConfig <- peConfigSim
  let result = L.head $ lines $ simResult peConfig
  assertEqual "Measured timeout wrong " "Timeout took 50 microseconds" result

{- | A simple instance containing just VexRisc and UART as peripheral.
Runs the `hello` binary from `firmware-binaries`.
-}
dut :: PeConfig 6 -> Circuit () (Df Basic200 (BitVector 8))
dut peConfig = withLittleEndian
  $ withClockResetEnable clockGen (resetGenN d2) enableGen
  $ circuit
  $ \_unit -> do
    (uartRx, jtag) <- idleSource
    [ uartBus
      , (mmTime, timeBus0)
      , (mmIdleA, idleBusA)
      , (mmIdleB, idleBusB)
      ] <-
      processingElement NoDumpVcd peConfig -< (mm, jtag)

    mm <- ignoreMM
    idleSink <| (watchDogWb @_ @_ @4 "1 cycle" d1) -< idleBusA
    idleSink
      <| (watchDogWb @_ @_ @4 "50 us" (SNat @(PeriodToCycles Basic200 (Microseconds 50))))
      -< idleBusB
    ToConst.toBwd todoMM -< mmIdleA
    ToConst.toBwd todoMM -< mmIdleB

    timeBus1 <- watchDogWb @_ @_ @4 "" d0 -< timeBus0
    _localCounter <- timeWb Nothing -< (mmTime, timeBus1)
    (uartTx, _uartStatus) <-
      (uartInterfaceWb @_ @_ @4) d2 d2 uartBytes -< (uartBus, uartRx)
    idC -< uartTx
{-# OPAQUE dut #-}

type IMemWords = DivRU (4 * 1024) 4
type DMemWords = DivRU (4 * 1024) 4

peConfigSim :: IO (PeConfig 6)
peConfigSim =
  peConfigFromElf
    (SNat @IMemWords)
    (SNat @DMemWords)
    (NameOnly "watchdog_test")
    Release
    d0
    d0
    False
    Riscv32imc.vexRiscv0

tests :: TestTree
tests = $(testGroupGenerator)
