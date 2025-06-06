-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

module Wishbone.Watchdog where

-- Preludes
import Clash.Prelude

-- Local
import Bittide.DoubleBufferedRam
import Bittide.Instances.Domains
import Bittide.ProcessingElement
import Bittide.ProcessingElement.Util
import Bittide.Wishbone
import Project.FilePath

-- Other
import BitPackC (ByteOrder (BigEndian))
import Data.Char
import Data.Maybe
import Protocols
import Protocols.Idle
import Protocols.MemoryMap
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH
import VexRiscv (DumpVcd (NoDumpVcd))

-- Qualified
import qualified Data.List as L

sim :: IO ()
sim = putStrLn simResult

simResult :: String
simResult = chr . fromIntegral <$> catMaybes uartStream
 where
  uartStream = sampleC def dut

{- | Run the timing module self test with processingElement and inspect it's uart output.
The test returns names of tests and a boolean indicating if the test passed.
-}
case_time_rust_self_test :: Assertion
case_time_rust_self_test = do
  let result = L.head $ lines simResult
  assertEqual "Measured timeout wrong " result "Timeout took 50 microseconds"

{- | A simple instance containing just VexRisc and UART as peripheral.
Runs the `hello` binary from `firmware-binaries`.
-}
dut ::
  Circuit () (Df Basic200 (BitVector 8))
dut = withClockResetEnable clockGen resetGen enableGen
  $ circuit
  $ \_unit -> do
    (uartRx, jtag, mm) <- idleSource -< ()
    [ (prefixUart, (mmUart, uartBus))
      , (prefixTime, (mmTime, timeBus))
      , (prefixIdleA, (mmIdleA, idleBusA))
      , (prefixIdleB, (mmIdleB, idleBusB))
      ] <-
      processingElement NoDumpVcd peConfig -< (mm, jtag)

    idleSink <| (watchDogWb @_ @_ @4 "1 cycle" d1) -< idleBusA
    idleSink
      <| (watchDogWb @_ @_ @4 "50 us" (SNat @(PeriodToCycles Basic200 (Microseconds 50))))
      -< idleBusB
    constBwd 0b100 -< prefixIdleA
    constBwd todoMM -< mmIdleA

    constBwd 0b101 -< prefixIdleB
    constBwd todoMM -< mmIdleB

    timeBus1 <- watchDogWb @_ @_ @4 "" d0 -< timeBus
    _localCounter <- timeWb -< (mmTime, timeBus1)
    constBwd 0b011 -< prefixTime
    (uartTx, _uartStatus) <-
      (uartInterfaceWb @_ @_ @4) d2 d2 uartSim -< (mmUart, (uartBus, uartRx))
    constBwd 0b010 -< prefixUart
    idC -< uartTx
 where
  peConfig = unsafePerformIO $ do
    root <- findParentContaining "cabal.project"
    let elfPath = root </> firmwareBinariesDir "riscv32imc" Release </> "watchdog_test"

    (iMem, dMem) <- vecsFromElf @IMemWords @DMemWords BigEndian elfPath Nothing
    pure
      $ PeConfig
        { initI = Reloadable (Vec iMem)
        , prefixI = 0b000
        , initD = Reloadable (Vec dMem)
        , prefixD = 0b001
        , iBusTimeout = d0 -- No timeouts on the instruction bus
        , dBusTimeout = d0 -- No timeouts on the data bus
        , includeIlaWb = False
        }
{-# NOINLINE dut #-}

type IMemWords = DivRU (64 * 1024) 4
type DMemWords = DivRU (32 * 1024) 4

tests :: TestTree
tests = $(testGroupGenerator)
