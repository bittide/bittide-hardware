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
import Bittide.SharedTypes
import Bittide.Wishbone
import Project.FilePath

-- Other
import Data.Char
import Data.Maybe
import Protocols
import Protocols.Idle
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH
import VexRiscv (DumpVcd (NoDumpVcd))

-- Qualified
import qualified Data.List as L
import qualified Protocols.Df as Df

sim :: IO ()
sim = putStrLn simResult

simResult :: String
simResult = chr . fromIntegral <$> mapMaybe Df.dataToMaybe uartStream
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
dut = withClockResetEnable clockGen (resetGenN d2) enableGen
  $ circuit
  $ \_unit -> do
    (uartRx, jtag) <- idleSource -< ()
    [uartBus, timeBus, idleBusA, idleBusB] <- processingElement NoDumpVcd peConfig -< jtag

    idleSink <| (watchDogWb @_ @_ @4 "1 cycle" d1) -< idleBusA
    idleSink
      <| (watchDogWb @_ @_ @4 "50 us" (SNat @(PeriodToCycles Basic200 (Microseconds 50))))
      -< idleBusB

    timeWb <| (watchDogWb @_ @_ @4 "" d0) -< timeBus
    (uartTx, _uartStatus) <- (uartInterfaceWb @_ @_ @4) d2 d2 uartSim -< (uartBus, uartRx)
    idC -< uartTx
 where
  peConfig = unsafePerformIO $ do
    root <- findParentContaining "cabal.project"
    let
      elfDir = root </> firmwareBinariesDir "riscv32imc-unknown-none-elf" Release
      elfPath = elfDir </> "watchdog_test"

    (iMem, dMem) <- vecsFromElf @IMemWords @DMemWords BigEndian elfPath Nothing
    pure
      $ PeConfig
        { memMapConfig = 0b000 :> 0b001 :> 0b010 :> 0b011 :> 0b100 :> 0b101 :> Nil
        , initI = Reloadable (Vec iMem)
        , initD = Reloadable (Vec dMem)
        , iBusTimeout = d0 -- No timeouts on the instruction bus
        , dBusTimeout = d0 -- No timeouts on the data bus
        }

type DMemWords = DivRU (32 * 1024) 4
type IMemWords = DivRU (32 * 1024) 4

tests :: TestTree
tests = $(testGroupGenerator)
