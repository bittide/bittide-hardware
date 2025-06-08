-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Wishbone.SwitchDemoProcessingElement where

import Clash.Explicit.Prelude
import Clash.Prelude (HiddenClockResetEnable, withClockResetEnable)

import BitPackC (ByteOrder (BigEndian))
import Data.Char (chr)
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes)
import Project.FilePath
import Protocols
import Protocols.Idle
import Protocols.MemoryMap
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH
import VexRiscv (DumpVcd (NoDumpVcd))

import Bittide.DoubleBufferedRam
import Bittide.ProcessingElement
import Bittide.ProcessingElement.Util
import Bittide.SwitchDemoProcessingElement
import Bittide.Wishbone

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x : xs) = x : if p x then takeWhileInclusive p xs else []

sim :: IO ()
sim = putStr simResult

simResult :: String
simResult = unlines . takeWhileInclusive (/= "Finished") . lines $ uartString
 where
  uartString = chr . fromIntegral <$> catMaybes uartStream
  uartStream =
    sampleC def{timeoutAfter = 200_000}
      $ withClockResetEnable clk reset enable
      $ dut @System dnaA dnaB

  clk = clockGen
  reset = resetGen
  enable = enableGen
  dnaA = pure 0xAAAA_0123_4567_89AB_CDEF_0001
  dnaB = pure 0xBBBB_0123_4567_89AB_CDEF_0001

case_switch_demo_pe_test :: Assertion
case_switch_demo_pe_test = assertBool msg (receivedString == expectedString)
 where
  msg =
    "Received string "
      <> receivedString
      <> " not equal to expected string "
      <> expectedString
  -- Filter the 'debugging' prints, which are prefixed with 'INFO'
  receivedString = unlines . filter (not . isPrefixOf "INFO") . lines $ simResult
  expectedString =
    unlines
      [ "Buffer A: [(0x10100, 0xBBBB0123456789ABCDEF0001), (0x10000, 0xAAAA0123456789ABCDEF0001)]"
      , "Buffer B: [(0x10000, 0xAAAA0123456789ABCDEF0001), (0xABBAABBAABBA0003, 0xABBA0005ABBAABBAABBA0004)]"
      , "Finished"
      ]

{- | A simulation-only design containing two `switchDemoPeWb`s connected to a single
VexRiscV. The VexRiscV runs the `switch_demo_pe_test` binary from `firmware-binaries`.
-}
dut ::
  forall dom.
  ( HiddenClockResetEnable dom
  , 1 <= DomainPeriod dom
  ) =>
  -- | Fake DNA (used to identify the different PEs)
  Signal dom (BitVector 96) ->
  -- | Fake DNA (used to identify the different PEs)
  Signal dom (BitVector 96) ->
  Circuit () (Df dom (BitVector 8))
dut dnaA dnaB = circuit $ do
  (uartRx, jtagIdle, mm) <- idleSource -< ()
  [ (prefixUart, (mmUart, uartBus))
    , (prefixTime, (mmTime, timeBus))
    , (prefixA, (mmA, peBusA))
    , (prefixB, (mmB, peBusB))
    ] <-
    processingElement NoDumpVcd peConfig -< (mm, jtagIdle)
  (uartTx, _uartStatus) <- uartInterfaceWb d16 d2 uartSim -< (mmUart, (uartBus, uartRx))
  constBwd 0b010 -< prefixUart

  Fwd localCounter <- timeWb -< (mmTime, timeBus)
  constBwd 0b011 -< prefixTime

  (linkAB, _stateAB) <-
    switchDemoPeWb d2 -< (mmA, (Fwd localCounter, peBusA, dnaAC, linkBA))
  constBwd 0b100 -< prefixA
  constBwd 0b101 -< prefixB

  (linkBA, _stateBA) <-
    switchDemoPeWb d2 -< (mmB, (Fwd localCounter, peBusB, dnaBC, linkAB))
  dnaAC <- signalToCSignal dnaA -< ()
  dnaBC <- signalToCSignal dnaB -< ()
  idC -< uartTx
 where
  signalToCSignal :: Signal dom a -> Circuit () (CSignal dom a)
  signalToCSignal = Circuit . const . ((),)

  peConfig = unsafePerformIO $ do
    root <- findParentContaining "cabal.project"
    let
      elfDir = root </> firmwareBinariesDir "riscv32imc" Release
      elfPath = elfDir </> "switch_demo_pe_test"
    (iMem, dMem) <- vecsFromElf @IMemWords @DMemWords BigEndian elfPath Nothing
    pure
      PeConfig
        { initI = Reloadable (Vec iMem)
        , prefixI = 0b000
        , initD = Reloadable (Vec dMem)
        , prefixD = 0b001
        , iBusTimeout = d0 -- No timeouts on the instruction bus
        , dBusTimeout = d0 -- No timeouts on the data bus
        , includeIlaWb = False
        }

type IMemWords = DivRU (72 * 1024) 4
type DMemWords = DivRU (32 * 1024) 4

tests :: TestTree
tests = $(testGroupGenerator)
