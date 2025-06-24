-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
-- Don't warn about partial functions: this is a test, so we'll see it fail.
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

module Wishbone.CaptureUgn where

import Clash.Explicit.Prelude
import Clash.Prelude (HiddenClockResetEnable, withClockResetEnable)
import qualified Prelude as P

import BitPackC (ByteOrder (BigEndian))
import Clash.Signal.Internal
import Data.Char
import Data.Maybe
import Numeric
import Project.FilePath
import Protocols
import Protocols.Idle
import Protocols.MemoryMap
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH
import VexRiscv (DumpVcd (NoDumpVcd))

import Bittide.CaptureUgn
import Bittide.DoubleBufferedRam
import Bittide.ProcessingElement
import Bittide.ProcessingElement.Util
import Bittide.Wishbone

{- | Test whether we can read the local and remote sequence counters from the captureUgn
peripheral.
-}
case_capture_ugn_self_test :: Assertion
case_capture_ugn_self_test =
  assertBool
    msg
    ( actualLocalCounter
        == expectedLocalCounter
        && actualRemoteCounter
        == expectedRemoteCounter
    )
 where
  msg =
    "Received local counter 0x"
      <> showHex actualLocalCounter ""
      <> " and remote counter 0x"
      <> showHex actualRemoteCounter ""
      <> " are not equal to expected counters 0x"
      <> showHex expectedLocalCounter ""
      <> " and 0x"
      <> showHex expectedRemoteCounter ""
  (expectedLocalCounter, unpack -> expectedRemoteCounter) = getSequenceCounters $ bundle (localCounter, eb)
  (actualLocalCounter, actualRemoteCounter) = parseResult simResult
  clk = clockGen
  rst = resetGen
  ena = enableGen
  simResult = chr . fromIntegral <$> catMaybes uartStream
  uartStream =
    sampleC def
      $ withClockResetEnable clk rst enableGen
      $ dut @System eb localCounter

  {- The local counter starts counting up from 0x1122334411223344. The elastic buffer
  outputs Nothing for 1000 cycles, after which it will  start outputting a decreasing
  counter starting at 0xaabbccddeeff1234.
  -}
  localCounter = register clk rst ena 0xaabbccddeeff1234 (localCounter + 1)
  eb = regEn clk rst ena Nothing remoteStarted (Just . pack <$> remoteCounter)
   where
    remoteCounter = register clk rst ena (0x1122334411223344 :: Unsigned 64) (remoteCounter - 1)
    remoteStarted = counter .==. pure maxBound
    counter = register clk rst ena (0 :: Index 1000) (satSucc SatBound <$> counter)

{- | A simulation-only instance containing just VexRisc with UART and the captureUgn
peripheral which runs the `capture_ugn_test` binary from `firmware-binaries`.
-}
dut ::
  forall dom.
  (HiddenClockResetEnable dom) =>
  -- | Elastic buffer
  Signal dom (Maybe (BitVector 64)) ->
  -- | Local sequence counter
  Signal dom (Unsigned 64) ->
  Circuit () (Df dom (BitVector 8))
dut eb localCounter = circuit $ do
  (uartRx, jtagIdle) <- idleSource -< ()
  [(prefixUart, (mmUart, uartBus)), (prefixUgn, (mmUgn, ugnBus))] <-
    processingElement @dom NoDumpVcd peConfig -< (mm, jtagIdle)
  (uartTx, _uartStatus) <- uartInterfaceWb d2 d2 uartSim -< (mmUart, (uartBus, uartRx))
  mm <- ignoreMM
  constBwd 0b10 -< prefixUart
  _bittideData <- captureUgn localCounter eb -< (mmUgn, ugnBus)
  constBwd 0b11 -< prefixUgn
  idC -< uartTx
 where
  peConfig = unsafePerformIO $ do
    root <- findParentContaining "cabal.project"
    let elfPath = root </> firmwareBinariesDir "riscv32imc" Release </> "capture_ugn_test"
    (iMem, dMem) <- vecsFromElf @IMemWords @DMemWords BigEndian elfPath Nothing
    pure
      PeConfig
        { initI = Reloadable (Vec iMem)
        , prefixI = 0b00
        , initD = Reloadable (Vec dMem)
        , prefixD = 0b01
        , iBusTimeout = d0 -- No timeouts on the instruction bus
        , dBusTimeout = d0 -- No timeouts on the data bus
        , includeIlaWb = False
        }

type IMemWords = DivRU (64 * 1024) 4
type DMemWords = DivRU (32 * 1024) 4

{- | Simulation function which matches the remote counter to the correct sample
of the local counter.
-}
getSequenceCounters ::
  Signal dom (Unsigned 64, Maybe (BitVector 64)) ->
  (Unsigned 64, BitVector 64)
getSequenceCounters ((a, Just b) :- _) = (a, b)
getSequenceCounters ((_, Nothing) :- xs) = getSequenceCounters xs

parseResult :: String -> (Unsigned 64, Unsigned 64)
parseResult = (read :: String -> (Unsigned 64, Unsigned 64)) . P.head . lines

tests :: TestTree
tests = $(testGroupGenerator)
