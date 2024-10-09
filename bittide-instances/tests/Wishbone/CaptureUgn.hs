-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

module Wishbone.CaptureUgn where

import Clash.Explicit.Prelude
import Clash.Prelude (withClockResetEnable)

import Clash.Cores.UART (ValidBaud, uart)
import Clash.Signal.Internal
import Data.Char
import Data.Maybe
import Language.Haskell.TH
import Numeric
import Project.FilePath
import Protocols
import Protocols.Idle
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

import Bittide.CaptureUgn
import Bittide.DoubleBufferedRam
import Bittide.Instances.Domains
import Bittide.ProcessingElement
import Bittide.ProcessingElement.Util
import Bittide.SharedTypes
import Bittide.Wishbone
import Clash.Cores.UART.Extra (MaxBaudRate)

import qualified Prelude as P

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
  baud = SNat @(MaxBaudRate Basic50)
  clk = clockGen
  rst = resetGen
  ena = enableGen
  simResult = fmap (chr . fromIntegral) $ catMaybes $ sampleN 100_000 uartStream
  (uartStream, _, _) = withClockResetEnable (clockGen @Basic50) rst ena $ uart baud uartTx (pure Nothing)
  uartTx = dut baud clk rst eb localCounter (pure 0)

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
  forall dom baud.
  (KnownDomain dom, ValidBaud dom baud) =>
  SNat baud ->
  Clock dom ->
  -- | CPU reset
  Reset dom ->
  -- | Elastic buffer
  Signal dom (Maybe (BitVector 64)) ->
  -- | Local sequence counter
  Signal dom (Unsigned 64) ->
  -- | USB_UART_TX
  Signal dom Bit ->
  -- | USB_UART_RX
  Signal dom Bit
dut baud clk rst eb localCounter usbUartTx = usbUartRx
 where
  (_, usbUartRx) = go (usbUartTx, pure ())

  go =
    toSignals
      $ withClockResetEnable clk rst enableGen
      $ circuit
      $ \uartRx -> do
        eb <- ebCircuit -< ()
        jtagIdle <- idleSource -< ()
        [uartBus, ugnBus] <- processingElement @dom peConfig -< jtagIdle
        (uartTx, _uartStatus) <- uartWb d256 d16 baud -< (uartBus, uartRx)
        _bittideData <- captureUgn localCounter -< (ugnBus, eb)
        idC -< uartTx

  ebCircuit :: Circuit () (CSignal dom (Maybe (BitVector 64)))
  ebCircuit = Circuit $ const ((), eb)

  (iMem, dMem) =
    $( do
        root <- runIO $ findParentContaining "cabal.project"
        let
          elfDir = root </> firmwareBinariesDir "riscv32imc-unknown-none-elf" Release
          elfPath = elfDir </> "capture_ugn_test"

        memBlobsFromElf BigEndian (Nothing, Nothing) elfPath Nothing
     )

  peConfig =
    PeConfig
      (0b00 :> 0b01 :> 0b10 :> 0b11 :> Nil)
      (Reloadable $ Blob iMem)
      (Reloadable $ Blob dMem)

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
