-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -Wno-orphans #-}

module Tests.Si539xSpiMM where

import Clash.Prelude

import Data.Char (chr)
import Data.Maybe (catMaybes)

import Clash.Cores.SPI (SPIMode (SPIMode0), spiSlaveLatticeSBIO)
import Clash.Signal.Internal (Signal ((:-)))

import Protocols
import Protocols.MemoryMap hiding (Address)

import Bittide.ClockControl.Si539xSpi (Address, Page)
import Bittide.SharedTypes (Byte, Bytes)

import Bittide.Instances.Tests.Si539xSpi (circuitFn)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)

import qualified Data.Map as Map

-- Since waiting periods are part of programming the external clock board, the simulation
-- is faster with a lower frequency.
createDomain vXilinxSystem{vPeriod = hzToPeriod 1e6, vName = "Basic1"}

tests :: TestTree
tests =
  testGroup
    "Tests.Si539xSpiMM"
    [testCase "Configuration succeeds" configureSucceeds]

configureSucceeds :: Assertion
configureSucceeds =
  assertBool "Eventually the configuration finishes" (or simOut)
 where
  simOut = takeWhile not $ fst $ sampleC def dut

sim :: IO ()
sim =
  putStrLn
    $ fmap (chr . fromIntegral)
    $ catMaybes uartStream
 where
  uartStream = snd $ sampleC def{timeoutAfter = 10_000_000} dut

dut :: Circuit () (CSignal Basic1 Bool, Df Basic1 (BitVector 8))
dut = circuit $ do
  mm <- ignoreMM

  (uartTx, spiDone, (Fwd sclk, Fwd mosi, Fwd ss)) <-
    withClockResetEnable @Basic1 clockGen (resetGenN d2) enableGen
      $ circuitFn
      -< (mm, Fwd miso)

  let miso = si5391Mock sclk mosi ss
  idC -< (spiDone, uartTx)

si5391Mock ::
  forall dom.
  (KnownDomain dom) =>
  Signal dom Bool ->
  Signal dom Bit ->
  Signal dom Bool ->
  Signal dom Bit
si5391Mock sck mosi ss = readFromBiSignal miso
 where
  slaveOut :: Signal dom (Maybe (Bytes 2))
  (veryUnsafeToBiSignalIn -> miso, _, slaveOut) =
    withClockResetEnable clockGen resetGen enableGen
      $ spiSlaveLatticeSBIO SPIMode0 False sck mosi miso ss slaveIn

  slaveIn =
    si5391Model
      (deepErrorX "", deepErrorX "", Map.fromList [(0x00FE, 0xF), (0x000C, 0x00)])
      slaveOut

  si5391Model ::
    (Page, Address, Map.Map (Bytes 2) Byte) ->
    Signal dom (Maybe (Bytes 2)) ->
    Signal dom (Bytes 2)
  si5391Model oldState@(page, addr, regs) (maybeInput :- inputs) =
    case maybeInput of
      Nothing -> output :- si5391Model oldState inputs
      Just input -> output :- si5391Model newState inputs
       where
        (command, byte) = split input
        newState = case (shiftR command 5, addr) of
          (0, _) -> (page, byte, regs)
          (2, 1) -> (byte, addr, regs)
          (2, _) -> (page, addr, Map.insert key byte regs)
          (4, _) -> (page, addr, regs)
          (3, _) -> (page, succ addr, Map.insert key byte regs)
          (5, _) -> (page, succ addr, regs)
          _ -> newState
   where
    key = pack (page, addr)
    output = maybe undefinedValue resize (Map.lookup key regs)
    undefinedValue =
      deepErrorX $ "si5391Model: Tried reading uninitialized value at " <> show (page, addr)

type IMemWords = DivRU (64 * 1024) 4
type DMemWords = DivRU (64 * 1024) 4
