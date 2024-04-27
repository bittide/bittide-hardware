-- SPDX-FileCopyrightText: 2022-2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE NumericUnderscores #-}

module Tests.ClockControl.Si539xSpi where

import Clash.Prelude
import Clash.Signal.Internal(Signal((:-)))
import Clash.Cores.SPI

import Bittide.ClockControl.Si539xSpi
import Bittide.ClockControl.Si5391A
import Bittide.SharedTypes

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map as Map

createDomain vXilinxSystem{vPeriod = hzToPeriod 1e6, vName = "Basic1"}

tests :: TestTree
tests = testGroup "Tests.ClockControl.Si539xSpi"
 [ testCase "Configuration succeeds" configureSucceeds]


topEntity :: Signal Basic1 (Bool, Bool)
topEntity = bundle (masterBusy, configState .==. pure Finished)
  where
  (_, masterBusy, configState, (sclk, mosi,ss)) =
    withClockResetEnable clockGen resetGen enableGen $
    si539xSpi testConfigA (SNat @50000) (pure Nothing) miso
  miso = si5391Mock sclk mosi ss


si5391Mock :: forall dom . KnownDomain dom => Signal dom Bool -> Signal dom Bit -> Signal dom Bool -> Signal dom Bit
si5391Mock sck mosi ss = readFromBiSignal miso
 where
  slaveOut :: Signal dom (Maybe (Bytes 2))
  (veryUnsafeToBiSignalIn -> miso, _, slaveOut) =
    withClockResetEnable clockGen resetGen enableGen $
    spiSlaveLatticeSBIO SPIMode0 False sck mosi miso ss slaveIn

  slaveIn = si5391Model (deepErrorX "", deepErrorX "", Map.fromList [(0x00FE, 0xF), (0x00C0, 0x00)]) slaveOut

  si5391Model ::
    (Page, Address, Map.Map (Bytes 2) Byte) ->
    Signal dom (Maybe (Bytes 2)) ->
    Signal dom (Bytes 2)
  si5391Model oldState@(page, addr, regs) (maybeInput :- inputs) =
    case maybeInput of
      Nothing    -> output :- si5391Model oldState inputs
      Just input -> output :- si5391Model newState inputs
       where
        (command, byte) = split input
        newState =case (shiftR command 5, addr) of
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

configureSucceeds :: Assertion
configureSucceeds =
  assertBool "Eventually the configuration indicates becomes true" (or $ fmap snd simOut)
 where
  simOut = takeWhile fst $ sample topEntity
