-- SPDX-FileCopyrightText: 2022 Google LLC
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

clockGenGroup :: TestTree
clockGenGroup = testGroup "Clock generation group"
 [ testCase "Configuration succeeds" configureSucceeds]


topEntity :: Signal Basic1 (Bool, Bool)
topEntity = bundle (masterBusy, configSuccess)
  where
  (_, masterBusy, configSuccess, (sclk, mosi,ss)) =
    withClockResetEnable clockGen resetGen enableGen $
    si539xSpi testConfig (SNat @50000) (pure Nothing) (readFromBiSignal miso)

  slaveOut :: Signal Basic1 (Maybe (Bytes 2))
  (veryUnsafeToBiSignalIn -> miso, _, slaveOut) =
    withClockResetEnable clockGen resetGen enableGen $
    spiSlaveLatticeSBIO SPIMode0 False sclk mosi miso ss slaveIn

  slaveIn = slaveMachine (deepErrorX "", deepErrorX "", Map.empty) slaveOut

  slaveMachine ::
    (Page, Address, Map.Map (Bytes 2) Byte) ->
    Signal Basic1 (Maybe (Bytes 2)) ->
    Signal Basic1 (Bytes 2)
  slaveMachine oldState@(page, addr, regs) (maybeInput :- inputs) =
    case maybeInput of
      Nothing    -> output :- slaveMachine oldState inputs
      Just input -> output :- slaveMachine newState inputs
       where
        (command, byte) = split input
        newState =case (shiftR command 5, addr) of
          (0, _) -> (page, byte, regs)
          (2, 1) -> (byte, addr, regs)
          (2, _) -> (page, addr, Map.insert (pack (page, addr)) byte regs)
          (4, _) -> (page, addr, regs)
          (3, _) -> (page, succ addr, Map.insert (pack (page, addr)) byte regs)
          (5, _) -> (page, succ addr, regs)
          _ -> newState
   where
    output = maybe undefinedValue resize (Map.lookup (pack (page, addr)) regs)
    undefinedValue =
      deepErrorX $ "slaveMachine: Tried reading uninitialized value at " <> show (page, addr)



configureSucceeds :: Assertion
configureSucceeds =
  assertBool "Eventually the configuration indicates becomes true" (or $ fmap snd simOut)
 where
  simOut = sampleN 100_000_000 topEntity
