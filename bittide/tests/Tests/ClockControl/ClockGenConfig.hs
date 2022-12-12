{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE NumericUnderscores #-}
module Tests.ClockControl.ClockGenConfig where

import Clash.Prelude
import Clash.Signal.Internal(Signal((:-)))

import Bittide.ClockControl.Si5391
import Bittide.ClockControl.ClockGenConfig
import qualified Data.Map as Map
import Bittide.SharedTypes

import Test.Tasty.HUnit
import Test.Tasty
import Clash.Cores.SPI

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

  slaveMachine :: (Page, Address, Map.Map (Bytes 2) Byte) -> Signal Basic1 (Maybe (Bytes 2)) -> Signal Basic1 (Bytes 2)
  slaveMachine (page, addr, regs) (Nothing :- inputs) = output :- slaveMachine (page, addr, regs) inputs
    where
    output = maybe (deepErrorX "") resize (Map.lookup (pack (page, addr)) regs)
  slaveMachine (page, addr, regs) (Just input :- inputs) = output :- slaveMachine nextState inputs
    where
    output = maybe (deepErrorX "") resize (Map.lookup (pack (page, addr)) regs)
    command :: Byte
    (command, byte) = split input
    nextState =case (shiftR command 5, addr) of
      (0, _) -> (page, byte, regs)
      (2, 1) -> (byte, addr, regs)
      (2, _) -> (page, addr, Map.insert (pack (page, addr)) byte regs)
      (4, _) -> (page, addr, regs)
      (3, _) -> (page, succ addr, Map.insert (pack (page, addr)) byte regs)
      (5, _) -> (page, succ addr, regs)
      _ -> nextState

configureSucceeds :: Assertion
configureSucceeds =
  assertBool "Eventually the configuration indicates becomes true" (or $ fmap snd simOut)
 where
  simOut = sampleN 100_000_000 topEntity

