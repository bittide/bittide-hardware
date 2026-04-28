-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Wishbone.SwitchDemoProcessingElement where

import Clash.Explicit.Prelude
import Clash.Prelude (withClockResetEnable)

import Data.Char (chr)
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes)
import Project.FilePath
import Protocols
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

import Bittide.Instances.Common (PeConfigElfSource (NameOnly), peConfigFromElf)
import Bittide.Instances.Tests.SwitchDemoProcessingElement (DMemWords, IMemWords, dut)
import Bittide.ProcessingElement

import qualified Bittide.Cpus.Riscv32imc as Riscv32imc

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x : xs) = x : if p x then takeWhileInclusive p xs else []

sim :: IO ()
sim = do
  peConfig <- peConfigSim
  putStr $ simResult peConfig

simResult :: PeConfig 6 -> String
simResult peConfig = unlines . takeWhileInclusive (/= "Finished") . lines $ uartString
 where
  uartString = chr . fromIntegral <$> catMaybes uartStream
  uartStream =
    sampleC def{timeoutAfter = 200_000}
      $ withClockResetEnable clk reset enable
      $ dut @System peConfig dnaA dnaB

  clk = clockGen
  reset = resetGenN d2
  enable = enableGen
  dnaA = pure 0xAAAA_0123_4567_89AB_CDEF_0001
  dnaB = pure 0xBBBB_0123_4567_89AB_CDEF_0001

case_switch_demo_pe_test :: Assertion
case_switch_demo_pe_test = do
  peConfig <- peConfigSim
  let
    msg =
      "Received string\n"
        <> receivedString
        <> "not equal to expected string\n"
        <> expectedString
    -- Filter the 'debugging' prints, which are prefixed with 'INFO'
    receivedString = unlines . filter (not . isPrefixOf "INFO") . lines $ simResult peConfig
    expectedString =
      unlines
        [ "Buffer A: [(0x10100, 0xBBBB0123456789ABCDEF0001), (0x10000, 0xAAAA0123456789ABCDEF0001)]"
        , "Buffer B: [(0x10000, 0xAAAA0123456789ABCDEF0001), (0xABBAABBAABBA0003, 0xABBAABBAABBA0005ABBAABBAABBA0004)]"
        , "Finished"
        ]
  assertBool msg (receivedString == expectedString)

peConfigSim :: IO (PeConfig 6)
peConfigSim =
  peConfigFromElf
    (SNat @IMemWords)
    (SNat @DMemWords)
    (NameOnly "switch_demo_pe_test")
    Release
    d0
    d0
    False
    Riscv32imc.vexRiscv0

tests :: TestTree
tests = $(testGroupGenerator)
