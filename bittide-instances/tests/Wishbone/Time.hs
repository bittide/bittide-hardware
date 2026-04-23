-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Wishbone.Time where

-- Preludes
import Clash.Prelude

-- Local
import Bittide.Instances.Domains (Basic50)
import Bittide.Instances.Tests.TimeWb (
  dMemWords,
  dMemWordsC,
  dutNoMm,
  iMemWords,
  iMemWordsC,
  peConfigSim,
 )
import Bittide.ProcessingElement

-- Other
import Control.Monad (forM_, when)
import Data.Char
import Data.List (isInfixOf)
import Data.Maybe
import Protocols
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH
import Text.Parsec
import Text.Parsec.String

sim :: IO ()
sim = do
  peConfig <- peConfigSim iMemWords dMemWords "time_self_test"
  putStrLn $ simResult peConfig

simResult :: PeConfig 4 -> String
simResult peConfig = chr . fromIntegral <$> catMaybes uartStream
 where
  uartStream =
    sampleC def
      $ withClockResetEnable @Basic50 clockGen resetGen enableGen
      $ dutNoMm peConfig

{- | Run the timing module self test with processingElement and inspect it's uart output.
The test returns names of tests and a boolean indicating if the test passed.
-}
case_time_rust_self_test :: Assertion
case_time_rust_self_test = do
  peConfig <- peConfigSim iMemWords dMemWords "time_self_test"
  let simOutput = simResult peConfig
  -- Run the test with HUnit
  case parseTestResults simOutput of
    Left err -> assertFailure $ show err <> "\n" <> simOutput
    Right results -> do
      forM_ results $ \result -> assertResult result
 where
  assertResult (TestResult name (Just err)) = assertFailure ("Test " <> name <> " failed with error" <> err)
  assertResult (TestResult _ Nothing) = return ()

type IMemWords = DivRU (8 * 1024) 4
type DMemWords = DivRU (4 * 1024) 4

data TestResult = TestResult String (Maybe String) deriving (Show)

type Ascii = BitVector 8
asciiToChar :: Ascii -> Char
asciiToChar = chr . fromIntegral

testResultParser :: Parser TestResult
testResultParser = do
  testName <- manyTill anyChar (try (string ": "))
  result <-
    choice
      [ string "None" >> return Nothing
      , Just <$> (string "Some(" *> manyTill anyChar (char ')'))
      ]
  _ <- endOfLine
  return $ TestResult testName result

testResultsParser :: Parser [TestResult]
testResultsParser = do
  _ <- string "Start time self test" >> endOfLine
  manyTill testResultParser done
 where
  done = try (string "Done") >> endOfLine >> return ()

parseTestResults :: String -> Either ParseError [TestResult]
parseTestResults = parse testResultsParser ""

{- | Run the timing module test with C HAL and inspect its uart output.
This test validates that the C HAL for the timer peripheral works correctly.
-}
case_time_c_test :: Assertion
case_time_c_test = do
  peConfig <- peConfigSim iMemWordsC dMemWordsC "c_timer_wb"
  let
    uartStreamC =
      sampleC def{timeoutAfter = 300_000}
        $ withClockResetEnable @Basic50 clockGen (resetGenN d2) enableGen
        $ dutNoMm peConfig
    simResultC = chr . fromIntegral <$> catMaybes uartStreamC
  when (not $ "=== All tests PASSED! ===" `isInfixOf` simResultC)
    $ assertFailure
    $ "C timer test did not report all tests PASSED\n"
    <> simResultC
  when (not $ "C Timer HAL test completed successfully!" `isInfixOf` simResultC)
    $ assertFailure
    $ "C timer test did not complete successfully\n"
    <> simResultC
  when ("*** TEST FAILED:" `isInfixOf` simResultC)
    $ assertFailure
    $ "C timer test reported a failure\n"
    <> simResultC

tests :: TestTree
tests = $(testGroupGenerator)
