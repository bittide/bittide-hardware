-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Wishbone.Axi where

-- Preludes
import Clash.Explicit.Prelude hiding (writeFile)

-- Other
import Bittide.Instances.Tests.Axi (dut, peConfigSim)
import Control.Monad (forM_)
import Data.Char
import Data.Maybe
import Protocols.Experimental.Simulate (sampleC)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH
import Text.Parsec
import Text.Parsec.String

-- {-# ANN module "HLint: Missing NOINLINE pragma" #-}

sim :: IO ()
sim = do
  peConfig <- peConfigSim
  putStr
    $ fmap (chr . fromIntegral)
    $ catMaybes (sampleC def $ dut peConfig)

{- | Run the axi module self test with processingElement and inspect it's uart output.
The test returns names of tests and a boolean indicating if the test passed.
-}
case_axi_stream_rust_self_test :: Assertion
case_axi_stream_rust_self_test = do
  peConfig <- peConfigSim
  let
    uartStream = sampleC def $ dut peConfig
    simResult = chr . fromIntegral <$> catMaybes uartStream
  -- Run the test with HUnit
  case parseTestResults simResult of
    Left errMsg -> assertFailure $ show errMsg <> "\n" <> simResult
    Right results -> do
      forM_ results $ \result -> assertResult result
 where
  assertResult (TestResult name (Just errMsg)) =
    assertFailure ("Test " <> name <> " failed with error \"" <> errMsg <> "\"")
  assertResult (TestResult _ Nothing) = return ()

data TestResult = TestResult String (Maybe String) deriving (Show, Eq)

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
  _ <- string "Start axi self test" >> endOfLine
  manyTill testResultParser done
 where
  done = try (string "Done") >> endOfLine >> return ()

{- | Parse test results from the simulation output. See 'case_parseTestResults'
for example inputs.
-}
parseTestResults :: String -> Either ParseError [TestResult]
parseTestResults = parse testResultsParser ""

case_parseTestResults :: Assertion
case_parseTestResults = do
  Right [] @=? parseTestResults "Start axi self test\nDone\n"

  Right [TestResult "a" Nothing]
    @=? parseTestResults "Start axi self test\na: None\nDone\n"

  Right [TestResult "a" Nothing, TestResult "b" Nothing]
    @=? parseTestResults "Start axi self test\na: None\nb: None\nDone\n"

  Right [TestResult "a" (Just "1"), TestResult "b" Nothing]
    @=? parseTestResults "Start axi self test\na: Some(1)\nb: None\nDone\n"

tests :: TestTree
tests = $(testGroupGenerator)
