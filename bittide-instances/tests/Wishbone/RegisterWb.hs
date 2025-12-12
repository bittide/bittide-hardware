-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Wishbone.RegisterWb where

import Clash.Prelude

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase)
import Test.Tasty.TH (testGroupGenerator)

import Bittide.Instances.Domains (Basic50)
import Bittide.Instances.Tests.RegisterWb (dutWithBinary, simResult)
import Data.Char (chr)
import Data.Maybe (catMaybes)
import Protocols (Circuit (Circuit), Df, Drivable (sampleC), toSignals)

import qualified Text.Parsec as P
import qualified Text.Parsec.String as P

case_sim :: Assertion
case_sim =
  case parseResultLine simResult of
    Left err ->
      assertFailure $ "Parse error: " <> show err
    Right (Just err) ->
      assertFailure $ "Test failed with error: " <> err
    Right Nothing ->
      pure ()

-- | Test the C version of the RegisterWb test
case_c_sim :: Assertion
case_c_sim =
  case parseResultLine cSimResult of
    Left err ->
      assertFailure $ "Parse error: " <> show err
    Right (Just err) ->
      assertFailure $ "Test failed with error: " <> err
    Right Nothing ->
      pure ()

cSimResult :: String
cSimResult = chr . fromIntegral <$> catMaybes uartStream
 where
  uartStream = sampleC def cdut0

  cdut0 :: Circuit () (Df Basic50 (BitVector 8))
  cdut0 = Circuit $ ((),) . snd . toSignals (dutWithBinary "c_registerwb_test") . ((),) . snd

parseResultLine :: String -> Either P.ParseError (Maybe String)
parseResultLine = P.parse resultLineParser ""

resultLineParser :: P.Parser (Maybe String)
resultLineParser = do
  _ <- P.string "RESULT: "
  status <-
    P.choice
      [ P.string "OK" >> return (Nothing :: Maybe String)
      , P.string "PANIC" >> (Just . ("PANIC" <>) <$> restOfLine)
      , P.string "FAIL: " >> (Just <$> restOfLine)
      ]
  return status
 where
  restOfLine = P.manyTill P.anyChar (P.try (P.char '\n'))

tests :: TestTree
tests = $(testGroupGenerator)
