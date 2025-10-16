-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Wishbone.RegisterWb where

import Clash.Prelude

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase)
import Test.Tasty.TH (testGroupGenerator)

import Bittide.Instances.Tests.RegisterWb (simResult)

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
