-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE DuplicateRecordFields #-}

module Tests.Vivado where

import Control.Concurrent.Async (forConcurrently)
import Control.Exception (try)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

import Vivado qualified as V

case_hello_world :: Assertion
case_hello_world = do
  output <- V.with $ \v -> V.exec v "puts hello"
  output @?= "hello\n"

case_error :: Assertion
case_error = do
  result <- try (V.with (`V.exec` "error fail"))
  case result of
    Left (V.TclException{error = e}) -> e @?= "fail"
    _ -> assertFailure "Expected an error"

case_invalidCommand :: Assertion
case_invalidCommand = do
  result <- try (V.with (`V.exec` " / "))
  case result of
    Left (V.TclException{error = e}) -> e @?= "invalid command name \"/\""
    _ -> assertFailure "Expected an error"

case_usable_after_error :: Assertion
case_usable_after_error = do
  V.with $ \v -> do
    result <- try $ V.exec v "error fail"
    case result of
      Left (V.TclException{error = e}) -> do
        e @?= "fail"
        output <- V.exec v "puts hello"
        output @?= "hello\n"
      _ -> assertFailure "Expected an error"

case_async :: Assertion
case_async = do
  let nsExpected = [1 .. 8]
  nsActual <- forConcurrently nsExpected $ \n -> do
    V.with $ \v -> do
      V.exec v $ "puts " <> show (n :: Int)
  nsActual @?= map ((<> "\n") . show) nsExpected

tests :: TestTree
tests = $(testGroupGenerator)
