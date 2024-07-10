-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Project.Handle where

import Prelude

import Data.List.Extra (trim)
import System.IO

import Test.Tasty.HUnit

data Error = Ok | Error String
data Filter = Continue | Stop Error

{- | Utility function that reads lines from a handle, and applies a filter to
each line. If the filter returns 'Continue', the function will continue
reading lines. If the filter returns @Stop Ok@, the function will return
successfully. If the filter returns @Stop (Error msg)@, the function will
fail with the given message.
-}
expectLine :: (HasCallStack) => Handle -> (String -> Filter) -> Assertion
expectLine h f = do
  line <- trim <$> hGetLine h
  let cont = expectLine h f
  if null line
    then cont
    else case f line of
      Continue -> cont
      Stop Ok -> pure ()
      Stop (Error msg) -> assertFailure msg

{- | Utility function that reads lines from a handle, and waits for a specific
line to appear. Though this function does not fail in the traditional sense,
it will get stuck if the expected line does not appear. Only use in combination
with sensible time outs (also see 'main').
-}
waitForLine :: Handle -> String -> IO ()
waitForLine h expected =
  expectLine h $ \s ->
    if s == expected
      then Stop Ok
      else Continue

-- Utility function that returns the remaining characters in a handle.
readRemainingChars :: Handle -> IO String
readRemainingChars h = do
  rdy <- hReady h
  if rdy
    then do
      c <- hGetChar h
      (c :) <$> readRemainingChars h
    else (pure "")
