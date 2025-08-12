-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Tests.Gdb where

import Prelude

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)
import Test.Tasty.TH (testGroupGenerator)

import Gdb (readCommand, withGdb)

case_echo :: Assertion
case_echo = do
  withGdb $ \gdb -> do
    output <- readCommand gdb "echo Hello, GDB!"
    assertEqual "GDB should print 'Hello, GDB'" ["Hello, GDB!"] output

case_multilineEcho :: Assertion
case_multilineEcho = do
  withGdb $ \gdb -> do
    output <- readCommand gdb "echo Hello, GDB!\\nAnother line."
    assertEqual "GDB should echo two lines" ["Hello, GDB!", "Another line."] output

tests :: TestTree
tests = $(testGroupGenerator)
