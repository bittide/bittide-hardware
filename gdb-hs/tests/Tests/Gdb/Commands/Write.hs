-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Tests.Gdb.Commands.Write where

import Prelude

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH (testGroupGenerator)
import Tests.Common (getBinaryPath)
import Tests.Gdb.Commands.Read (WeirdAlignment (WeirdAlignment, a, b, c, d, e))

import qualified Gdb

case_simple_adt :: Assertion
case_simple_adt = do
  Gdb.withGdb $ \gdb -> do
    Gdb.setFile gdb =<< getBinaryPath "simple_adt"
    Gdb.runCommand gdb "break main.rs:28"
    Gdb.runCommand gdb "run"

    address <- (read @Integer . last . words) <$> Gdb.readCommand1 gdb "print &x"

    Gdb.writeLe gdb address set
    printResult <- Gdb.readCommand gdb "continue"
    case printResult of
      ["Continuing.", "Done: 25 abcdef01 1234 abcdef0123456789 ea", _exit] -> pure ()
      _ -> assertFailure $ "Unexpected output: " <> show printResult
 where
  set =
    WeirdAlignment
      { a = 0x25
      , b = 0xABCDEF01
      , c = 0x01234
      , d = 0xABCDEF0123456789
      , e = 0xEA
      }

tests :: TestTree
tests = $(testGroupGenerator)
