-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Main where

import Test.Tasty
import Prelude

import System.FilePath ((</>))
import Test.Tasty.HUnit (testCase)
import Tests.Common (getDataRoot, run)

import qualified Tests.Gdb.Commands.Read
import qualified Tests.Gdb.Commands.Write
import qualified Tests.Gdb.Internal

buildTestCase :: String -> TestTree
buildTestCase name = do
  testCase name $ do
    dataRoot <- getDataRoot
    run "cargo" ["build"] (Just (dataRoot </> name))

tests :: TestTree
tests =
  sequentialTestGroup
    "."
    AllSucceed
    [ testGroup
        "Build test programs"
        [ buildTestCase "simple_adt"
        ]
    , testGroup
        "Tests"
        [ Tests.Gdb.Commands.Read.tests
        , Tests.Gdb.Commands.Write.tests
        , Tests.Gdb.Internal.tests
        ]
    ]

main :: IO ()
main = defaultMain tests
