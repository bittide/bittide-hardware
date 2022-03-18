{-|
Copyright:           Copyright © 2022, Google LLC
License:             Apache-2.0
Maintainer:          devops@qbaylogic.com
|-}
module Main where

import Test.Tasty

import Tests.Calendar
import Tests.DoubleBufferedRAM
import Tests.ScatterGather
import Tests.Switch

tests :: TestTree
tests = testGroup "Unittests"
  [calGroup, sgGroup, switchGroup, ramGroup]

main :: IO ()
main = defaultMain tests
