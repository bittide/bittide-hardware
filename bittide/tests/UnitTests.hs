{-|
Copyright:           Copyright © 2022, Google LLC
License:             Apache-2.0
Maintainer:          devops@qbaylogic.com
|-}
module Main where

import Test.Tasty

import Tests.DoubleBufferedRAM

tests :: TestTree
tests = testGroup "Unittests"
  [ramGroup]

main :: IO ()
main = defaultMain tests
