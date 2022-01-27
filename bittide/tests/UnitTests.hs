{-|
Copyright:           Copyright © 2022, Google LLC
License:             Apache-2.0
Maintainer:          devops@qbaylogic.com
|-}
module Main where

import Test.Tasty
import Tests.ScatterGather

tests :: TestTree
tests = testGroup "Unittests"
  [sgGroup]

main :: IO ()
main = defaultMain tests
