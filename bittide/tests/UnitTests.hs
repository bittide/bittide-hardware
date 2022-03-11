{-|
Copyright:           Copyright © 2022, Google LLC
License:             Apache-2.0
Maintainer:          devops@qbaylogic.com
|-}
module Main where

import Test.Tasty

tests :: TestTree
tests = testGroup "Unittests" []

main :: IO ()
main = defaultMain tests
