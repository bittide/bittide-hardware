
module Main where

import Prelude

import Test.Tasty
import Test.Tasty.Hedgehog

import qualified Tests.Clash.Signal.Extra

tests :: TestTree
tests = testGroup "Tests"
  [ Tests.Clash.Signal.Extra.tests
  ]

setDefaultHedgehogTestLimit :: HedgehogTestLimit -> HedgehogTestLimit
setDefaultHedgehogTestLimit (HedgehogTestLimit Nothing) = HedgehogTestLimit (Just 1000)
setDefaultHedgehogTestLimit opt = opt

main :: IO ()
main = defaultMain $
  adjustOption setDefaultHedgehogTestLimit
  tests
