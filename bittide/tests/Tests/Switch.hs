{-|
Copyright:           Copyright © 2022, Google LLC
License:             Apache-2.0
Maintainer:          devops@qbaylogic.com
|-}
module Bittide.Testing.Tests.Switch where

import Prelude
import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.Hedgehog

import Hedgehog ((===))
import qualified HedgeHog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Bittide.ScatterGather

genA = Gen.set (Range.linear 0 100)

