-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Protocols.Plugin #-}

module Tests.Protocols.ReqResp (tests) where

import Clash.Prelude as C

import Clash.Hedgehog.Sized.Vector (genVec)
import Hedgehog (Gen, Property, (===))
import Protocols
import Protocols.BiDf as BiDf
import Protocols.Hedgehog (ExpectOptions (..), defExpectOptions, propWithModelSingleDomainT)
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog.Extra (testProperty)
import Test.Tasty.TH (testGroupGenerator)

import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Protocols.Df as Df
import qualified Protocols.Hedgehog as PH
import qualified Protocols.ReqResp as ReqResp

smallInt :: Gen Int
smallInt = Gen.integral (Range.linear 0 10)

genStalls :: (KnownNat n) => Gen (Vec n ((StallAck, [Int])))
genStalls = do
  numStalls <- smallInt
  genVec (PH.genStalls smallInt numStalls PH.Stall)

-- | Ensure that 'prefetch' behaves as an identity when paired with loopback.
prop_prefetch_identity :: Property
prop_prefetch_identity = H.property $ do
  stalls <- H.forAll genStalls
  let
    impl = exposeClockResetEnable $ circuit $ \(req, invalidate) -> do
      (reqResp, resp) <- ReqResp.fromDfs -< req
      BiDf.loopback id
        <| BiDf.mapC (Df.fifo d8) idC
        <| stallC simConfig stalls
        <| ReqResp.prefetch @System
        -< (reqResp, invalidate)
      idC -< resp

  propWithModelSingleDomainT @System
    expectOptions
    gen
    (\_ _ _ -> fst)
    impl
    (===)
 where
  simConfig = def
  expectOptions = defExpectOptions{eoResetCycles = 5}
  gen = do
    requests <- Gen.list (Range.linear 0 10) smallInt
    invalidates <- Gen.list (Range.linear 0 10) (Gen.maybe smallInt)
    return (requests, invalidates)

tests :: TestTree
tests = $(testGroupGenerator)
