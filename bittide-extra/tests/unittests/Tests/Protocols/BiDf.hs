-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Protocols.Plugin #-}

module Tests.Protocols.BiDf (tests) where

import Clash.Prelude as C

import Clash.Hedgehog.Sized.Vector (genVec)
import Data.Typeable (Typeable)
import Hedgehog (Gen, Property, (===))
import Protocols
import Protocols.BiDf as BiDf
import Protocols.Hedgehog (ExpectOptions (..), defExpectOptions, propWithModelSingleDomainT)
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog.Extra (testProperty)
import Test.Tasty.TH (testGroupGenerator)

import qualified Data.List as L
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Protocols.BiDf as BiDf
import qualified Protocols.Df as Df
import qualified Protocols.Df.Extra as Df
import qualified Protocols.Hedgehog as PH

smallInt :: Gen Int
smallInt = Gen.integral (Range.linear 0 10)

genStalls :: (KnownNat n) => Gen (Vec n ((StallAck, [Int])))
genStalls = do
  numStalls <- smallInt
  genVec (PH.genStalls smallInt numStalls PH.Stall)

prop_loopback_id :: Property
prop_loopback_id = H.property $ do
  stalls <- H.forAll genStalls
  let
    impl ::
      forall dom a.
      (HiddenClockResetEnable dom, NFDataX a, Show a, ShowX a) =>
      Circuit (Df dom a) (Df dom a)
    impl = circuit $ \req -> do
      (biDf, resp) <- BiDf.fromDfs -< req
      BiDf.loopback id <| BiDf.mapC (Df.fifo d8) idC <| stallC simConfig stalls -< biDf
      idC -< resp
  PH.idWithModelSingleDomainT @System
    defExpectOptions
    gen
    (\_ _ _ -> id)
    (exposeClockResetEnable impl)
 where
  simConfig = def
  gen :: Gen [Int]
  gen = Gen.list (Range.linear 0 10) smallInt

-- | Ensure that 'prefetch2' behaves as an identity when paired with loopback.
prop_prefetch2_identity :: Property
prop_prefetch2_identity = H.property $ do
  stalls <- H.forAll genStalls
  let
    impl :: (HiddenClockResetEnable System) => Circuit (Df System Int) (Df System Int)
    impl = circuit $ \req -> do
      (biDf, resp) <- BiDf.fromDfs -< req
      BiDf.loopback id
        <| BiDf.mapC (Df.fifo d8) idC
        <| stallC simConfig stalls
        <| BiDf.prefetch2 @System
        -< biDf
      idC -< resp

  propWithModelSingleDomainT @System
    expectOptions
    gen
    (\_ _ _ -> id)
    (exposeClockResetEnable impl)
    (===)
 where
  simConfig = def
  dfNames name = [name <> "_fwd", name <> "_bwd"]
  bidefNames name =
    L.concatMap
      (\suffix -> [name <> "_request" <> suffix, name <> "_response" <> suffix])
      (dfNames name)
  expectOptions = defExpectOptions{eoResetCycles = 5} -- , eoDumpVcd = Just ("prefetch2.vcd", ["prefetch_request_fwd", "prefetch_request_bwd", "prefetch_response_fwd", "prefetch_response_bwd"])}
  gen :: Gen [Int]
  gen = Gen.list (Range.linear 10 10) smallInt

tests :: TestTree
tests = $(testGroupGenerator)
