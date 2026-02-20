-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Tests.Protocols.ReqResp (tests) where

import Clash.Prelude as C

import Clash.Hedgehog.Sized.Vector (genVec)
import Hedgehog (Gen, Property)
import Protocols
import Protocols.Hedgehog (defExpectOptions)
import Protocols.ReqResp as ReqResp
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog.Extra (testProperty)
import Test.Tasty.TH (testGroupGenerator)

import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Protocols.Df as Df
import qualified Protocols.Df.Extra as Df
import qualified Protocols.Hedgehog as PH

smallInt :: Gen Int
smallInt = Gen.integral (Range.linear 0 10)

genStalls :: (KnownNat n) => Gen (Vec n ((StallAck, [Int])))
genStalls = do
  numStalls <- smallInt
  genVec (PH.genStalls smallInt numStalls PH.Stall)

prop_fromDfs_toDfs_id :: Property
prop_fromDfs_toDfs_id = H.property $ do
  stalls <- H.forAll genStalls
  let
    impl ::
      (HiddenClockResetEnable dom) =>
      Circuit (Df dom Int) (Df dom Integer)
    impl = circuit $ \reqIn -> do
      (reqResp, respOut) <- ReqResp.fromDfs -< reqIn
      req0 <- ReqResp.toDfs -< (reqResp, req1)
      req1 <-
        applyC (fmap (fmap toInteger)) id <| stallC def stalls <| Df.bypassFifo d2 (Df.fifo d8) -< req0
      idC -< respOut

  PH.idWithModelSingleDomainT @System
    defExpectOptions
    gen
    (\_ _ _ -> fmap toInteger)
    (exposeClockResetEnable impl)
 where
  gen :: Gen [Int]
  gen = Gen.list (Range.linear 0 10) smallInt

tests :: TestTree
tests = $(testGroupGenerator)
