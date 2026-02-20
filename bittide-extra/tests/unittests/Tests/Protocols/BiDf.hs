-- SPDX-FileCopyrightText: 2024 B. Gamari; Google LLC
--
-- SPDX-License-Identifier: BSD-2-Clause

module Tests.Protocols.BiDf (tests) where

import Clash.Prelude as C

import Clash.Hedgehog.Sized.Vector (genVec)
import Hedgehog (Gen, Property)
import Protocols
import Protocols.BiDf as BiDf
import Protocols.Extra (fmapC)
import Protocols.Hedgehog (ExpectOptions (eoResetCycles), defExpectOptions)
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog.Extra (testProperty)
import Test.Tasty.TH (testGroupGenerator)

import qualified Data.List as L
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Protocols.Df as Df
import qualified Protocols.Df.Extra as Df
import qualified Protocols.Hedgehog as PH
import qualified Protocols.Vec as Vec

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
      BiDf.loopback id
        <| BiDf.map (Df.bypassFifo d1 $ Df.fifo d8) idC
        <| stallC simConfig stalls
        -< biDf
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

prop_fromDfs_toDfs_id :: Property
prop_fromDfs_toDfs_id = H.property $ do
  stallsA <- H.forAll genStalls
  stallsB <- H.forAll genStalls
  stallsC <- H.forAll genStalls
  stallsD <- H.forAll genStalls
  let
    impl ::
      (HiddenClockResetEnable dom) =>
      Circuit (Df dom Int) (Df dom Integer)
    impl = circuit $ \reqIn -> do
      (biDf0, respOut) <- BiDf.fromDfs -< reqIn

      -- Allow stalling and latency on `biDf0 <-> biDf1`
      biDf1 <-
        stallC simConfig stallsA
          <| BiDf.map (Df.bypassFifo d1 $ Df.fifo d4) (Df.bypassFifo d1 $ Df.fifo d4)
          <| stallC simConfig stallsB
          -< biDf0

      -- Change the type to prevent accidental miswiring
      req1 <- applyC (fmap (fmap fromIntegral)) id -< req0

      -- Allow stalling and latency on `req0 <-> req2`
      req2 <-
        stallC simConfig stallsC
          <| Df.bypassFifo d1 (Df.fifo d4)
          <| stallC simConfig stallsD
          -< req1

      req0 <- BiDf.toDfs -< (biDf1, req2)
      -- Return response
      idC -< respOut

  PH.idWithModelSingleDomainT @System
    -- There is no need to test reset behavior since `toDfs` and `fromDfs` are stateless circuits
    defExpectOptions{eoResetCycles = 0}
    gen
    (\_ _ _ -> fmap toInteger)
    (exposeClockResetEnable impl)
 where
  simConfig = def
  gen :: Gen [Int]
  gen = Gen.list (Range.linear 0 10) smallInt

prop_fanin :: Property
prop_fanin = H.property $ do
  stalls <- H.forAll genStalls
  let
    impl ::
      forall dom.
      (HiddenClockResetEnable dom) =>
      Circuit (Vec 3 (Df dom Int)) (Vec 3 (Df dom Int))
    impl = circuit $ \reqs -> do
      (biDfs, resps) <- Vec.unzip <| fmapC (BiDf.fromDfs) -< reqs
      respBiDf0 <- BiDf.fanin -< biDfs
      BiDf.loopback id
        <| BiDf.map (Df.bypassFifo d1 (Df.fifo d8)) (Df.bypassFifo d1 (Df.fifo d8))
        <| stallC simConfig stalls
        -< respBiDf0
      idC -< resps
  PH.propWithModelSingleDomainT @System
    eOpts
    gen
    (\_ _ _ -> id)
    (exposeClockResetEnable impl)
    prop
 where
  simConfig = def
  eOpts = defExpectOptions{PH.eoStopAfterEmpty = Just 300}
  gen :: Gen (Vec 3 [Int])
  gen = genVec (Gen.list (Range.linear 0 10) smallInt)
  -- Since stalling can change the order of the samples, we only check if they are all present
  prop expected sampled = L.sort (L.concat (toList sampled)) H.=== L.sort (L.concat (toList expected))

tests :: TestTree
tests = $(testGroupGenerator)
