-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Protocols.Plugin #-}

module Tests.Protocols.BiDf (tests) where

import Clash.Prelude as C

import Clash.Hedgehog.Sized.Vector (genVec)
import Hedgehog (Gen, Property, (===))
import Protocols
import Protocols.BiDf as BiDf
import Protocols.Hedgehog (ExpectOptions (..), defExpectOptions, propWithModelSingleDomainT)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog.Extra (testProperty)
import Test.Tasty.TH (testGroupGenerator)

import qualified Data.Maybe as Maybe
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Internal.Shrink as Shrink
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

-- | Ensure that 'prefetch' behaves as an identity when paired with loopback.
prop_prefetch_identity_invalidate :: Property
prop_prefetch_identity_invalidate = H.property $ do
  stalls <- H.forAll genStalls
  let
    impl = exposeClockResetEnable $ circuit $ \(req, invalidate0) -> do
      (biDf0, resp) <- BiDf.fromDfs -< req
      biDf1 <- BiDf.trace "Input" -< biDf0
      invalidate1 <- Df.trace "Invalidate" -< invalidate0
      BiDf.loopback id
        <| BiDf.mapC (Df.fifo d8) idC
        <| stallC simConfig stalls
        <| BiDf.trace "Output"
        <| BiDf.prefetch @System
        -< (biDf1, invalidate1)
      idC -< resp

  propWithModelSingleDomainT @System
    expectOptions
    gen
    (\_ _ _ -> fst)
    impl
    (===)
 where
  simConfig = def
  expectOptions = defExpectOptions{eoResetCycles = 5, eoSampleMax = 10000, eoStopAfterEmpty = Just 1000}
  gen = do
    requests <- Gen.shrink Shrink.list $ Gen.list (Range.linear 0 10) smallInt
    invalidates <- Gen.shrink Shrink.list $ Gen.list (Range.linear 0 10) (Gen.maybe smallInt)
    return (requests, invalidates)

case_prefetch_identity_tb :: Assertion
case_prefetch_identity_tb = do
  let
    stalls = (StallWithNack, []) :> (StallWithNack, []) :> Nil
    stallsDrive = (StallWithNack, []) :> (StallWithNack, []) :> Nil
    gen =
      ( fmap Just [1 :: Int, 0, 0]
      ,
        [ Nothing
        , Nothing
        , Nothing
        , Nothing
        , Nothing
        ]
      )
    simConfig = def{timeoutAfter = 1000}
    impl = exposeClockResetEnable $ circuit $ \(req, invalidate0) -> do
      (biDf0, resp) <- BiDf.fromDfs -< req
      biDf1 <- BiDf.trace "Input" -< biDf0
      invalidate1 <- Df.trace "Invalidate" -< invalidate0
      BiDf.loopback id
        <| BiDf.mapC (Df.fifo d8) idC
        <| stallC simConfig stalls
        <| BiDf.trace "Output"
        <| BiDf.prefetch @System @Int @Int
        -< (biDf1, invalidate1)
      idC -< resp
    result =
      sampleC simConfig
        $ impl clockGen resetGen enableGen
        <| stallC simConfig stallsDrive
        <| driveC simConfig gen
  assertEqual "Pass" [1, 0, 0] (Maybe.catMaybes result)

tests :: TestTree
tests = $(testGroupGenerator)
