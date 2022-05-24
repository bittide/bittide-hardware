-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Tests.Link where

import Clash.Prelude
import Clash.Hedgehog.Sized.Unsigned

import Bittide.Extra.Wishbone
import Bittide.Link

import Hedgehog
import Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import qualified Data.List as L
import qualified GHC.TypeNats as TN
import qualified Hedgehog.Gen as Gen hiding (resize)

import Bittide.SharedTypes
import Data.Proxy
import Data.String


linkGroup :: TestTree
linkGroup = testGroup "Link group"
  [testPropertyNamed "txUnit can be set to continuously transmit the preamble and sequence counter."
    "txSendSC" txSendSC]

data TestConfig where
  TestConfig ::
    ( 1 <= bytes
    , 2 <= addressWidth
    , 1 <= preambleWidth
    , 1 <= frameWidth
    , 1 <= seqCountWidth) =>
    SNat bytes ->
    SNat addressWidth ->
    SNat preambleWidth ->
    SNat frameWidth ->
    SNat seqCountWidth ->
    TestConfig
deriving instance Show TestConfig

genTestConfig :: Gen TestConfig
genTestConfig = do
  (TN.someNatVal -> (SomeNat (snatProxy -> bs))) <- Gen.enum 1 8
  (TN.someNatVal -> (SomeNat (snatProxy -> aw))) <- Gen.enum 2 64
  (TN.someNatVal -> (SomeNat (snatProxy -> pw))) <- Gen.enum 1 1024
  (TN.someNatVal -> (SomeNat (snatProxy -> fw))) <- Gen.enum 1 128
  (TN.someNatVal -> (SomeNat (snatProxy -> scw))) <- Gen.enum 1 128
  case
    ( compareSNat d1 bs
    , compareSNat d2 aw
    , compareSNat d1 pw
    , compareSNat d1 fw
    , compareSNat d1 scw) of
      (SNatLE, SNatLE, SNatLE, SNatLE, SNatLE) -> pure $ TestConfig bs aw pw fw scw
      _ -> error "genTestConfig: Generated configuration does not satisfy constraints."

configTxUnit ::
  forall bs aw pw fw scw .
  ( HiddenClockResetEnable System, 1 <= bs, 2 <= aw, 1 <= pw, 1 <= fw, 1 <= scw) =>
  SNat bs -> SNat aw -> SNat pw -> SNat fw -> SNat scw ->
    (BitVector pw
    -> Signal System (BitVector scw)
    -> Signal System (WishboneM2S bs aw)
    -> Signal System (DataLink fw)
    -> (Signal System (WishboneS2M bs), Signal System (DataLink fw)))
configTxUnit SNat SNat SNat SNat SNat = txUnit @System @bs @aw @pw @fw @scw

configRxUnit ::
  forall bs aw pw fw scw .
  ( HiddenClockResetEnable System, 1 <= bs, 2 <= aw, 1 <= pw, 1 <= fw, 1 <= scw) =>
  SNat bs -> SNat aw -> SNat pw -> SNat fw -> SNat scw ->
    (Proxy scw
    -> BitVector pw
    -> Signal System (WishboneM2S bs aw)
    -> Signal System (DataLink fw)
    -> Signal System (WishboneS2M bs))
configRxUnit SNat SNat SNat SNat SNat = rxUnit @System @bs @aw @pw @fw @scw

txSendSC :: Property
txSendSC = property $ do
  tc <- forAll genTestConfig
  case tc of
    (TestConfig bs@(SNat :: SNat bs) aw@(SNat :: SNat aw) pw@(SNat :: SNat pw) fw@(SNat :: SNat fw) scw@(SNat :: SNat scw)) -> do
      iterations <- forAll $ Gen.enum 1 10
      let
        pwNum = snatToNum pw
        fwNum = snatToNum fw
        scwNum = snatToNum scw
        iterationDuration = (pwNum `divRU` fwNum) + (scwNum `divRU` fwNum)
        iterationsTotal = iterations * iterationDuration
      -- First two cycles output depend on the initial state.
      -- The txUnit's state machine starts after the control register has been updated,
      -- causing an extra cycle of delay.
      offTime0 <- forAll $ Gen.enum 2 $ max 3 iterationsTotal
      onTime   <- forAll $ Gen.enum 1 iterationsTotal
      offTime1 <- forAll $ Gen.enum 0 iterationsTotal
      let
        simLength = offTime0 + onTime + offTime1
        simRange = Range.singleton simLength
        genFrame = Gen.maybe (pack <$> genUnsigned Range.constantBounded)
      preamble <- forAll $ pack <$> genUnsigned Range.constantBounded
      seqCount <- forAll $ genUnsigned Range.constantBounded
      framesIn <- forAll $ Gen.list simRange genFrame
      let
        topEntity (unbundle -> (scIn, wbIn0, linkIn)) = bundle $ withClockResetEnable
          clockGen resetGen enableGen configTxUnit bs aw pw fw scw preamble scIn wbIn0 linkIn
        -- Compensate for the register write + state machine start delays.
        wbOff0 = L.replicate (offTime0-2) emptyWishboneM2S
        wbOn = startSignal : L.replicate onTime emptyWishboneM2S
        wbOff1 = stopSignal : L.repeat emptyWishboneM2S
        wbIn = wbOff0 <> wbOn <> wbOff1
        topEntityInput = L.zip3 (L.repeat seqCount) wbIn framesIn
        startSignal = (emptyWishboneM2S @bs @aw)
          { addr = 0
          , writeEnable = True
          , writeData = 1
          , busSelect = maxBound
          , busCycle = True
          , strobe = True
          }
        stopSignal = (emptyWishboneM2S @bs @aw)
          { addr = 0
          , writeEnable = True
          , writeData = 0
          , busSelect = maxBound
          , busCycle = True
          , strobe = True
          }
        (wbOut, simOut) = L.unzip $ simulateN simLength topEntity topEntityInput
        RegisterBank (toList . fmap Just -> preambleFrames) = getRegs @_ @fw preamble
        RegisterBank (toList . fmap Just -> secCountFames) = getRegs @_ @fw seqCount

        -- It takes 1 cycle for the the control register to be updated by the wishbone bus.
        (offFrames0, L.drop onTime -> offFrames1) = L.splitAt offTime0 framesIn
        onFrames = L.take onTime (cycle $ preambleFrames <> secCountFames)
        expectedOutput = offFrames0 <> onFrames <> offFrames1

      footnote . fromString $ "simOut: " <> showX simOut
      footnote . fromString $ "framesIn: " <> showX (L.take simLength framesIn)
      footnote . fromString $ "wbOut: " <> showX wbOut
      footnote . fromString $ "wbIn: " <> showX (L.take simLength wbIn)
      simOut === expectedOutput
 where
  divRU a b = (b + a - 1) `div` a
