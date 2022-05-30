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
import Data.String

linkGroup :: TestTree
linkGroup = testGroup "Link group"
  [testPropertyNamed "txUnit can be set to continuously transmit the preamble and sequence counter."
    "txSendSC" txSendSC]

-- | Configuration for either a txUnit or tyUnit.
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

-- | Generates a 'TestConfig' for a txUnit or tyUnit.
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

-- | Use SNat values obtained from a 'TestConfig' to configure a 'txUnit'.
configTxUnit ::
  forall bs aw pw fw scw .
  ( HiddenClockResetEnable System, 1 <= bs, 2 <= aw, 1 <= pw, 1 <= fw, 1 <= scw) =>
  SNat bs -> SNat aw -> SNat pw -> SNat fw -> SNat scw ->
    (BitVector pw
    -> Signal System (Unsigned scw)
    -> Signal System (WishboneM2S bs aw)
    -> Signal System (DataLink fw)
    -> (Signal System (WishboneS2M bs), Signal System (DataLink fw)))
configTxUnit SNat SNat SNat SNat SNat = txUnit @System @bs @aw @pw @fw @scw

-- | Use SNat values obtained from a 'TestConfig' to configure a 'rxUnit'.
configRxUnit ::
  forall bs aw pw fw scw .
  ( HiddenClockResetEnable System, 1 <= bs, 2 <= aw, 1 <= pw, 1 <= fw, 1 <= scw) =>
  SNat bs -> SNat aw -> SNat pw -> SNat fw -> SNat scw ->
    (  BitVector pw
    -> Signal System (WishboneM2S bs aw)
    -> Signal System (DataLink fw)
    -> Signal System (Unsigned scw)
    -> Signal System (WishboneS2M bs))
configRxUnit SNat SNat SNat SNat SNat = rxUnit @System @bs @aw @pw @fw @scw

-- | Tests whether the transmission of a static preamble and static seqCounter works.
-- It does so by simulating the 'txUnit' for a number of cycles in transparent mode,
-- then simulating it in transmission mode, then again in transparent mode. Transparent
-- and transmission mode refer to passing through the incoming link and transmitting
-- preamble + sequence counter respectively.
txSendSC :: Property
txSendSC = property $ do
  tc <- forAll genTestConfig
  case tc of
    (TestConfig bs@(SNat :: SNat bs) aw@(SNat :: SNat aw) pw@(SNat :: SNat pw) fw@(SNat :: SNat fw) scw@(SNat :: SNat scw)) -> do
      iterations <- forAll $ Gen.enum 1 10
      preamble <- forAll $ pack <$> genUnsigned Range.constantBounded
      let
        pwNum = snatToNum pw
        fwNum = snatToNum fw
        scwNum = snatToNum scw
        pwFrames = pwNum `divRU` fwNum
        scFrames = scwNum `divRU` fwNum
        iterationDuration = pwFrames + scFrames
        iterationsTotal = iterations * iterationDuration
      -- First two cycles output depend on the initial state.
      -- The txUnit's state machine starts after the control register has been updated,
      -- causing an extra cycle of delay.
      offTime0 <- forAll $ Gen.enum 2 $ max 2 iterationsTotal
      onTime   <- forAll $ Gen.enum 1 iterationsTotal
      offTime1 <- forAll $ Gen.enum 1 iterationsTotal
      let
        simLength = offTime0 + onTime + offTime1
        simRange = Range.singleton simLength
        genFrame = Gen.maybe (pack <$> genUnsigned Range.constantBounded)
      framesIn <- forAll $ Gen.list simRange genFrame
      seqCountIn <-forAll . Gen.list simRange $ genUnsigned Range.constantBounded
      let
        topEntity (unbundle -> (scIn, wbIn0, linkIn)) = bundle $ withClockResetEnable
          clockGen resetGen enableGen configTxUnit bs aw pw fw scw preamble scIn wbIn0 linkIn
        -- Compensate for the register write + state machine start delays.
        wbOff0 = L.replicate (offTime0-2) emptyWishboneM2S
        wbOn = startSignal : L.replicate onTime emptyWishboneM2S
        wbOff1 = stopSignal : L.repeat emptyWishboneM2S
        wbIn = wbOff0 <> wbOn <> wbOff1
        RegisterBank (toList . fmap Just-> preambleFrames) = getRegs @_ @fw preamble
        topEntityInput = L.zip3 seqCountIn wbIn framesIn
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
        -- It takes 1 cycle for the the control register to be updated by the wishbone bus.
        (offFrames0, L.drop onTime -> offFrames1) = L.splitAt offTime0 framesIn
        onFrames = L.take onTime $ preambleFrames L.++ L.concatMap ((L.++ preambleFrames) . fmap Just . secToFrames) (everyNth iterationDuration $ L.drop (min (simLength - 1) (offTime0 + pwFrames - 1)) seqCountIn)
        expectedOutput = L.take simLength $ offFrames0 <> onFrames <> offFrames1
      footnote . fromString $ "iterationsDuration: " <> showX iterationDuration
      footnote . fromString $ "droppedSCs: " <> showX offTime0 <> " + " <> showX pwFrames <> " - 1 = " <> showX (offTime0 + pwFrames - 1)
      footnote . fromString $ "simOut: " <> showX simOut
      footnote . fromString $ "expected: " <> showX expectedOutput
      footnote . fromString $ "onFrames: " <> showX onFrames
      footnote . fromString $ "framesIn: " <> showX (L.take simLength framesIn)
      footnote . fromString $ "wbOut: " <> showX wbOut
      footnote . fromString $ "wbIn: " <> showX (L.take simLength wbIn)
      simOut === expectedOutput

divRU :: Integral a => a -> a -> a
divRU b a = (b + a - 1) `div` a

secToFrames :: forall regSize a . (KnownNat regSize, Paddable a) => a -> [BitVector regSize]
secToFrames sc = out
  where
  RegisterBank (toList -> out) = getRegs sc

everyNth :: Int -> [a] -> [a]
everyNth 0 _ = error "Can not take every 0th element."
everyNth _ [] = []
everyNth n (x:xs) = x : everyNth n (L.drop (n-1) xs)
