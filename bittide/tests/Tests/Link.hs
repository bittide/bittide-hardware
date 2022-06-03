-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Tests.Link where

import Clash.Prelude hiding (fromList)
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
import Clash.Sized.Vector
import Data.Maybe

linkGroup :: TestTree
linkGroup = testGroup "Link group"
  [ testPropertyNamed "txUnit can be set to continuously transmit the preamble and sequence counter."
    "txSendSC" txSendSC
  , testPropertyNamed "rxUnit can be set to continuously detect the preamble and store the sequence counter."
    "rxSendSC" rxSendSC
  , testPropertyNamed "Integration test with txUnit sending a preamble and sequence counter to rxUnit."
    "integration" integration]

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
  (TN.someNatVal -> (SomeNat (snatProxy -> aw))) <- Gen.enum 8 64
  (TN.someNatVal -> (SomeNat (snatProxy -> pw))) <- Gen.enum 8 1024
  (TN.someNatVal -> (SomeNat (snatProxy -> fw))) <- Gen.enum 32 128
  (TN.someNatVal -> (SomeNat (snatProxy -> scw))) <- Gen.enum 8 128
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
        pwNrOfFrames = pwNum `divRU` fwNum
        scNumOfFrames = scwNum `divRU` fwNum
        iterationDuration = pwNrOfFrames + scNumOfFrames
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
        onFrames = L.take onTime $ preambleFrames L.++ L.concatMap ((L.++ preambleFrames) . fmap Just . secToFrames) (everyNth iterationDuration $ L.drop (min (simLength - 1) (offTime0 + pwNrOfFrames - 1)) seqCountIn)
        expectedOutput = L.take simLength $ offFrames0 <> onFrames <> offFrames1
      footnote . fromString $ "iterationsDuration: " <> showX iterationDuration
      footnote . fromString $ "droppedSCs: " <> showX offTime0 <> " + " <> showX pwNrOfFrames <> " - 1 = " <> showX (offTime0 + pwNrOfFrames - 1)
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

-- | Tests whether the transmission of a static preamble and static seqCounter works.
-- It does so by simulating the 'txUnit' for a number of cycles in transparent mode,
-- then simulating it in transmission mode, then again in transparent mode.
rxSendSC :: Property
rxSendSC = property $ do
  tc <- forAll genTestConfig
  case tc of
    (TestConfig _ aw@(SNat :: SNat aw) pw@(SNat :: SNat pw) fw@(SNat :: SNat fw) scw@(SNat :: SNat scw)) -> do
      iterations <- forAll $ Gen.enum 1 3
      preamble <- forAll $ replaceBit (natToNum @pw - 1:: Integer ) 1 . pack <$> genUnsigned Range.constantBounded
      let
        -- We hard code the number of bytes to 4 because 'registerWB' assumes a 4 byte aligned address.
        bs = d4
        bsNum = snatToNum bs
        pwNum = snatToNum pw
        fwNum = snatToNum fw
        scwNum = snatToNum scw
        pwNrOfFrames = pwNum `divRU` fwNum
        scNumOfFrames = scwNum `divRU` fwNum
        iterationDuration = pwNrOfFrames + scNumOfFrames
        iterationsTotal = iterations * iterationDuration
        readBackCycles = (2 * scwNum) `divRU` (bsNum * 8)
      -- First two cycles output depend on the initial state.
      -- The txUnit's state machine starts after the control register has been updated,
      -- causing an extra cycle of delay.
      offTime0 <- forAll $ Gen.enum 2 (2 + iterationsTotal)
      onTime   <- forAll $ Gen.enum (2 + iterationDuration) iterationsTotal
      offTime1 <- forAll $ Gen.enum (2 + readBackCycles) (readBackCycles + iterationDuration)
      remoteSeqCounts <- forAll . Gen.list (Range.singleton (1 + iterations)) $ genUnsigned Range.constantBounded
      let
        simLength = offTime0 + onTime + offTime1
        simRange = Range.singleton simLength
        genFrame = Gen.maybe (pack <$> genUnsigned Range.constantBounded)
        filterPreambles = Gen.filter (not . containsPreamble @pw @fw preamble . (L.replicate (pwNrOfFrames-1) 0 <>) . catMaybes . (<> L.take (pwNrOfFrames - 1) onFrames))
        RegisterBank (toList . fmap Just -> preambleFrames) = getRegs @_ @fw preamble
        onFrames = L.take onTime $ L.concatMap ((preambleFrames <>) . fmap Just . secToFrames) (remoteSeqCounts :: [Unsigned scw])
      offFrames0 <- forAll $ filterPreambles (Gen.list (Range.singleton offTime0) genFrame)
      offFrames1 <- forAll $ Gen.list (Range.singleton offTime1) genFrame
      localSeqCounts <- forAll . Gen.list simRange $ genUnsigned Range.constantBounded
      let
        topEntity (unbundle -> (wbIn0, linkIn, localCounter)) = withClockResetEnable
          clockGen resetGen enableGen configRxUnit bs aw pw fw scw preamble wbIn0 linkIn localCounter
        -- Compensate for the register write + state machine start delays.
        wbIn = wbWrite : L.replicate (offTime0 + onTime) (wbRead 0) <> cycle (fmap wbRead [0..fromIntegral readBackCycles])
        framesIn = offFrames0 <> onFrames <> offFrames1
        topEntityInput = L.zip3 wbIn framesIn localSeqCounts
        wbWrite = (emptyWishboneM2S @4 @aw)
          { addr = 0 :: BitVector aw
          , writeEnable = True
          , writeData = 1 :: BitVector 32
          , busSelect = maxBound
          , busCycle = True
          , strobe = True
          }
        wbRead (a :: BitVector aw) = (emptyWishboneM2S @4 @aw)
          { addr = shiftL a 2
          , busCycle = True
          , strobe = True
          }
        simOut = simulateN simLength topEntity topEntityInput
        decodedOutput = directedDecoding wbIn simOut :: [(Unsigned scw, Unsigned scw)]
        storedOutput = L.last decodedOutput
        expected = (localSeqCounts L.!! (offTime0 + pwNrOfFrames), L.head remoteSeqCounts)
      -- It takes 1 cycle for the the control register to be updated by the wishbone bus.
      footnote . fromString $ "wishbone reads per result: " <> show (natToNum @(Regs (Unsigned scw, Unsigned scw) 32) :: Integer)
      footnote . fromString $ "iterationsDuration: " <> showX iterationDuration
      footnote . fromString $ "simOut: " <> showX simOut
      footnote . fromString $ "decodedOut: " <> showX decodedOutput
      footnote . fromString $ "expected: " <> showX expected
      footnote . fromString $ "onFrames: " <> showX onFrames
      footnote . fromString $ "framesIn: " <> showX (L.take simLength framesIn)
      footnote . fromString $ "wbIn: " <> showX (L.take simLength wbIn)
      storedOutput === expected
 where
  directedDecoding :: forall bs aw a . (KnownNat bs, 1 <= bs, KnownNat aw, Paddable a) => [WishboneM2S bs aw] -> [WishboneS2M bs] -> [a]
  directedDecoding (m2s:m2ss) (s2m:s2ms)
    | slvAck && firstFrame && storedSC && isJust decodingData = decoded : uncurry directedDecoding rest
    | otherwise = directedDecoding m2ss s2ms
   where
    slvAck = acknowledge s2m
    firstFrame = addr m2s == 0
    storedSC = readData s2m == 0
    bothLists = L.zip m2ss s2ms
    (fmap snd -> decodingFrames, L.unzip -> rest) = span (\(m,s) -> acknowledge s && (/= 0) (addr m)) bothLists
    decodingData = fromList . fmap readData $ L.reverse decodingFrames
    decoded = registersToData . RegisterBank $ fromJust decodingData
  directedDecoding _ _ = []

containsPreamble :: forall preamble frameWidth . (KnownNat preamble, KnownNat frameWidth, 1 <= frameWidth) => BitVector preamble -> [BitVector frameWidth] -> Bool
containsPreamble preamble = f
 where
  len = natToNum @(DivRU preamble frameWidth)
  f [] = False
  f (inp:utList)
    | L.length frameslist == len && registersToData (RegisterBank (unsafeFromList frameslist)) == preamble = True
    | otherwise = f utList
   where
     frameslist = L.take len (inp:utList)

-- | Tests whether the transmission of a static preamble and static seqCounter works.
-- It does so by simulating the 'txUnit' for a number of cycles in transparent mode,
-- then simulating it in transmission mode, then again in transparent mode.
integration :: Property
integration = property $ do
  tc <- forAll genTestConfig
  linkLatency <- forAll $ Gen.int (Range.constant 0 64)
  case (TN.someNatVal (fromIntegral linkLatency), tc) of
    (SomeNat llProxy, TestConfig _ aw@(SNat :: SNat aw) pw@(SNat :: SNat pw) fw@(SNat :: SNat fw) scw@(SNat :: SNat scw)) -> do
      iterations <- forAll $ Gen.enum 1 3
      preamble <- forAll $ replaceBit (natToNum @pw - 1:: Integer ) 1 . pack <$> genUnsigned Range.constantBounded
      let
        -- We hard code the number of bytes to 4 because 'registerWB' assumes a 4 byte aligned address.
        bs = d4
        bsNum = snatToNum bs
        pwNum = snatToNum pw
        fwNum = snatToNum fw
        scwNum = snatToNum scw
        pwNrOfFrames = pwNum `divRU` fwNum
        scNumOfFrames = scwNum `divRU` fwNum
        iterationDuration = pwNrOfFrames + scNumOfFrames
        iterationsTotal = iterations * iterationDuration
        readBackCycles = (2 * scwNum) `divRU` (bsNum * 8)
      -- First two cycles output depend on the initial state.
      -- The txUnit's state machine starts after the control register has been updated,
      -- causing an extra cycle of delay.
      delayCycles <- forAll $ Gen.enum 2 (2 + iterationsTotal)
      transmitCycles  <- forAll $ Gen.enum iterationDuration iterationsTotal
      posTransmitCycles <- forAll $ Gen.enum (linkLatency + 2 * ( 1 + readBackCycles)) (linkLatency + 2 * ( 1 + readBackCycles) + iterationsTotal)
      let
        simLength = delayCycles + transmitCycles + posTransmitCycles
        genFrame = Gen.maybe (pack <$> genUnsigned Range.constantBounded)
        RegisterBank  (fmap Just . toList -> preambleFrames) = getRegs preamble
        filterPreambles = Gen.filter (not . containsPreamble @pw @fw preamble . (L.replicate (pwNrOfFrames-1) 0 <>) . catMaybes . (<> L.take (pwNrOfFrames - 1) preambleFrames))

      gatherFrames0 <- forAll $ filterPreambles (Gen.list (Range.singleton delayCycles) genFrame)
      gatherFrames1 <- forAll $ Gen.list (Range.singleton (simLength - delayCycles)) genFrame
      let
        topEntity :: HiddenClockResetEnable System =>
          ( Signal System (Maybe (BitVector fw)
          , Unsigned scw
          , WishboneM2S 4 aw
          , WishboneM2S 4 aw)
          -> Signal System (DataLink fw, WishboneS2M 4))
        topEntity (unbundle -> (gatherOut, counter, wbTxM2S, wbRxM2S)) = bundle (linkIn, wbRxS2M)
         where
          (wbTxS2M, linkIn) = configTxUnit bs aw pw fw scw preamble counter wbTxM2S gatherOut
          linkOut = registerN (snatProxy llProxy) Nothing linkIn
          wbRxS2M = configRxUnit bs aw pw fw scw preamble wbRxM2S linkOut counter
        -- Compensate for the register write + state machine start delays.
        wbTxIn = L.replicate (delayCycles - 2) emptyWishboneM2S <> (wbWrite 0 1 : L.replicate transmitCycles emptyWishboneM2S) <> (wbWrite 0 0 : L.repeat emptyWishboneM2S)
        wbRxIn = wbWrite 0 1 : L.replicate (delayCycles + transmitCycles - 1) (wbRead 0) <> cycle (fmap wbRead [0..fromIntegral readBackCycles])
        counters = [0..]
        framesIn = gatherFrames0 <> gatherFrames1
        topEntityInput = L.zip4 framesIn counters wbTxIn wbRxIn
        wbWrite a x = (emptyWishboneM2S @4 @aw)
          { addr = shiftL a 2
          , writeEnable = True
          , writeData =x
          , busSelect = maxBound
          , busCycle = True
          , strobe = True
          }
        wbRead (a :: BitVector aw) = (emptyWishboneM2S @4 @aw)
          { addr = shiftL a 2
          , busCycle = True
          , strobe = True
          }
        (linkIn, wbRxOut) = L.unzip $ simulateN simLength topEntity topEntityInput
        decodedOutput = directedDecoding wbRxIn wbRxOut :: [(Unsigned scw, Unsigned scw)]
        storedOutput = L.last decodedOutput
        expected = (fromIntegral (delayCycles + linkLatency + pwNrOfFrames), fromIntegral (delayCycles + pwNrOfFrames - 1))
      -- It takes 1 cycle for the the control register to be updated by the wishbone bus.
      footnote . fromString $ "wishbone reads per result: " <> show (natToNum @(Regs (Unsigned scw, Unsigned scw) 32))
      footnote . fromString $ "iterationsDuration: " <> showX iterationDuration
      footnote . fromString $ "linkIn: " <> showX linkIn
      footnote . fromString $ "wbRxOut: " <> showX wbRxOut
      footnote . fromString $ "wbRxIn: " <> showX (L.take simLength wbRxIn)
      footnote . fromString $ "decodedOut: " <> showX decodedOutput
      footnote . fromString $ "expected: " <> showX expected
      footnote . fromString $ "framesIn: " <> showX (L.take simLength framesIn)
      storedOutput === expected
 where
  directedDecoding :: forall bs aw a . (KnownNat bs, 1 <= bs, KnownNat aw, Paddable a) => [WishboneM2S bs aw] -> [WishboneS2M bs] -> [a]
  directedDecoding (m2s:m2ss) (s2m:s2ms)
    | slvAck && firstFrame && storedSC && isJust decodingData = decoded : uncurry directedDecoding rest
    | otherwise = directedDecoding m2ss s2ms
   where
    slvAck = acknowledge s2m
    firstFrame = addr m2s == 0
    storedSC = readData s2m == 0
    bothLists = L.zip m2ss s2ms
    (fmap snd -> decodingFrames, L.unzip -> rest) = span (\(m,s) -> acknowledge s && (/= 0) (addr m)) bothLists
    decodingData = fromList . fmap readData $ L.reverse decodingFrames
    decoded = registersToData . RegisterBank $ fromJust decodingData
  directedDecoding _ _ = []

registerN ::
  (HiddenClockResetEnable dom, NFDataX o, KnownNat n) =>
  SNat n -> o -> Signal dom o -> Signal dom o
registerN n a = mealy go (replicate n a)
 where
  go state0 inp = (state1, out)
   where
    (out :> state1) = state0 :< inp

wcre :: KnownDomain dom => (HiddenClockResetEnable dom => r) -> r
wcre = withClockResetEnable clockGen resetGen enableGen
