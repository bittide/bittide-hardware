-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -fconstraint-solver-iterations=5 #-}

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Tests.Link where

import Clash.Prelude hiding (fromList)

import Clash.Hedgehog.Sized.Unsigned
import Clash.Sized.Vector
import Data.Constraint (Dict(Dict))
import Data.Constraint.Nat.Extra (divWithRemainder)
import Data.Maybe
import Data.String
import GHC.Stack
import Hedgehog
import Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import Tests.Shared
import Protocols.Wishbone

import Bittide.Link
import Bittide.SharedTypes

import qualified Data.List as L
import qualified GHC.TypeNats as TN
import qualified Hedgehog.Gen as Gen hiding (resize)
import Data.Bifunctor


tests :: TestTree
tests = testGroup "Tests.Link"
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
  (TN.someNatVal -> (SomeNat (snatProxy -> pw))) <- Gen.enum 8 64
  (TN.someNatVal -> (SomeNat (snatProxy -> fw))) <- Gen.enum 1 64
  (TN.someNatVal -> (SomeNat (snatProxy -> scw))) <- Gen.enum 4 64
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
    -> Signal System (DataLink fw)
    -> Signal System (WishboneM2S aw bs (Bytes bs))
    -> (Signal System (WishboneS2M (Bytes bs)), Signal System (DataLink fw)))
configTxUnit SNat SNat SNat SNat SNat preamble sq frameIn m2s0 =
  (s2m1, dl)
 where
  (s2m0, dl) = txUnit @System @bs @aw @pw @fw @scw preamble sq frameIn m2s1
  (m2s1, s2m1) = validateWb m2s0 s2m0


-- | Use SNat values obtained from a 'TestConfig' to configure a 'rxUnit'.
configRxUnit ::
  forall bs aw pw fw scw .
  ( HiddenClockResetEnable System, 1 <= bs, 2 <= aw, 1 <= pw, 1 <= fw, 1 <= scw) =>
  SNat bs -> SNat aw -> SNat pw -> SNat fw -> SNat scw ->
    (  BitVector pw
    -> Signal System (Unsigned scw)
    -> Signal System (DataLink fw)
    -> Signal System (WishboneM2S aw bs (Bytes bs))
    -> Signal System (WishboneS2M (Bytes bs)))
configRxUnit SNat SNat SNat SNat SNat linkIn preamble localCounter m2s0 =
  s2m1
 where
  s2m0 = rxUnit @System @bs @aw @pw @fw @scw linkIn preamble localCounter m2s1
  (m2s1, s2m1) = validateWb m2s0 s2m0

-- | Tests whether the transmission of a static preamble and static seqCounter works.
-- It does so by simulating the 'txUnit' for a number of cycles in transparent mode,
-- then simulating it in transmission mode, then again in transparent mode. Transparent
-- and transmission mode refer to passing through the incoming link and transmitting
-- preamble + sequence counter respectively.
txSendSC :: Property
txSendSC = property $ do
  tc <- forAll genTestConfig
  case tc of
    (TestConfig
      bs@(SNat :: SNat bs)
      aw@(SNat :: SNat aw)
      pw@(SNat :: SNat pw)
      fw@(SNat :: SNat fw)
      scw@(SNat :: SNat scw)) -> do
      iterations <- forAll $ Gen.enum 1 10
      preamble <- forAll (genDefinedBitVector @pw)
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
        genFrame = Gen.maybe genDefinedBitVector
        droppedScs = offTime0 + pwNrOfFrames - 1
      framesIn <- forAll $ Gen.list simRange genFrame
      seqCountIn <- forAll $ Gen.list (Range.singleton (simLength + iterationDuration)) $ genUnsigned Range.constantBounded
      let
        topEntity ::
          Signal System
            ( Unsigned scw
            , WishboneM2S aw ((bs * 8) `DivRU` 8) (Bytes bs)
            , Maybe (BitVector fw)
            ) ->
          Signal System (WishboneS2M (Bytes bs), Maybe (BitVector fw))
        topEntity (unbundle -> (scIn, wbIn0, linkIn)) =
          case divWithRemainder @bs @8 @7 of
            Dict -> bundle $ withClockResetEnable
              clockGen resetGen enableGen configTxUnit bs aw pw fw scw preamble scIn linkIn wbIn0
        -- Compensate for the register write + state machine start delays.
        wbOff0 = L.replicate (offTime0-2) emptyWishboneM2S
        wbOn = startSignal : L.replicate onTime emptyWishboneM2S
        wbOff1 = stopSignal : L.repeat emptyWishboneM2S
        wbIn = wbOff0 <> wbOn <> wbOff1
        RegisterBank (toList . fmap Just-> preambleFrames) = getRegsBe @fw preamble
        topEntityInput = L.zip3 seqCountIn wbIn framesIn
        startSignal = (emptyWishboneM2S @aw @(Bytes bs))
          { addr = 0
          , writeEnable = True
          , writeData = 1
          , busSelect = maxBound
          , busCycle = True
          , strobe = True
          }
        stopSignal = (emptyWishboneM2S @aw @(Bytes bs))
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
        onFrames = L.take onTime $ preambleFrames L.++
          L.concatMap ((L.++ preambleFrames) . valToFrames)
          (everyNth iterationDuration $
          L.drop (min (simLength - 1) droppedScs) seqCountIn)
        expectedOutput = L.take simLength $ offFrames0 <> onFrames <> offFrames1
      footnote . fromString $ "iterationsDuration: " <> showX iterationDuration
      footnote . fromString $ "droppedSCs: " <> showX droppedScs
      footnote . fromString $ "simOut: " <> showX simOut
      footnote . fromString $ "expected: " <> showX expectedOutput
      footnote . fromString $ "onFrames: " <> showX onFrames
      footnote . fromString $ "framesIn: " <> showX (L.take simLength framesIn)
      footnote . fromString $ "wbOut: " <> showX wbOut
      footnote . fromString $ "wbIn: " <> showX (L.take simLength wbIn)
      simOut === expectedOutput

-- | Convert any value a to a list of frames.
valToFrames :: forall n a . (KnownNat n, 1 <= n, Paddable a) => a -> [DataLink n]
valToFrames sc = fmap Just out
  where
  RegisterBank (toList -> out) = getRegsBe sc

-- | Take every Nth element from a list by recursively taking the first and dropping (n-1) elements.
everyNth :: HasCallStack => Int -> [a] -> [a]
everyNth 0 _ = error "Can not take every 0th element."
everyNth _ [] = []
everyNth n (x:xs) = x : everyNth n (L.drop (n-1) xs)

-- | Tests whether the detecting of a static preamble decoding a seqCounter works.
-- It does so by generating inputFrames followed by a repeated preamble and sequence
-- counter and sending these to the 'rxUnit'. During transmission the 'rxUnit' is set to
-- capture the sequence counter and at the end of the transmission should have the
-- sequence counter available from the Wishbone bus.
rxSendSC :: Property
rxSendSC = property $ do
  tc <- forAll genTestConfig
  case tc of
    (TestConfig _
      aw@(SNat :: SNat aw)
      pw@(SNat :: SNat pw)
      fw@(SNat :: SNat fw)
      scw@(SNat :: SNat scw)) -> do
      iterations <- forAll $ Gen.enum 1 3
      preamble <- forAll $ replaceBit (natToNum @pw - 1 :: Int) 1 <$> genDefinedBitVector
      let
        -- We hard code the number of bytes to 4 because 'registerWB' assumes a 4 byte aligned address.
        bs = d4
        (bsNum, pwNum, fwNum, scwNum) =
          (snatToNum bs, snatToNum pw, snatToNum fw, snatToNum scw)
        pwNrOfFrames = pwNum `divRU` fwNum
        scInWords = scwNum `divRU` (bsNum * 8)
        scNumOfFrames = scwNum `divRU` fwNum
        iterationDuration = pwNrOfFrames + scNumOfFrames
        iterationsTotal = iterations * iterationDuration
        readBackCycles = 2 * scInWords
      -- First two cycles output depend on the initial state.
      -- The txUnit's state machine starts after the control register has been updated,
      -- causing an extra cycle of delay.
      offTime0 <- forAll $ Gen.enum 2 iterationsTotal
      offTime1 <- forAll $ Gen.enum 2 iterationsTotal
      remoteSeqCounts <- forAll . Gen.list (Range.singleton (1 + iterations)) $
        genUnsigned @_ @scw Range.constantBounded
      let
        onTime = iterationDuration * iterations
        simLength = offTime0 + onTime + offTime1 + readBackCycles
        simRange = Range.singleton simLength
        RegisterBank (fmap Just . toList -> preambleFrames) = getRegsBe preamble
        filterCond inp
          | isJust inp = let (x,y) = (fromJust inp, fromJust $ L.head preambleFrames) in (x .&. y) /= y
          | otherwise = True
        genFrame = Gen.maybe genDefinedBitVector
        onFrames =
          L.take onTime $ L.concatMap ((preambleFrames <>) . valToFrames) remoteSeqCounts
      offFrames0 <- forAll $ Gen.list (Range.singleton offTime0) $ Gen.filter filterCond genFrame
      offFrames1 <- forAll $ Gen.list (Range.singleton offTime1) genFrame
      localSeqCounts <- forAll . Gen.list simRange $ genUnsigned Range.constantBounded
      let
        topEntity (unbundle -> (wbIn0, linkIn, localCounter)) = wcre
          configRxUnit bs aw pw fw scw preamble localCounter linkIn wbIn0
        -- At the end of the simulation, read the sequence counters.
        wbIn = L.take (simLength - readBackCycles) (wbWrite : L.repeat (wbRead 0)) <>
          fmap wbRead [1..]
        framesIn = offFrames0 <> onFrames <> offFrames1 <> L.repeat Nothing
        topEntityInput = L.zip3 wbIn framesIn localSeqCounts
        wbWrite = (emptyWishboneM2S @aw @(Bytes 4))
          { addr = 0 :: BitVector aw
          , writeEnable = True
          , writeData = 1 :: BitVector 32
          , busSelect = maxBound
          , busCycle = True
          , strobe = True
          }
        wbRead (a :: BitVector aw) = (emptyWishboneM2S @aw)
          { addr = shiftL a 2
          , busCycle = True
          , strobe = True
          , busSelect = maxBound
          }
        simOut = simulateN simLength topEntity topEntityInput
        decodedOutput = bimap
          (getDataBe @32 @(Unsigned scw))
          (getDataBe @32 @(Unsigned scw))
          <$> directedDecoding wbIn simOut
        storedOutput = L.last decodedOutput
        expected = (L.head remoteSeqCounts, localSeqCounts L.!! (offTime0 + pwNrOfFrames))
      -- It takes 1 cycle for the the control register to be updated by the wishbone bus.
      footnote . fromString $ "wishbone reads per result: " <> show readBackCycles
      footnote . fromString $ "iterationsDuration: " <> showX iterationDuration
      footnote . fromString $ "simOut: " <> showX simOut
      footnote . fromString $ "decodedOut: " <> showX decodedOutput
      footnote . fromString $ "expected: " <> showX expected
      footnote . fromString $ "onFrames: " <> showX onFrames
      footnote . fromString $ "framesIn: " <> showX (L.take simLength framesIn)
      footnote . fromString $ "wbIn: " <> showX (L.take simLength wbIn)
      storedOutput === expected
 where
  directedDecoding ::
    forall bs aw a .
    (KnownNat bs, 1 <= bs, KnownNat aw, Paddable a) =>
    [WishboneM2S aw bs (Bytes bs)] -> [WishboneS2M (Bytes bs)] -> [a]
  directedDecoding (m2s:m2ss) (s2m:s2ms)
    | slvAck && firstFrame && storedSC && isJust decodingData =
      decoded : uncurry directedDecoding rest
    | otherwise = directedDecoding m2ss s2ms
   where
    slvAck = acknowledge s2m
    firstFrame = addr m2s == 0
    storedSC = readData s2m == resize (pack Done)
    bothLists = L.zip m2ss s2ms
    (fmap snd -> decodingFrames, L.unzip -> rest) =
      span (\(m,s) -> acknowledge s && (/= 0) (addr m)) bothLists
    decodingData = fromList . fmap readData $ decodingFrames
    decoded = getDataLe . RegisterBank $ fromJust decodingData
  directedDecoding _ _ = []

-- | Tests whether we can use 'txUnit' to transmit the local sequence counter over a link with
-- variable latency and capture it with 'rxUnit'. The captured timing oracle will shows us
-- the latency of the link.
integration :: Property
integration = property $ do
  tc <- forAll genTestConfig
  linkLatency <- forAll $ Gen.int (Range.constant 0 64)
  case (TN.someNatVal (fromIntegral linkLatency), tc) of
    (SomeNat llProxy,
      TestConfig _
      aw@(SNat :: SNat aw)
      pw@(SNat :: SNat pw)
      fw@(SNat :: SNat fw)
      scw@(SNat :: SNat scw)) -> do
      iterations <- forAll $ Gen.enum 1 3
      preamble <- forAll $ replaceBit (natToNum @pw - 1:: Integer ) 1 <$> genDefinedBitVector
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
        scInWords = scwNum `divRU` (bsNum * 8)
        readBackCycles = 2 * scInWords

      -- First two cycles output depend on the initial state.
      -- The txUnit's state machine starts after the control register has been updated,
      -- causing an extra cycle of delay.
      delayCycles <- forAll $ Gen.enum 2 (2 + iterationsTotal)
      transmitCycles  <- forAll $ Gen.enum iterationDuration iterationsTotal
      postTransmitCycles <- forAll $ Gen.enum
        -- We need at most 2 read back cycles to reliably read the captured counters.
        -- Each read attempt requires an extra cycle to read the control register.
        (linkLatency + 2 * ( 1 + readBackCycles))
        (linkLatency + 2 * ( 1 + readBackCycles) + iterationsTotal)
      let
        RegisterBank (fmap Just . toList -> preambleFrames) = getRegsBe preamble
        simLength = delayCycles + transmitCycles + postTransmitCycles
        filterCond inp
          | isJust inp = let (x,y) = (fromJust inp, fromJust $ L.head preambleFrames) in (x .&. y) /= y
          | otherwise = True
        genFrame = Gen.maybe genDefinedBitVector
      gatherFrames0 <- forAll . Gen.list (Range.singleton delayCycles) $ Gen.filter filterCond genFrame
      gatherFrames1 <- forAll $ Gen.list (Range.singleton (simLength - delayCycles)) genFrame
      let
        topEntity :: HiddenClockResetEnable System =>
          ( Signal System (Maybe (BitVector fw)
          , Unsigned scw
          , WishboneM2S aw 4 (Bytes 4)
          , WishboneM2S aw 4 (Bytes 4))
          -> Signal System (DataLink fw, WishboneS2M (Bytes 4)))
        topEntity (unbundle -> (gatherOut, counter, wbTxM2S, wbRxM2S)) = out
         where
          (_, linkIn) = configTxUnit bs aw pw fw scw preamble counter gatherOut wbTxM2S
          linkOut = registerN (snatProxy llProxy) Nothing linkIn
          wbRxS2M = configRxUnit bs aw pw fw scw preamble counter linkOut wbRxM2S
          out = bundle (linkIn, wbRxS2M)
        -- Compensate for the register write + state machine start delays.
        wbTxOff = L.replicate (delayCycles - 2) emptyWishboneM2S
        wbTxOn = wbWrite 0 1 : L.replicate transmitCycles emptyWishboneM2S
        wbTxIn = wbTxOff <> wbTxOn <> (wbWrite 0 0 : L.repeat emptyWishboneM2S)
        wbRxRead0 = L.replicate (delayCycles + transmitCycles - 1) (wbRead 0)
        wbRxIn = wbWrite 0 1 : wbRxRead0 <> cycle (fmap wbRead [0..fromIntegral readBackCycles])
        counters = cycle [0..]
        framesIn = gatherFrames0 <> gatherFrames1 <> L.repeat Nothing
        topEntityInput = L.zip4 framesIn counters wbTxIn wbRxIn
        wbWrite a x = (emptyWishboneM2S @aw @(Bytes 4))
          { addr = shiftL a 2
          , writeEnable = True
          , writeData =x
          , busSelect = maxBound
          , busCycle = True
          , strobe = True
          }
        wbRead (a :: BitVector aw) = (emptyWishboneM2S @aw)
          { addr = shiftL a 2
          , busCycle = True
          , strobe = True
          , busSelect = maxBound
          }
        (linkFrames, wbRxOut) = L.unzip $ simulateN simLength topEntity topEntityInput
        decodedOutput = bimap
          (getDataBe @32 @(Unsigned scw))
          (getDataBe @32 @(Unsigned scw))
          <$> directedDecoding wbRxIn wbRxOut
        storedOutput = L.last decodedOutput
        expected =
          ( fromIntegral (delayCycles + pwNrOfFrames - 1)
          , fromIntegral (delayCycles + linkLatency + pwNrOfFrames))
      -- It takes 1 cycle for the the control register to be updated by the wishbone bus.
      footnote . fromString $ "readBackCycles: " <> show readBackCycles
      footnote . fromString $ "iterationsDuration: " <> showX iterationDuration
      footnote . fromString $ "linkFrames: " <> showX linkFrames
      footnote . fromString $ "wbRxOut: " <> showX wbRxOut
      footnote . fromString $ "wbRxIn: " <> showX (L.take simLength wbRxIn)
      footnote . fromString $ "decodedOut: " <> showX decodedOutput
      footnote . fromString $ "expected: " <> showX expected
      footnote . fromString $ "framesIn: " <> showX (L.take simLength framesIn)
      storedOutput === expected
 where
  directedDecoding ::
    forall bs aw a .
    (KnownNat bs, 1 <= bs, KnownNat aw, Paddable a) =>
    [WishboneM2S aw bs (Bytes bs)] -> [WishboneS2M (Bytes bs)] -> [a]
  directedDecoding (m2s:m2ss) (s2m:s2ms)
    | slvAck && firstFrame && storedSC && isJust decodingData =
      decoded : uncurry directedDecoding rest
    | otherwise = directedDecoding m2ss s2ms
   where
    slvAck = acknowledge s2m
    firstFrame = addr m2s == 0
    storedSC = readData s2m == resize (pack Done)
    bothLists = L.zip m2ss s2ms
    (fmap snd -> decodingFrames, L.unzip -> rest) =
      span (\(m,s) -> acknowledge s && (/= 0) (addr m)) bothLists
    decodingData = fromList $ fmap readData decodingFrames
    decoded = getDataLe . RegisterBank $ fromJust decodingData
  directedDecoding _ _ = []

-- | Register with a configurable amount of stages n where (n ~ 0) results in a wire.
registerN ::
  (HiddenClockResetEnable dom, NFDataX o, KnownNat n) =>
  SNat n -> o -> Signal dom o -> Signal dom o
registerN n a = mealy go (replicate n a)
 where
  go state0 inp = (tail z, head z)
   where
    z = state0 :< inp
