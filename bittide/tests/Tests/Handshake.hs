-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}

module Tests.Handshake where

import Clash.Prelude hiding (delayN)

import Bittide.ElasticBuffer (ElasticBufferData (Data))
import Bittide.Handshake
import Hedgehog
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)
import Test.Tasty.Hedgehog (testPropertyNamed)

import qualified Data.List as List
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

{- | Number of bytes in a BittideWord. Meant to be adjustable based on expected
workload size
-}
type BittideWordSize = 64

{- Generate a finite list that starts False, switches to True after a random
number of cycles in range @[minK, maxK]@, and stays True.

The list is finite so that hedgehog does not try to print/shrink an infinite
data structure.
-}
genStickyTrue :: Int -> Int -> Int -> Gen [Bool]
genStickyTrue minK maxK len = do
  k <- Gen.int (Range.linear minK maxK)
  let l = List.replicate k False List.++ List.replicate (len - k) True
  pure l

{- Generate a finite list that starts False, switches to True for once cycle after
a random number of cycles in range @[minK, maxK]@, and then stays False.

The list is finite so that hedgehog does not try to print/shrink an infinite
data structure.
-}
genOneshotTrue :: Int -> Int -> Int -> Gen [Bool]
genOneshotTrue minK maxK len = do
  k <- Gen.int (Range.linear minK maxK)
  let l = List.replicate k False List.++ List.replicate 1 True List.++ List.replicate (len - k - 1) False
  pure l

-- | Output a list that is always true as soon as the input list has a True value.
stickyTrue :: [Bool] -> [Bool]
stickyTrue = List.drop 1 . List.scanl (||) False

{- | Create @n@ registers in a row, each with initial value @a@. Isn't synthesizable so
takes a hedgehog-friendly 'Natural'.
-}
delayN ::
  forall a dom.
  (NFDataX a, HiddenClock dom) =>
  Natural ->
  a ->
  Signal dom a ->
  Signal dom a
delayN n initVal s =
  withEnable enableGen $ List.iterate (delay initVal) s List.!! fromIntegral n

data HandshakeIn dom n = HandshakeIn
  { reset :: Reset dom
  , fromCore :: Signal dom (BitVector n)
  , receiveOk :: Signal dom Bool
  }

data HandshakesIn dom n = HandshakesIn
  { a :: HandshakeIn dom n
  , b :: HandshakeIn dom n
  }

data HandshakeOut dom n = HandshakeOut
  { toCore :: Signal dom (BitVector n)
  , fromCoreStatus :: Signal dom Status
  , toCoreStatus :: Signal dom Status
  }

data HandshakesOut dom n = HandshakesOut
  { a :: HandshakeOut dom n
  , b :: HandshakeOut dom n
  }

{- Connect two handshake state machines together, with a variable number of cycle delay
between the two state machines. The cycle delays need not be symmetrical.
-}
handshakesWithDelays ::
  forall dom n.
  ( HiddenClock dom
  , KnownNat n
  , BitSize (Meta ()) <= n
  ) =>
  -- | Delay from A -> B
  Natural ->
  -- | Delay from B -> A
  Natural ->
  HandshakesIn dom n ->
  HandshakesOut dom n
handshakesWithDelays delayAtoB delayBtoA handshakesIn =
  HandshakesOut
    { a =
        HandshakeOut
          { toCore = wireBtoA
          , fromCoreStatus = transmitStatusA
          , toCoreStatus = receiveStatusA
          }
    , b =
        HandshakeOut
          { toCore = wireAtoB
          , fromCoreStatus = transmitStatusB
          , toCoreStatus = receiveStatusB
          }
    }
 where
  (handshakeWordA, receiveStatusA, transmitStatusA, _metaA) =
    handshake
      @()
      handshakesIn.a.reset
      (Data <$> wireBtoA)
      handshakesIn.a.receiveOk
      (pure ())

  (handshakeWordB, receiveStatusB, transmitStatusB, _metaB) =
    handshake
      @()
      handshakesIn.b.reset
      (Data <$> wireAtoB)
      handshakesIn.b.receiveOk
      (pure ())

  -- XXX: In practice, these words would be sourced from 'fromCore' when the handshake
  --      component is in reset too, because we boot up 'handshakeWb' in 'PassThrough'
  --      mode. That doesn't really make sense for this test though.
  wordFromA = mux (transmitStatusA .== PostHandshake) handshakesIn.a.fromCore handshakeWordA
  wordFromB = mux (transmitStatusB .== PostHandshake) handshakesIn.b.fromCore handshakeWordB

  wireAtoB :: Signal dom (BitVector n)
  wireAtoB = delayN delayAtoB garbageWord wordFromA

  wireBtoA :: Signal dom (BitVector n)
  wireBtoA = delayN delayBtoA garbageWord wordFromB

garbageWord :: (KnownNat n) => BitVector n
garbageWord = 0xDEAD_ABBA

dummyWord :: (KnownNat n) => BitVector n
dummyWord = 0x11

ugnWord :: (KnownNat n) => BitVector n
ugnWord = 0xaa

userDataWord :: (KnownNat n) => BitVector n
userDataWord = 0xff

{- | What data to send (from the core) while in various stages. See 'ugnSender'. The core
is said to be "evil" if it sends words that are parsable as handshake data, while being
outside of the handshake period.
-}
data EvilDuring = EvilDuring
  { negotiation :: Bool
  , last :: Bool
  , postHandshake :: Bool
  }
  deriving (Show)

genEvilDuring :: Gen EvilDuring
genEvilDuring = EvilDuring <$> Gen.bool <*> Gen.bool <*> Gen.bool

genMeta :: Gen (Meta ())
genMeta = Meta <$> Gen.bool <*> Gen.bool <*> pure ()

{- A simple state machine that starts by sending dummy data. When it receives a 'Last', it
sends out 'ugnWord' value next cycle, then continuously sends out 'userDataWord'. In all
other cycles it sends 'dummyWord'.
-}
ugnSender ::
  forall dom n.
  ( HiddenClock dom
  , KnownNat n
  , BitSize (Meta ()) <= n
  ) =>
  {- | Send parsable data during stages (see 'EvilDuring'). The idea is that the handshake
  should be wired up in such a way that data from the core gets ignored while not
  post-handshake. Plus, while post-handshake, the handshakes should be insensitive to any
  incoming, parsable data.
  -}
  EvilDuring ->
  -- | Evil meta to send
  Meta () ->
  -- | @fromCore@ status
  Signal dom Status ->
  -- | @fromCore@
  Signal dom (BitVector n)
ugnSender evilDuring (metaToWord -> evilMeta) status =
  go <$> prevStatus <*> status
 where
  prevStatus = withEnable enableGen $ delay Negotiating status

  go prev now =
    case (prev, now) of
      (_, Negotiating)
        | evilDuring.negotiation -> evilMeta
        | otherwise -> dummyWord
      (_, Last)
        | evilDuring.last -> evilMeta
        | otherwise -> dummyWord
      (Last, _) ->
        ugnWord
      (_, PostHandshake)
        | evilDuring.postHandshake -> evilMeta
        | otherwise -> userDataWord

data DutIn dom = DutIn
  { reset :: Reset dom
  , enable :: Enable dom
  , delay :: Natural
  , evilDuring :: EvilDuring
  , evilMeta :: Meta ()
  }

data DutOut dom n = DutOut
  { toCore :: Signal dom (BitVector n)
  , toCoreStatus :: Signal dom Status
  , fromCore :: Signal dom (BitVector n)
  , fromCoreStatus :: Signal dom Status
  }

dut ::
  (DutIn XilinxSystem, DutIn XilinxSystem) ->
  (DutOut XilinxSystem 64, DutOut XilinxSystem 64)
dut (dutInA, dutInB) =
  withClock @XilinxSystem clockGen
    $ let
        fromCoreA = ugnSender dutInA.evilDuring dutInA.evilMeta handshakesOut.a.fromCoreStatus
        fromCoreB = ugnSender dutInB.evilDuring dutInB.evilMeta handshakesOut.b.fromCoreStatus

        handshakesOut =
          handshakesWithDelays
            dutInA.delay
            dutInB.delay
            HandshakesIn
              { a =
                  HandshakeIn
                    { reset = dutInA.reset
                    , fromCore = fromCoreA
                    , receiveOk = fromEnable dutInA.enable
                    }
              , b =
                  HandshakeIn
                    { reset = dutInB.reset
                    , fromCore = fromCoreB
                    , receiveOk = fromEnable dutInB.enable
                    }
              }

        dutOutA =
          DutOut
            { toCore = handshakesOut.a.toCore
            , toCoreStatus = handshakesOut.a.toCoreStatus
            , fromCore = fromCoreA
            , fromCoreStatus = handshakesOut.a.fromCoreStatus
            }
        dutOutB =
          DutOut
            { toCore = handshakesOut.b.toCore
            , toCoreStatus = handshakesOut.b.toCoreStatus
            , fromCore = fromCoreB
            , fromCoreStatus = handshakesOut.b.fromCoreStatus
            }
       in
        (dutOutA, dutOutB)

{- | Check that the handshake never finishes if at least one handshake is in reset,
disabled, or both.
-}
prop_noHandshake :: Property
prop_noHandshake = property $ do
  nCycles <- forAll $ Gen.int (Range.linear 8 64)
  -- Pick which side stays blocked for the entire simulation.
  blockedIsA <- forAll Gen.bool
  -- Pick how the blocked side is blocked: via reset, via disabled enable, or both.
  blockViaReset <- forAll Gen.bool
  blockViaDisable <- forAll $ if blockViaReset then Gen.bool else pure True

  -- Free side: reset deasserts (sticky-true on "not in reset"), enable asserts
  -- (sticky-true on "enabled") at some point. Allow them to never transition by
  -- letting @minK@ go all the way up to @nCycles@.
  freeReset <- forAll $ genStickyTrue 0 nCycles nCycles
  freeEnable <- forAll $ genStickyTrue 0 nCycles nCycles

  delayA <- forAll $ Gen.integral (Range.linear 1 8)
  delayB <- forAll $ Gen.integral (Range.linear 1 8)
  evilDuringA <- forAll genEvilDuring
  evilDuringB <- forAll genEvilDuring
  evilMetaA <- forAll genMeta
  evilMetaB <- forAll genMeta

  let
    blockedReset = if blockViaReset then List.repeat True else List.repeat False
    blockedEnable = if blockViaDisable then List.repeat False else List.repeat True

    -- Lists of (active-high reset, enable) per cycle, padded to infinity.
    blockedResetList = blockedReset
    blockedEnableList = blockedEnable
    -- Free side: starts in reset, then deasserts at some point (or never).
    freeResetList = List.map not freeReset List.++ List.repeat False
    freeEnableList = freeEnable List.++ List.repeat True

    mkDutIn resetList enableList d evilDuring evilMeta =
      withClock @XilinxSystem clockGen
        $ DutIn
          { reset = unsafeFromActiveHigh (fromList resetList)
          , enable = toEnable (fromList enableList)
          , delay = d
          , evilDuring
          , evilMeta
          }

    (sideA, sideB)
      | blockedIsA =
          ( mkDutIn blockedResetList blockedEnableList delayA evilDuringA evilMetaA
          , mkDutIn freeResetList freeEnableList delayB evilDuringB evilMetaB
          )
      | otherwise =
          ( mkDutIn freeResetList freeEnableList delayA evilDuringA evilMetaA
          , mkDutIn blockedResetList blockedEnableList delayB evilDuringB evilMetaB
          )

    (outA, outB) = dut (sideA, sideB)

    aToCoreStatus = sampleN nCycles outA.toCoreStatus
    bToCoreStatus = sampleN nCycles outB.toCoreStatus
    aFromCoreStatus = sampleN nCycles outA.fromCoreStatus
    bFromCoreStatus = sampleN nCycles outB.fromCoreStatus

  footnote ("aToCoreStatus: " <> show aToCoreStatus)
  footnote ("bToCoreStatus: " <> show bToCoreStatus)
  footnote ("aFromCoreStatus: " <> show aFromCoreStatus)
  footnote ("bFromCoreStatus: " <> show bFromCoreStatus)

  assert (List.all (== Negotiating) aToCoreStatus)
  assert (List.all (== Negotiating) bToCoreStatus)
  assert (List.all (== Negotiating) aFromCoreStatus)
  assert (List.all (== Negotiating) bFromCoreStatus)

{- | Status sequence must match @Negotiating+ Last PostHandshake+@ exactly: one or
more 'Negotiating', then exactly one 'Last', then one or more 'PostHandshake'.
-}
isValidStatusSequence :: [Status] -> Bool
isValidStatusSequence = goNeg
 where
  goNeg (Negotiating : xs) = goNeg xs
  goNeg (Last : xs) = goPost xs
  goNeg _ = False
  goPost [] = True
  goPost (PostHandshake : xs) = goPost xs
  goPost _ = False

{- | For the cycle where status first becomes 'PostHandshake', expect 'ugnWord' on
@toCore@. Subsequent post-handshake cycles should carry 'userDataWord'.
-}
checkPostHandshakeStream ::
  (MonadTest m) => [Status] -> [BitVector 64] -> m ()
checkPostHandshakeStream statuses words_ =
  case List.dropWhile ((/= PostHandshake) . fst) (List.zip statuses words_) of
    [] -> failure
    ((_, w0) : rest) -> do
      w0 === ugnWord
      List.map snd rest === List.replicate (List.length rest) userDataWord

{- | Check that the handshake completes when both sides are eventually unblocked. After
the unblocking, both sides should reach 'PostHandshake' on all four status signals, the
status sequences should be monotonic with exactly one 'Last' cycle, and the first
post-handshake word delivered on @toCore@ should be 'ugnWord' followed by 'userDataWord'
indefinitely.
-}
prop_handshake :: Property
prop_handshake = property $ do
  let nCycles = 96
      transitionWindow = 8

  resetA <- forAll $ genStickyTrue 0 transitionWindow transitionWindow
  enableA <- forAll $ genStickyTrue 0 transitionWindow transitionWindow
  resetB <- forAll $ genStickyTrue 0 transitionWindow transitionWindow
  enableB <- forAll $ genStickyTrue 0 transitionWindow transitionWindow

  delayA <- forAll $ Gen.integral (Range.linear 1 8)
  delayB <- forAll $ Gen.integral (Range.linear 1 8)

  -- Allow evil during negotiation/Last, but not post-handshake: after the
  -- handshake completes, ugnSender should produce userDataWord deterministically.
  evilDuringA <- forAll $ EvilDuring <$> Gen.bool <*> Gen.bool <*> pure False
  evilDuringB <- forAll $ EvilDuring <$> Gen.bool <*> Gen.bool <*> pure False
  evilMetaA <- forAll genMeta
  evilMetaB <- forAll genMeta

  let
    mkResetList xs = xs <> List.repeat True -- active low
    mkEnableList xs = xs <> List.repeat True

    mkDutIn r e d evilDuring evilMeta =
      withClock @XilinxSystem clockGen
        $ DutIn
          { reset = unsafeFromActiveLow (fromList (mkResetList r))
          , enable = toEnable (fromList (mkEnableList e))
          , delay = d
          , evilDuring
          , evilMeta
          }

    sideA = mkDutIn resetA enableA delayA evilDuringA evilMetaA
    sideB = mkDutIn resetB enableB delayB evilDuringB evilMetaB

    (outA, outB) = dut (sideA, sideB)

    aToCoreStatus = sampleN nCycles outA.toCoreStatus
    aFromCoreStatus = sampleN nCycles outA.fromCoreStatus
    bToCoreStatus = sampleN nCycles outB.toCoreStatus
    bFromCoreStatus = sampleN nCycles outB.fromCoreStatus
    aToCore = sampleN nCycles outA.toCore
    bToCore = sampleN nCycles outB.toCore

  footnote ("aToCoreStatus: " <> show aToCoreStatus)
  footnote ("aFromCoreStatus: " <> show aFromCoreStatus)
  footnote ("bToCoreStatus: " <> show bToCoreStatus)
  footnote ("bFromCoreStatus: " <> show bFromCoreStatus)
  footnote ("aToCore: " <> show aToCore)
  footnote ("bToCore: " <> show bToCore)

  -- Eventual completion + monotonic status with exactly one Last cycle.
  assert (isValidStatusSequence aToCoreStatus)
  assert (isValidStatusSequence aFromCoreStatus)
  assert (isValidStatusSequence bToCoreStatus)
  assert (isValidStatusSequence bFromCoreStatus)

  -- First PostHandshake word on toCore is ugnWord, rest are userDataWord.
  checkPostHandshakeStream aToCoreStatus aToCore
  checkPostHandshakeStream bToCoreStatus bToCore

testMetadataParsing :: Assertion
testMetadataParsing = do
  let regularWord = 0 :: BitVector 64
  let metadataWord = magicConstant @(BittideWordSize - 3) ++# (0b111 :: BitVector 3)
  let metadata = Meta True True True :: Meta Bool

  assertEqual "Parsing regular word returns Nothing" Nothing (wordToMeta @() regularWord)
  assertEqual "Parsing metadata word returns Just xxx" (Just metadata) (wordToMeta metadataWord)
  assertEqual
    "id ~ fromWord . toWord"
    (Just metadata)
    (wordToMeta @_ @BittideWordSize $ metaToWord metadata)

tests :: TestTree
tests =
  testGroup
    "Bittide.Handshake"
    [ testCase "testMetadataParsing" testMetadataParsing
    , testPropertyNamed "prop_noHandshake" "prop_noHandshake" prop_noHandshake
    , testPropertyNamed "prop_handshake" "prop_handshake" prop_handshake
    ]
