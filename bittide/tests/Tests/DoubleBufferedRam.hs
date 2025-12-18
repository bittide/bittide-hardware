-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- Don't warn about partial functions: this is a test, so we'll see it fail.
{-# OPTIONS_GHC -Wno-x-partial #-}

module Tests.DoubleBufferedRam (tests) where

import Clash.Prelude

import Clash.Hedgehog.Sized.Index
import Clash.Hedgehog.Sized.Unsigned
import Clash.Hedgehog.Sized.Vector
import Data.Maybe
import Data.Proxy
import Data.String
import Data.Type.Equality (type (:~:) (Refl))
import Hedgehog
import Numeric (showHex)
import Protocols.Hedgehog.Internal
import Protocols.MemoryMap (unMemmap)
import Protocols.Wishbone
import Protocols.Wishbone.Standard.Hedgehog (
  WishboneMasterRequest (Read, Write),
  driveStandard,
  wishbonePropWithModel,
 )
import Test.Tasty
import Test.Tasty.Hedgehog

import Bittide.DoubleBufferedRam
import Bittide.SharedTypes
import Tests.Shared

import qualified Clash.Sized.Vector as V
import qualified Data.IntMap as I
import qualified Data.List as L
import qualified GHC.TypeNats as TN
import qualified Hedgehog.Gen as Gen hiding (resize)
import qualified Hedgehog.Range as Range
import qualified Protocols.Wishbone.Standard.Hedgehog as WB
import qualified Prelude as P

tests :: TestTree
tests =
  testGroup
    "Tests.DoubleBufferedRam"
    [ testPropertyNamed
        "Byte addressable blockRam matches behavioral model."
        "readWriteByteAddressableBlockram"
        readWriteByteAddressableBlockram
    , testPropertyNamed
        "Byte addressable blockRam with always high byteEnables behaves like blockRam"
        "byteAddressableBlockRamAsBlockRam"
        byteAddressableBlockRamAsBlockRam
    , testPropertyNamed
        "Byte addressable register can be written to and read from with byte enables."
        "readWriteRegisterByteAddressable"
        readWriteRegisterByteAddressable
    , testPropertyNamed
        "registerWb function as a normal register."
        "registerWbSigToSig"
        registerWbSigToSig
    , testPropertyNamed
        "registerWb can be written to with wishbone."
        "registerWbWbToSig"
        registerWbWbToSig
    , testPropertyNamed
        "registerWb can be read from with wishbone."
        "registerWbSigToWb"
        registerWbSigToWb
    , testPropertyNamed
        "registerWb write conflict resolution matches set priorities"
        "registerWbWriteCollisions"
        registerWbWriteCollisions
    , testPropertyNamed
        "Test wishboneStorage spec compliance"
        "wbStorageSpecCompliance"
        wbStorageSpecCompliance
    , testPropertyNamed
        "Test whether wbStorage acts the same its Behavioral model"
        "wbStorageBehavior"
        wbStorageBehavior
    , testPropertyNamed
        "Test whether wbStorage reports errors at out-of-bounds accesses"
        "wbStorageRangeErrors"
        wbStorageRangeErrors
    , testPropertyNamed
        "Test whether wbStorage acts the same its Behavioral model (clash-protocols)"
        "wbStorageProtocolsModel"
        wbStorageProtocolsModel
    ]

data BitvecVec where
  BitvecVec ::
    (1 <= bits, 1 <= memDepth, 1 <= Regs (BitVector bits) 8) =>
    SNat memDepth ->
    SNat bits ->
    Vec memDepth (BitVector bits) ->
    BitvecVec

instance Show BitvecVec where
  show (BitvecVec SNat SNat v) = show v

genBlockRamContents :: Int -> Int -> Gen BitvecVec
genBlockRamContents memDepth bits = do
  case ( TN.someNatVal $ fromIntegral (memDepth - 1)
       , TN.someNatVal $ fromIntegral $ bits - 1
       ) of
    (SomeNat depth0, SomeNat bits0) -> go (snatProxy depth0) (snatProxy bits0)
 where
  go :: forall memDepth bits. SNat memDepth -> SNat bits -> Gen BitvecVec
  go depth0@SNat bits0@SNat =
    case compareSNat d1 (SNat @(Regs (BitVector (bits + 1)) 8)) of
      SNatLE ->
        BitvecVec (succSNat depth0) (succSNat bits0)
          <$> genNonEmptyVec genDefinedBitVector
      _ -> error "genBlockRamContents: Generated BitVector is of size 0."

{- | This test checks if we can write new values to the byte addressable 'blockRam'
('blockRamByteAddressable') and read them. It uses 'byteAddressableRamBehavior' as-
reference model.
-}
readWriteByteAddressableBlockram :: Property
readWriteByteAddressableBlockram = property $ do
  ramDepth <- forAll $ Gen.enum 1 31
  nrOfBits <- forAll $ Gen.enum 1 31
  simLength <- forAll $ bitCoerce @_ @Int <$> genUnsigned (Range.constant 2 100)
  ramContents <- forAll $ genBlockRamContents ramDepth nrOfBits
  case ramContents of
    BitvecVec SNat SNat contents -> do
      let
        simRange = Range.singleton simLength
        topEntity (unbundle -> (readAddr, writePort, byteSelect)) =
          withClockResetEnable
            clockGen
            resetGen
            enableGen
            blockRamByteAddressable
            (Vec contents)
            readAddr
            writePort
            byteSelect
      writeAddresses <- forAll $ Gen.list simRange $ genIndex Range.constantBounded
      readAddresses <- forAll $ Gen.list simRange $ genIndex Range.constantBounded
      writeEntries <- forAll (Gen.list simRange $ Gen.maybe genDefinedBitVector)
      byteSelectSignal <- forAll $ Gen.list simRange genDefinedBitVector
      let
        topEntityInput =
          L.zip3
            readAddresses
            (P.zipWith (\adr wr -> (adr,) <$> wr) writeAddresses writeEntries)
            byteSelectSignal

        simOut = simulateN @System simLength topEntity topEntityInput
        (_, expectedOut) =
          L.mapAccumL
            byteAddressableRamBehavior
            (L.head topEntityInput, contents)
            $ L.drop 1 topEntityInput
      -- TODO: Due to some unexpected mismatch between the expected behavior of either
      -- blockRam or the behavioral model, the boot behavior is inconsistent. We drop the first
      -- expectedOutput cycle too, we expect this is due to the resets supplied b simulateN.
      -- An issue has been made regarding this.
      L.drop 2 simOut === L.drop 1 expectedOut

{- | This test checks if 'blockRamByteAddressable' behaves the same as 'blockRam' when the
byteEnables are always high.
-}
byteAddressableBlockRamAsBlockRam :: Property
byteAddressableBlockRamAsBlockRam = property $ do
  ramDepth <- forAll $ Gen.enum 1 31
  nrOfBits <- forAll $ Gen.enum 1 31
  simLength <- forAll $ Gen.int $ Range.constant 2 100
  ramContents <- forAll $ genBlockRamContents ramDepth nrOfBits
  case ramContents of
    BitvecVec SNat SNat contents -> do
      let
        simRange = Range.singleton simLength
        -- topEntity returns a tuple with the outputs of (byteAddressableRam,blockRam)
        topEntity (unbundle -> (readAddr, writePort)) =
          withClockResetEnable clockGen resetGen enableGen
            $ bundle
              ( blockRamByteAddressable (Vec contents) readAddr writePort (pure maxBound)
              , blockRam contents readAddr writePort
              )
      writeAddresses <- forAll $ Gen.list simRange $ genIndex Range.constantBounded
      readAddresses <- forAll $ Gen.list simRange $ genIndex Range.constantBounded
      writeEntries <- forAll (Gen.list simRange $ Gen.maybe genDefinedBitVector)
      let
        topEntityInput =
          L.zip
            readAddresses
            (P.zipWith (\adr wr -> (adr,) <$> wr) writeAddresses writeEntries)
        simOut = simulateN @System simLength topEntity topEntityInput
        (fstOut, sndOut) = L.unzip simOut
      footnote . fromString $ "simOut: " <> showX simOut
      fstOut === sndOut

{- | This test checks that we can generate a 'registerByteAddressable' that stores a
configurable amount of bytes and selectively update its contents on a per byte basis.
-}
readWriteRegisterByteAddressable :: Property
readWriteRegisterByteAddressable = property $ do
  nBytes <- forAll $ Gen.enum 1 10
  case TN.someNatVal nBytes of
    SomeNat p -> case compareSNat d1 (snatProxy p) of
      SNatLE -> go p
      _ -> error "readWriteRegisterByteAddressable: Amount of nBytes == 0."
 where
  go ::
    forall nBytes m.
    ( KnownNat nBytes
    , 1 <= nBytes
    , KnownNat (nBytes * 8)
    , 1 <= (nBytes * 8)
    , Monad m
    ) =>
    Proxy nBytes ->
    PropertyT m ()
  go Proxy =
    case sameNat (Proxy @nBytes) (Proxy @(Regs (Vec nBytes Byte) 8)) of
      Just Refl -> do
        simLength <- forAll $ Gen.enum 1 100
        let
          writeGen = genNonEmptyVec @nBytes $ genDefinedBitVector @8
        initVal <- forAll writeGen
        writes <- forAll $ Gen.list (Range.singleton simLength) writeGen
        byteEnables <-
          forAll
            $ Gen.list (Range.singleton simLength)
            $ genDefinedBitVector @(Regs (Vec nBytes Byte) 8)
        let
          topEntity (unbundle -> (newVal, byteEnable)) =
            withClockResetEnable @System clockGen resetGen enableGen
              $ registerByteAddressable initVal newVal byteEnable
          expectedOut = P.scanl useByteEnable initVal $ P.zip writes byteEnables
          useByteEnable olds (news, unpack -> enables) =
            (\(enable, old, new) -> if enable then new else old) <$> zip3 enables olds news
          simOut = simulateN simLength topEntity $ P.zip writes byteEnables
        simOut === P.take simLength expectedOut
      _ ->
        error "readWriteRegisterByteAddressable: Amount of bytes not equal to registers required."

{- | This test checks that 'registerWb' can be written to and read from via its wishbone bus.
This test makes sure that writing and reading with the wishbone bus works both with
'CircuitPriority' and 'WishbonePriority' enabled. During this test the circuit input does
not write to the register.
-}
registerWbSigToSig :: Property
registerWbSigToSig = property $ do
  bits <- forAll $ Gen.enum 1 100
  case TN.someNatVal bits of
    SomeNat p -> case compareSNat d1 (snatProxy p) of
      SNatLE -> go p
      _ -> error "registerWbSigToSig: Amount of bits == 0."
 where
  go :: forall bits m. (KnownNat bits, 1 <= bits, Monad m) => Proxy bits -> PropertyT m ()
  go Proxy = case compareSNat d1 $ SNat @(Regs (BitVector bits) 32) of
    SNatLE -> do
      initVal <- forAll $ genDefinedBitVector @bits
      writes <- forAll $ Gen.list (Range.constant 1 25) $ genDefinedBitVector @bits
      let
        simLength = L.length writes + 1
        someReg prio sigIn =
          fst
            $ withClockResetEnable clockGen resetGen enableGen
            $ registerWbSpecVal @_ @_ @4 @32 prio initVal (pure emptyWishboneM2S) sigIn
        topEntity sigIn = bundle (someReg CircuitPriority sigIn, someReg WishbonePriority sigIn)
        topEntityInput = (Just <$> writes) <> [Nothing]
        simOut = simulateN @System simLength topEntity topEntityInput
        (fstOut, sndOut) = L.unzip simOut
      footnote . fromString $ "simOut: " <> showX simOut
      footnote . fromString $ "input:" <> showX topEntityInput
      footnote . fromString $ "expected" <> showX writes
      fstOut === sndOut
      writes === L.drop 1 fstOut
    _ -> error "registerWbSigToSig: Registers required to store bitvector == 0."

{- | This test checks that 'registerWb' can be written to with the wishbone bus and read from
with the circuit output. This test makes sure that the behavior with 'CircuitPriority'
and 'WishbonePriority' is identical. During this test the circuit input does not write
to the register.
-}
registerWbWbToSig :: Property
registerWbWbToSig = property $ do
  bits <- forAll $ Gen.enum 1 100
  case TN.someNatVal bits of
    SomeNat p -> case compareSNat d1 (snatProxy p) of
      SNatLE -> go p
      _ -> error "registerWbWbToSig: Amount of bits == 0."
 where
  go :: forall bits m. (KnownNat bits, 1 <= bits, Monad m) => Proxy bits -> PropertyT m ()
  go Proxy = case compareSNat d1 $ SNat @(Regs (BitVector bits) 32) of
    SNatLE -> do
      let regs = (natToNum @(DivRU bits 32))
      initVal <- forAll $ genDefinedBitVector @bits
      writes <- forAll $ Gen.list (Range.constant 1 25) $ genDefinedBitVector @bits
      let
        simLength = L.length writes * regs + 2
        someReg prio wbIn =
          fst
            $ withClockResetEnable clockGen resetGen enableGen
            $ registerWbSpecVal @System @_ @4 @30 prio initVal wbIn (pure Nothing)
        topEntity wbIn = bundle (someReg CircuitPriority wbIn, someReg WishbonePriority wbIn)
        topEntityInput = L.concatMap wbWrite writes <> L.repeat emptyWishboneM2S
        simOut = simulateN simLength topEntity topEntityInput
        (fstOut, sndOut) = L.unzip simOut
        filteredOut = everyNth regs $ L.drop 1 fstOut

      footnote . fromString $ "simOut: " <> showX simOut
      footnote . fromString $ "filteredOut:" <> showX filteredOut
      footnote . fromString $ "input:" <> showX (L.take simLength topEntityInput)
      footnote . fromString $ "expected" <> showX writes
      fstOut === sndOut
      writes === L.take (L.length writes) filteredOut
    _ -> error "registerWbWbToSig: Registers required to store bitvector == 0."
   where
    wbWrite v = L.zipWith bv2WbWrite [0 .. L.length l - 1] l
     where
      RegisterBank (toList -> l) = getRegsLe v
  everyNth n l
    | L.length l >= n = L.head xs : everyNth n (L.drop 1 xs)
    | otherwise = []
   where
    xs = L.drop (n - 1) l

{- | This test checks that 'registerWb' can be written to by the circuit and read from
with the wishbone bus. This test makes sure that the behavior with 'CircuitPriority'
and 'WishbonePriority' is identical. During this test the wishbone bus does not write
to the register.
-}
registerWbSigToWb :: Property
registerWbSigToWb = property $ do
  bits <- forAll $ Gen.enum 1 100
  case TN.someNatVal bits of
    SomeNat p -> case compareSNat d1 (snatProxy p) of
      SNatLE -> go p
      _ -> error "registerWbSigToWb: Amount of bits == 0."
 where
  go :: forall bits m. (KnownNat bits, 1 <= bits, Monad m) => Proxy bits -> PropertyT m ()
  go Proxy = case compareSNat d1 $ SNat @(Regs (BitVector bits) 32) of
    SNatLE -> do
      initVal <- forAll $ genDefinedBitVector @bits
      writes <- forAll $ Gen.list (Range.constant 1 25) $ genDefinedBitVector @bits
      let
        someReg prio sigIn wbIn =
          snd
            $ withClockResetEnable clockGen resetGen enableGen
            $ registerWbSpecVal @_ @_ @4 @32 prio initVal wbIn sigIn
        topEntity (unbundle -> (sigIn, wbIn)) =
          bundle
            (someReg CircuitPriority sigIn wbIn, someReg WishbonePriority sigIn wbIn)
        padWrites x = L.take (natToNum @(Regs (BitVector bits) 32)) $ Just x : L.repeat Nothing
        readOps =
          emptyWishboneM2S
            : cycle
              (wbRead <$> [(0 :: Int) .. (natToNum @(Regs (BitVector bits) 32) - 1)])
        topEntityInput = L.zip (L.concatMap padWrites writes <> [Nothing]) readOps
        simLength = L.length topEntityInput
        simOut = simulateN @System simLength topEntity topEntityInput
        (fstOut, sndOut) = L.unzip simOut
      footnote . fromString $ "simOut: " <> showX simOut
      footnote . fromString $ "input:" <> showX topEntityInput
      footnote . fromString $ "expected" <> showX writes
      postProcWb fstOut === postProcWb sndOut
      writes === wbDecoding (L.drop 1 fstOut)
    _ -> error "registerWbSigToWb: Registers required to store bitvector == 0."
   where
    wbDecoding :: ([WishboneS2M 4] -> [BitVector bits])
    wbDecoding (wbNow : wbRest)
      | acknowledge wbNow = entry : wbDecoding rest
      | otherwise = wbDecoding wbRest
     where
      (fmap readData -> entryList, rest) =
        L.splitAt (natToNum @(Regs (BitVector bits) 32)) (wbNow : wbRest)
      entry = case V.fromList entryList of
        Just (vec :: Vec (Regs (BitVector bits) 32) (BitVector 32)) ->
          getDataLe @32 (RegisterBank vec)
        Nothing ->
          error
            $ "wbDecoding: list to vector conversion failed: "
            <> show entryList
            <> "from "
            <> show (wbNow : wbRest)
    wbDecoding [] = []
    wbRead i =
      (emptyWishboneM2S @32)
        { addr = resize (pack i)
        , busCycle = True
        , busSelect = maxBound
        , strobe = True
        }
    postProcWb (WishboneS2M{..} : wbRest)
      | acknowledge = Just readData : postProcWb wbRest
      | err = Nothing : postProcWb wbRest
      | otherwise = postProcWb wbRest
    postProcWb _ = []

{- | This test checks that the behavior of 'registerWb' matches the set priorities when
a write conflict occurs. It is expected that with 'WishbonePriority', the circuit
ignores write operations from the circuit during a wishbone write operation.
With 'CircuitPriority', wishbone write operations are acknowledged, but silently
ignored during a circuit write cycle.
-}
registerWbWriteCollisions :: Property
registerWbWriteCollisions = property $ do
  bits <- forAll $ Gen.enum 1 32
  case TN.someNatVal bits of
    SomeNat p -> case compareSNat d1 (snatProxy p) of
      SNatLE -> go p
      _ -> error "registerWbWriteCollisions: Amount of bits == 0."
 where
  go :: forall bits m. (KnownNat bits, 1 <= bits, Monad m) => Proxy bits -> PropertyT m ()
  go Proxy = case compareSNat d1 $ SNat @(Regs (BitVector bits) 32) of
    SNatLE -> do
      initVal <- forAll $ genDefinedBitVector @bits
      writeAmount <- forAll $ Gen.enum 1 25
      sigWrites <- forAll $ Gen.list (Range.singleton writeAmount) $ genDefinedBitVector @bits
      wbWrites <- forAll $ Gen.list (Range.singleton writeAmount) $ genDefinedBitVector @bits
      let
        simLength = writeAmount + 1
        someReg prio sigIn wbIn =
          fst
            $ withClockResetEnable clockGen resetGen enableGen
            $ registerWbSpecVal @System @_ @4 @30 prio initVal wbIn sigIn
        topEntity (unbundle -> (sigIn, wbIn)) =
          bundle
            (someReg CircuitPriority sigIn wbIn, someReg WishbonePriority sigIn wbIn)
        topEntityInput =
          L.zip
            (Just <$> sigWrites)
            (L.concatMap wbWrite wbWrites <> L.repeat emptyWishboneM2S)
        simOut = simulateN simLength topEntity topEntityInput
        (fstOut, sndOut) = L.unzip simOut

      footnote . fromString $ "WishbonePrio out: " <> showX sndOut
      footnote . fromString $ "CircuitPrio out: " <> showX fstOut
      footnote . fromString $ "input:" <> showX (L.take simLength topEntityInput)
      footnote . fromString $ "wbIn" <> showX wbWrites
      footnote . fromString $ "sigIn" <> showX sigWrites
      sigWrites === L.drop 1 fstOut
      wbWrites === L.drop 1 sndOut
    _ -> error "registerWbWriteCollisions: Registers required to store bitvector == 0."
   where
    wbWrite v = L.zipWith bv2WbWrite (L.reverse [0 .. L.length l - 1]) l
     where
      RegisterBank (toList -> l) = getRegsLe v

bv2WbWrite ::
  (BitPack a, Enum a) =>
  a ->
  ("DAT_MOSI" ::: BitVector 32) ->
  WishboneM2S 30 4
bv2WbWrite i v =
  (emptyWishboneM2S @30 @4)
    { addr = resize (pack i)
    , writeData = v
    , writeEnable = True
    , busCycle = True
    , strobe = True
    , busSelect = maxBound
    }

{- | Model for 'byteAddressableRam', it stores the inputs in its state for a one cycle delay
and updates the Ram based on the the write operation and byte enables.
Furthermore it contains read-before-write behavior based on the readAddr.
-}
byteAddressableRamBehavior ::
  forall bits memDepth nBytes.
  ( KnownNat memDepth
  , 1 <= memDepth
  , KnownNat nBytes
  , 1 <= nBytes
  , nBytes ~ Regs (BitVector bits) 8
  , KnownNat bits
  , 1 <= bits
  ) =>
  ( (Index memDepth, Maybe (LocatedBits memDepth bits), BitVector nBytes)
  , Vec memDepth (BitVector bits)
  ) ->
  (Index memDepth, Maybe (LocatedBits memDepth bits), BitVector nBytes) ->
  ( ( ( Index memDepth
      , Maybe (LocatedBits memDepth bits)
      , BitVector nBytes
      )
    , Vec memDepth (BitVector bits)
    )
  , BitVector bits
  )
byteAddressableRamBehavior state input = (state', ram !! readAddr)
 where
  ((readAddr, writeOp, byteEnable), ram) = state
  (writeAddr, writeData0) = fromMaybe (0, 0) writeOp
  writeTrue = isJust writeOp
  RegisterBank oldData = getRegsBe $ ram !! writeAddr
  RegisterBank newData = getRegsBe writeData0
  newEntry =
    getDataBe @8
      . RegisterBank
      $ zipWith
        (\sel (old, new) -> if sel then new else old)
        (unpack byteEnable)
        (zip oldData newData)

  ram1 = if writeTrue then replace writeAddr newEntry ram else ram
  state' = (input, ram1)

{- | Version of 'Bittide.DoubleBufferedRam.registerWb' which performs wishbone
  spec validation.
-}
registerWbSpecVal ::
  forall dom a nBytes addrW.
  ( HiddenClockResetEnable dom
  , Paddable a
  , KnownNat nBytes
  , 1 <= nBytes
  , KnownNat addrW
  , BitPack a
  , ShowX a
  ) =>
  -- | Determines the write priority on write collisions
  RegisterWritePriority ->
  -- | Initial value.
  a ->
  -- | Wishbone bus (master to slave)
  Signal dom (WishboneM2S addrW nBytes) ->
  -- | New circuit value.
  Signal dom (Maybe a) ->
  -- |
  --   1. Outgoing stored value
  --   2. Outgoing wishbone bus (slave to master)
  (Signal dom a, Signal dom (WishboneS2M nBytes))
registerWbSpecVal writePriority initVal m2s0 sigIn = (storedVal, s2m1)
 where
  (storedVal, s2m0) = registerWb @dom @a @nBytes @addrW writePriority initVal m2s1 sigIn
  (m2s1, s2m1) = validateWb m2s0 s2m0

wbStorageSpecCompliance :: Property
wbStorageSpecCompliance = property $ do
  nat <- forAll $ Gen.enum 1 32
  case TN.someNatVal (nat - 1) of
    SomeNat (succSNat . snatProxy -> n) -> go n
 where
  go :: forall v m. (KnownNat v, 1 <= v, Monad m) => SNat v -> PropertyT m ()
  go depth@SNat = do
    content <- forAll $ genNonEmptyVec @v (genDefinedBitVector @32)
    wcre
      $ wishbonePropWithModel @System
        defExpectOptions
        (\_ _ () -> Right ())
        (unMemmap $ wbStorage "" depth (Just (Vec content)))
        (genRequests (snatToNum (SNat @v) - 1))
        ()

  genRequests :: Unsigned 30 -> Gen [WishboneMasterRequest 30 4]
  genRequests size =
    Gen.list (Range.linear 0 32) $ do
      upperBound <- Gen.choice $ fmap pure [size, maxBound]
      WB.genWishboneTransfer (Range.constant 0 upperBound)

deriving instance (ShowX a) => ShowX (RamOp i a)

{- | Behavioral test for 'wbStorage', it checks whether the behavior of 'wbStorage' matches
the 'wbStorageBehaviorModel'.
-}
wbStorageBehavior :: Property
wbStorageBehavior = property $ do
  nWords <- forAll $ Gen.enum 2 32
  case TN.someNatVal (nWords - 2) of
    SomeNat (addSNat d2 . snatProxy -> nWords0) -> go nWords0
 where
  go ::
    forall words m. (KnownNat words, 2 <= words, Monad m) => SNat words -> PropertyT m ()
  go depth@SNat = do
    content <- forAll $ genVec @words genDefinedBitVector
    wbRequests <-
      forAll
        $ Gen.list
          (Range.linear 0 32)
        $ do
          valid <- Gen.bool
          let upperBound = if valid then (natToNum @words - 1) else maxBound :: Unsigned 30
          req <- WB.genWishboneTransfer (Range.constant 0 upperBound)
          stall <- Gen.int $ Range.constant 0 10
          pure (valid, (req, stall))

    let
      master = driveStandard defExpectOptions $ fmap snd wbRequests
      slave = wcre $ unMemmap (wbStorage @System @_ @30 "" depth (Just (Vec content)))
      simTransactions = exposeWbTransactions (Just 1000) master slave
      goldenTransactions = wbStorageBehaviorModel (toList content) $ fmap (fmap fst) wbRequests

    footnote $ "goldenTransactions" <> show goldenTransactions
    footnote $ "simTransactions" <> show simTransactions
    footnote $ "wbRequests" <> show wbRequests

    simTransactions === goldenTransactions

-- | Behavioral model for 'wbStorage'.
wbStorageBehaviorModel ::
  forall addrW bytes.
  (1 <= addrW, KnownNat bytes) =>
  (KnownNat addrW) =>
  [Bytes bytes] ->
  [(Bool, WishboneMasterRequest addrW bytes)] ->
  [Transaction addrW bytes]
wbStorageBehaviorModel initList initWbOps = snd $ L.mapAccumL f initList initWbOps
 where
  -- Invalid request
  f storedList (False, op) = (storedList, Error (wbMasterRequestToM2S op))
  -- Successful Read
  f storedList (True, op@(Read i _)) = (storedList, ReadSuccess wbM2S wbS2M)
   where
    dat = storedList L.!! (fromIntegral i)
    wbM2S = wbMasterRequestToM2S op
    wbS2M = (emptyWishboneS2M @4){acknowledge = True, readData = dat}

  -- Successful Write
  f storedList (True, op@(Write i bs a)) = (newList, WriteSuccess wbM2S wbS2M)
   where
    wbM2S = wbMasterRequestToM2S op
    wbS2M = emptyWishboneS2M{acknowledge = True}
    (preEntry, postEntry) = L.splitAt (fromIntegral i) storedList
    oldEntry = L.head postEntry
    newList = preEntry <> (pack newEntry : L.drop 1 postEntry)

    newEntry :: Vec bytes Byte
    newEntry =
      zipWith3
        (\b old new -> if b then new else old)
        (unpack bs)
        (unpack oldEntry)
        (unpack a)

wbStorageRangeErrors :: Property
wbStorageRangeErrors = property $ do
  nat <- forAll $ Gen.enum 1 32
  case TN.someNatVal (nat - 1) of
    SomeNat (succSNat . snatProxy -> n) -> go n
 where
  go :: forall v m. (KnownNat v, 1 <= v, Monad m) => SNat v -> PropertyT m ()
  go depth@SNat = do
    content <- forAll $ genNonEmptyVec @v (genDefinedBitVector @32)
    wcre
      $ wishbonePropWithModel @System @30
        defExpectOptions
        model
        (unMemmap $ wbStorage "" depth (Just (Vec content)))
        (genRequests (snatToNum depth))
        (snatToInteger depth)

  genRequests size =
    Gen.list
      (Range.linear 0 32)
      $ do
        upperBound <- Gen.choice $ fmap pure [size, 0]
        WB.genWishboneTransfer (Range.constant 0 upperBound)

  model (Read addr _) s2m@WishboneS2M{..} st0
    | addr >= fromIntegral st0 && err = Right st0
    | addr >= fromIntegral st0 && not err =
        Left
          $ "address out of range on read should error: "
          <> "addr: "
          <> showHex addr ""
          <> ", size "
          <> showHex st0 ""
    | acknowledge = Right st0
    | otherwise =
        Left
          $ "An in-range read should be ACK'd "
          <> "addr: "
          <> showHex addr ""
          <> ", size "
          <> showHex st0 ""
          <> " - "
          <> show s2m
  model (Write addr _ _) s2m@WishboneS2M{..} st0
    | addr >= fromIntegral st0 && err = Right st0
    | addr >= fromIntegral st0 && not err =
        Left
          $ "address out of range on write should error: "
          <> "addr: "
          <> showHex addr ""
          <> ", size "
          <> showHex st0 ""
    | acknowledge = Right st0
    | otherwise =
        Left
          $ "An in-range write should be ACK'd "
          <> "addr: "
          <> showHex addr ""
          <> ", size "
          <> showHex st0 ""
          <> " - "
          <> show s2m

wbStorageProtocolsModel :: Property
wbStorageProtocolsModel = property $ do
  nat <- forAll $ Gen.enum 1 32
  case TN.someNatVal (nat - 1) of
    SomeNat (succSNat . snatProxy -> n) -> go n
 where
  go :: forall v m. (KnownNat v, 1 <= v, Monad m) => SNat v -> PropertyT m ()
  go depth@SNat = do
    content <- forAll $ genNonEmptyVec @v (genDefinedBitVector @32)
    wcre
      $ wishbonePropWithModel @System @30
        defExpectOptions
        model
        (unMemmap $ wbStorage "" depth (Just (Vec content)))
        (genRequests (snatToNum depth))
        (I.fromAscList $ L.zip [0 ..] (toList content))

  genRequests size =
    Gen.list (Range.linear 0 32) $ do
      -- only generate valid requests here
      WB.genWishboneTransfer (Range.constant 0 (size - 1))

  model (Read addr sel) s2m@WishboneS2M{..} st0
    | err || retry =
        Left
          $ "An in-range read should be ACK'd "
          <> "addr: "
          <> showHex addr ""
          <> ", size "
          <> showHex (I.size st0 * 4) ""
          <> " - "
          <> show s2m
    | otherwise =
        let
          val = st0 I.! modelAddr
          maskedReadData = pack $ mux (unpack sel) (unpack readData) (unpack val :: Vec 4 Byte)
         in
          if val == maskedReadData
            then Right st0
            else
              Left
                $ "Read from model results in different value. Model: "
                <> showHex val ""
                <> ", Circuit: "
                <> showHex readData ""
   where
    modelAddr = fromIntegral addr
  model (Write addr sel wr) s2m@WishboneS2M{..} st0
    | err || retry =
        Left
          $ "An in-range write should be ACK'd "
          <> "addr: "
          <> showHex addr ""
          <> ", size "
          <> showHex (I.size st0 * 4) ""
          <> " - "
          <> show s2m
    | otherwise =
        Right $ I.insert modelAddr new st0
   where
    modelAddr = fromIntegral addr
    old = st0 I.! modelAddr
    new = pack $ mux (unpack sel) (unpack wr) (unpack old :: Vec 4 Byte)
