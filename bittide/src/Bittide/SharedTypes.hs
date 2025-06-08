-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=8 #-}

module Bittide.SharedTypes (
  module Bittide.SharedTypes,
  Bytes,
) where

import Clash.Prelude

import BitPackC (ByteOrder (..), Bytes)
import Data.Constraint
import Data.Constraint.Nat.Lemmas
import Data.Type.Equality ((:~:) (Refl))
import Protocols.Wishbone

-- | To be used when there are two options.
data AorB = A | B deriving (Eq, Generic, BitPack, Show, NFDataX)

-- | If we receive 'A', return 'B'. If we receive 'B', return 'A'
swapAorB :: AorB -> AorB
swapAorB A = B
swapAorB B = A

-- | Polymorphic record update of 'addr'.
updateM2SAddr ::
  BitVector addressWidthNew ->
  WishboneM2S addressWidthOld selWidth dat ->
  WishboneM2S addressWidthNew selWidth dat
updateM2SAddr newAddr WishboneM2S{..} = WishboneM2S{addr = newAddr, ..}

-- | Resize 'WishboneM2S's 'addr' field.
resizeM2SAddr ::
  (KnownNat addressWidthOld, KnownNat addressWidthNew) =>
  WishboneM2S addressWidthOld selWidth dat ->
  WishboneM2S addressWidthNew selWidth dat
resizeM2SAddr WishboneM2S{..} = WishboneM2S{addr = resize addr, ..}

-- | A single byte.
type Byte = BitVector 8

-- | A BitVector that contains one bit per byte in the BitSize of a.
type ByteEnable a = BitVector (Regs a 8)

-- | Type synonym that constrains @a@ and @b@ to both be @KnownNat@ and that @a <= b@.
type LessThan a b = (KnownNat a, KnownNat b, a <= b)

-- Synonym that returns the number of bits required to represent natural number n.
type NatRequiredBits n = CLog 2 (n + 1)

-- Constraint that requires the number of bits required to represent natural number n
-- to be lesser than or equal to bits.
type NatFitsInBits n bits = NatRequiredBits n <= bits

-- | Constraints required to add padding to @a@.
type Paddable a = (BitPack a, NFDataX a)

-- Located i x is a datatype that indicates that data x has a relation with Index i,
-- example usage: write operation of type D to a blockRam with 'i' addresses can
-- be described as: Located i D

-- | @writeData@ has a relation with @Index maxIndex@.
type Located maxIndex writeData = (Index maxIndex, writeData)

-- | @BitVector bits@ has a relation with @Index maxIndex@.
type LocatedBits maxIndex bits = Located maxIndex (BitVector bits)

-- | 'Byte' has a relation with @Index maxIndex@.
type LocatedByte maxIndex = Located maxIndex Byte

-- | 'Bytes' has a relation with @Index maxIndex@.
type LocatedBytes maxIndex nBytes = Located maxIndex (Bytes nBytes)

-- Padding bits added when a is stored in multiples of bw bits.
type Pad a bw = (Regs a bw * bw) - BitSize a

-- Amount of bw sized registers required to store a.
type Regs a bw = DivRU (BitSize a) bw

-- | Stores any arbitrary datatype as a vector of registers.
newtype RegisterBank regSize content (byteOrder :: ByteOrder)
  = RegisterBank (Vec (Regs content regSize) (BitVector regSize))
  deriving (Generic)

instance
  (KnownNat regSize, 1 <= regSize, BitPack content) =>
  BitPack (RegisterBank regSize content byteOrder)
  where
  type
    BitSize (RegisterBank regSize content byteOrder) =
      Regs content regSize * regSize
  pack (RegisterBank vec) = pack vec
  unpack bv = RegisterBank (unpack bv)

deriving newtype instance
  ( KnownNat regSize
  , 1 <= regSize
  , Paddable content
  , NFDataX (RegisterBank regSize content byteOrder)
  ) =>
  NFDataX (RegisterBank regSize content byteOrder)

deriving newtype instance
  (KnownNat regSize, ShowX (RegisterBank regSize content byteOrder)) =>
  ShowX (RegisterBank regSize content byteOrder)

convertBe ::
  (Paddable a, KnownNat bw, 1 <= bw) =>
  (RegisterBank bw a 'BigEndian, a) ->
  (a, RegisterBank bw a 'BigEndian)
convertBe (regBank, a) = (getDataBe regBank, getRegsBe a)

-- | Transforms a to _RegisterBank_.
getRegsLe ::
  forall bw a.
  (Paddable a, KnownNat bw, 1 <= bw) =>
  a ->
  RegisterBank bw a 'LittleEndian
getRegsLe a = case timesDivRU @bw @(BitSize a) of
  Dict -> RegisterBank (reverse $ bitCoerce (0 :: BitVector (Pad a bw), a))

-- | Transforms a to _RegisterBank_.
getRegsBe ::
  forall bw a. (Paddable a, KnownNat bw, 1 <= bw) => a -> RegisterBank bw a 'BigEndian
getRegsBe a = case timesDivRU @bw @(BitSize a) of
  Dict -> RegisterBank (bitCoerce (0 :: BitVector (Pad a bw), a))

-- | Transforms _RegisterBank_ to a.
getDataBe ::
  forall bw a. (Paddable a, KnownNat bw, 1 <= bw) => RegisterBank bw a 'BigEndian -> a
getDataBe (RegisterBank vec) =
  case timesDivRU @bw @(BitSize a) of
    Dict -> unpack . snd $ split @_ @(Pad a bw) @(BitSize a) (pack vec)

-- | Transforms _RegisterBank_ to a.
getDataLe ::
  forall bw a. (Paddable a, KnownNat bw, 1 <= bw) => RegisterBank bw a 'LittleEndian -> a
getDataLe (RegisterBank (reverse -> vec)) =
  case timesDivRU @bw @(BitSize a) of
    Dict -> unpack . snd $ split @_ @(Pad a bw) @(BitSize a) (pack vec)

{- | Coerces a tuple of index n and a boolean to index (n*2) where the LSB of the result
is determined by the boolean.
-}
mul2Index ::
  forall n b.
  (KnownNat n, 1 <= n, BitPack b, BitSize b ~ 1) =>
  Index n ->
  b ->
  Index (n * 2)
mul2Index n b = case clogProductRule @n of Refl -> bitCoerce (n, b)

-- | Coerces an index of size (n*2) to index n with the LSB as separate boolean.
div2Index ::
  forall n b.
  (KnownNat n, 1 <= n, BitPack b, BitSize b ~ 1) =>
  Index (n * 2) ->
  (Index n, b)
div2Index = case clogProductRule @n of Refl -> bitCoerce

-- | Delays the output controls to align them with the actual read / write timing.
delayControls ::
  (HiddenClockResetEnable dom) =>
  Signal dom (WishboneS2M bytes) ->
  Signal dom (WishboneS2M bytes)
delayControls wbIn = wbOut
 where
  delayedAck = register False (acknowledge <$> wbIn)
  delayedErr = register False (err <$> wbIn)
  wbOut =
    (\wb newAck newErr -> wb{acknowledge = newAck, err = newErr})
      <$> wbIn
      <*> delayedAck
      <*> delayedErr

-- | Takes an implicit reset and a Signal dom Bool that can force a reset when True.
forceReset ::
  (HiddenReset dom) =>
  -- | Forces a reset when True.
  Signal dom Bool ->
  -- | Active when the implicit reset is active or the first argument is True.
  Reset dom
forceReset force = unsafeFromActiveHigh (unsafeToActiveHigh hasReset .||. force)

-- | Divide and round up.
divRU :: (Integral a) => a -> a -> a
divRU b a = (b + a - 1) `div` a
