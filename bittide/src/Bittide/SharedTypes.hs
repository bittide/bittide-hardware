-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# OPTIONS_GHC -fconstraint-solver-iterations=8 #-}

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Bittide.SharedTypes where
import Clash.Prelude

import Data.Proxy
import Data.Type.Equality ((:~:)(Refl))
import Data.Constraint
import Data.Constraint.Nat.Extra
import Bittide.Extra.Wishbone

-- | A single byte.
type Byte = BitVector 8
-- | BitVector of _n_ bytes.
type Bytes n = BitVector (n*8)
-- | A BitVector that contains one bit per byte in the BitSize of a.
type ByteEnable a = BitVector (Regs a 8)
-- | Either contains a @Just (BitVector frameWidth)@ or @Nothing@.
type DataLink frameWidth = Maybe (BitVector frameWidth)
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
type Pad a bw  = (Regs a bw * bw) - BitSize a
-- Amount of bw sized registers required to store a.
type Regs a bw = DivRU (BitSize a) bw

-- | Vector of registers that stores data coming from a communication bus. It can be used
-- to store any arbitrary data type, the last register is padded with p bits. The number of
-- registers and the amount of padding depends on the bit size of the stored data.
newtype RegisterBank regSize content =
  RegisterBank (Vec (Regs content regSize) (BitVector regSize))

deriving newtype instance
  (KnownNat regSize, 1 <= regSize, Paddable content, NFDataX (RegisterBank regSize content))
  => NFDataX (RegisterBank regSize content)

deriving newtype instance (KnownNat regSize, ShowX (RegisterBank regSize content)) =>
  ShowX (RegisterBank regSize content)

-- | Stores any a, along with a type level variable that contains a bit width to which @a@
-- should be padded.
newtype Padded bw a = Padded {paddedToData :: a}

deriving newtype instance (KnownNat bw, Paddable a, NFDataX a, NFDataX (Padded bw a)) =>
  NFDataX (Padded bw a)

-- | Transforms a @BitVector bw@ containing a @Padded bw a@ to _Padded_.
bvAsPadded :: forall bw a. (Paddable a, KnownNat bw) => BitVector bw -> Padded bw a
bvAsPadded bv =
  case timesDivRU @bw @(BitSize a) of
    Dict -> case sameNat (Proxy @(Pad a bw + BitSize a)) (Proxy @bw) of
      Just Refl -> Padded . unpack . snd $ split @_ @(Pad a bw) @(BitSize a) bv
      _ -> error "bvAsPadded: Negative padding"

-- | Gets the stored data from a _RegisterBank_.
registersToData :: (Paddable a, KnownNat regSize) => RegisterBank regSize a -> a
registersToData = paddedToData . registersToPadded

-- | Transforms _Padded_ to _RegisterBank_.
paddedToRegisters :: forall bw a . (BitPack a, KnownNat bw) => Padded bw a -> RegisterBank bw a
paddedToRegisters (Padded a) = case timesDivRU @bw @(BitSize a) of
  Dict -> RegisterBank (unpack ((0b0 :: BitVector (Pad a bw)) ++# pack a))

-- | Transforms _RegisterBank_ to _Padded_.
registersToPadded :: forall bw a . (Paddable a, KnownNat bw) => RegisterBank bw a -> Padded bw a
registersToPadded (RegisterBank vec) =
  case timesDivRU @bw @(BitSize a) of
    Dict -> Padded . unpack . snd $ split @_ @(Pad a bw) @(BitSize a) (pack vec)

-- Stores its argument in a _RegisterBank_ based on a context-supplied register size.
getRegs :: (BitPack a, KnownNat regSize) => a -> RegisterBank regSize a
getRegs = paddedToRegisters . Padded

-- | Coerces a tuple of index n and a boolean to index (n*2) where the LSB of the result
-- is determined by the boolean.
mul2Index ::
  forall n b .
  (KnownNat n, 1 <= n, BitPack b, BitSize b ~ 1) =>
  Index n ->
  b ->
  Index (n*2)
mul2Index n b= case clog2axiom @n of Refl -> bitCoerce (n, b)

-- | Coerces an index of size (n*2) to index n with the LSB as separate boolean.
div2Index ::
  forall n b .
  (KnownNat n, 1 <= n, BitPack b, BitSize b ~ 1) =>
  Index (n*2) ->
  (Index n, b)
div2Index = case clog2axiom @n of Refl -> bitCoerce

-- | Delays the output controls to align them with the actual read / write timing.
delayControls ::
  HiddenClockResetEnable dom =>
  Signal dom (WishboneS2M bytes) ->
  Signal dom (WishboneS2M bytes)
delayControls wbIn = wbOut
 where
   delayedAck = register False (acknowledge <$> wbIn)
   delayedErr = register False (err <$> wbIn)
   wbOut = (\wb newAck newErr-> wb{acknowledge = newAck, err = newErr})
    <$> wbIn <*> delayedAck <*> delayedErr

-- | Takes an implicit reset and a Signal dom Bool that can force a reset when True.
forceReset ::
  HiddenReset dom =>
  -- | Forces a reset when True.
  Signal dom Bool ->
  -- | Active when the implicit reset is active or the first argument is True.
  Reset dom
forceReset force = unsafeFromHighPolarity (unsafeToHighPolarity hasReset .||. force)

-- | Divide and round up.
divRU :: Integral a => a -> a -> a
divRU b a = (b + a - 1) `div` a
