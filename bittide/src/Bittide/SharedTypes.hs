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

type Byte = BitVector 8
type Bytes n = BitVector (n*8)
type ByteEnable bytes = BitVector bytes
type DataLink frameWidth = Maybe (BitVector frameWidth)
type LessThan a b = (KnownNat a, KnownNat b, a <= b)

-- Synonym that returns the amount of bits required to represent natural number n.
type NatRequiredBits n = CLog 2 (n + 1)

-- Constraint that requires the amount of bits required to represent natural number n
-- to be lesser than or equal to bits.
type NatFitsInBits n bits = NatRequiredBits n <= bits

type Paddable a = (BitPack a, NFDataX a, 1 <= BitSize a)

-- Located i x is a datatype that indicates that data x has a relation with Index i,
-- example usage: write operation of type D to a blockRam with 'i' addresses can
-- be described as: Located i D
type Located maxIndex writeData = (Index maxIndex, writeData)
type LocatedBits maxIndex bits = Located maxIndex (BitVector bits)
type LocatedByte maxIndex = Located maxIndex Byte
type LocatedBytes maxIndex bytes = Located maxIndex (Bytes bytes)

-- Padding bits added when a is stored in multiples of bw bits.
type Pad a bw  = (Regs a bw * bw) - BitSize a
-- Amount of bw sized registers required to store a.
type Regs a bw = DivRU (BitSize a) bw

-- | Vector of registers that stores data coming from a communication bus. It can be used
-- to store any arbitrary data type, the last register is padded with p bits. The number of
-- registers and the amount of padding depends on the bit size of the stored data.
newtype RegisterBank regSize content =
  RegisterBank (Vec (Regs content regSize) (BitVector regSize))

deriving newtype instance (KnownNat regSize, 1 <= regSize, Paddable content, NFDataX (RegisterBank regSize content)) => NFDataX (RegisterBank regSize content)

deriving newtype instance (KnownNat regSize, 1 <= regSize, ShowX (RegisterBank regSize content)) =>
  ShowX (RegisterBank regSize content)

newtype Padded bw a = Padded {paddedToData :: a}

deriving newtype instance (KnownNat bw, 1 <= bw, Paddable a, NFDataX a, NFDataX (Padded bw a)) =>
  NFDataX (Padded bw a)

bvAsPadded :: forall bw a. (Paddable a, KnownNat bw, 1 <= bw) => BitVector bw -> Padded bw a
bvAsPadded bv =
  case timesDivRU @bw @(BitSize a) of
    Dict -> case sameNat (Proxy @(Pad a bw + BitSize a)) (Proxy @bw) of
      Just Refl -> Padded . unpack . snd $ split @_ @(Pad a bw) @(BitSize a) bv
      _ -> error "bvAsPadded: Negative padding"

registersToData :: (Paddable a, KnownNat regSize, 1 <= regSize) => RegisterBank regSize a -> a
registersToData = paddedToData . registersToPadded

paddedToRegisters :: forall bw a . (BitPack a, KnownNat bw, 1 <= bw) => Padded bw a -> RegisterBank bw a
paddedToRegisters (Padded a) = case timesDivRU @bw @(BitSize a) of
  Dict -> RegisterBank (unpack ((0b0 :: BitVector (Pad a bw)) ++# pack a))

registersToPadded :: forall bw a . (Paddable a, KnownNat bw, 1 <= bw) => RegisterBank bw a -> Padded bw a
registersToPadded (RegisterBank vec) =
  case timesDivRU @bw @(BitSize a) of
    Dict -> Padded . unpack . snd $ split @_ @(Pad a bw) @(BitSize a) (pack vec)

getRegs :: (BitPack a, KnownNat regSize, 1 <= regSize) => a -> RegisterBank regSize a
getRegs = paddedToRegisters . Padded
