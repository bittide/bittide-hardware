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

type ByteEnable bytes = BitVector bytes
type DataLink frameWidth = Maybe (BitVector frameWidth)
type LessThan a b = (KnownNat a, KnownNat b, a <= b)
type NatFitsInBits n bits = NatRequiredBits n <= bits
type NatRequiredBits n = CLog 2 (n + 1)
type Paddable a = (BitPack a, NFDataX a, 1 <= BitSize a)
type TypeRequiredRegisters t regSize = DivRU (BitSize t) regSize
type WriteAny maxIndex writeData = Maybe (Index maxIndex, writeData)

type Pad a bw  = (Regs a bw * bw) - BitSize a
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
