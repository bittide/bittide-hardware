-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UndecidableInstances #-}

{- | A bit-mask type for use in memory-mapped registers.

In Clash designs we often use @'Vec' n 'Bool'@ to track per-bit enables, valid
bits, and similar. The default 'BitPackC' representation of @'Vec' n 'Bool'@
allocates a full byte per bit, which is wasteful when these end up in
memory-mapped registers. Switching to a 'BitVector' is denser, but inverts the
index correspondence: @Vec@ index @0@ maps to the MSB of the bitvector via
Clash's standard 'pack'/'unpack', so a loop walking @0..n@ on the Haskell side
walks the wrong direction relative to firmware that pokes individual bits.

'Mask' fixes both problems: it is packed compactly (like 'BitVector'), and its
'toVec' / 'fromVec' / 'toBitVector' / 'fromBitVector' conversions all preserve
the @Vec@-index <-> bit-position correspondence: index @0@ is bit @0@ (the least
significant bit).

There is intentionally no usable 'BitPack' instance: 'BitPack' is conventionally
read as \"this is the HDL-level layout\", which would be misleading for a type
whose only purpose is to bridge the Haskell\/firmware boundary. An instance is
provided that triggers a 'TypeError' when used, to give a better error message
than the default \"no instance\" one.
-}
module Protocols.MemoryMap.Mask (
  Mask (..),
  toVec,
  fromVec,
  toBitVector,
  fromBitVector,
) where

import Clash.Prelude

import Clash.Class.BitPackC (BitPackC (..))
import Data.Data (Proxy (..))

import Protocols.MemoryMap.TypeDescription (
  TypeArgumentDecl (TadNat),
  TypeDefinition (Builtin),
  TypeDescription (..),
  TypeRef (TypeInst, TypeNat),
  WithTypeDescription (..),
 )
import qualified Protocols.MemoryMap.TypeDescription as TD

-- Clash doesn't handle 'newtype's properly in type families
{-# ANN module ("HLint: ignore Use newtype instead of data" :: String) #-}

{- | A bit-mask of @n@ bits.

Backing storage is an 'Unsigned', but the four conversion functions
('toVec', 'fromVec', 'toBitVector', 'fromBitVector') maintain the convention
that index @0@ corresponds to bit @0@ (the least significant bit), so indices
stay consistent across all four representations.
-}
data Mask (n :: Nat)
  = -- XXX: This is a @data@ rather than @newtype@ because Clash currently has bugs
    --      around newtype handling.
    Mask (Unsigned n)
  deriving (Generic, Show, Eq, NFDataX)

{- | Pack @n@ booleans into a 'Mask'. Index @i@ of the input vector becomes
bit @i@ of the result (LSB-first).
-}
fromVec :: (KnownNat n) => Vec n Bool -> Mask n
fromVec = Mask . bitCoerce . reverse

{- | Unpack a 'Mask' to @n@ booleans. Bit @i@ of the input becomes index @i@
of the output (LSB-first).
-}
toVec :: (KnownNat n) => Mask n -> Vec n Bool
toVec (Mask u) = reverse (bitCoerce u)

{- | Reinterpret a 'BitVector' as a 'Mask'. The bit at position @i@ of the
'BitVector' (counted from the LSB) corresponds to index @i@ in
@'toVec' . 'fromBitVector'@.
-}
fromBitVector :: (KnownNat n) => BitVector n -> Mask n
fromBitVector = Mask . bitCoerce

-- | Inverse of 'fromBitVector'.
toBitVector :: (KnownNat n) => Mask n -> BitVector n
toBitVector (Mask u) = bitCoerce u

instance
  ( KnownNat n
  , TypeError
      ( 'Text "'Mask' intentionally has no 'BitPack' instance."
          ':$$: 'Text "'BitPack' is conventionally read as the HDL-level layout, which"
          ':$$: 'Text "would be misleading for a type whose only purpose is to bridge the"
          ':$$: 'Text "Haskell/firmware boundary."
          ':$$: 'Text ""
          ':$$: 'Text "Use 'toBitVector' / 'fromBitVector' or 'toVec' / 'fromVec' instead."
      )
  ) =>
  BitPack (Mask n)
  where
  type BitSize (Mask n) = n
  pack = error "unreachable"
  unpack = error "unreachable"

instance (KnownNat n) => BitPackC (Mask n) where
  type ConstructorSizeC (Mask n) = ConstructorSizeC (Unsigned n)
  type AlignmentBoundaryC (Mask n) = AlignmentBoundaryC (Unsigned n)
  type ByteSizeC (Mask n) = ByteSizeC (Unsigned n)

  packC# byteOrder (Mask u) = packC# byteOrder u
  maybeUnpackC# byteOrder bytes = Mask <$> maybeUnpackC# byteOrder bytes

instance (KnownNat n) => WithTypeDescription (Mask n) where
  typeDescription Proxy =
    TypeDescription
      { name = ''Mask
      , args = [TadNat ''n]
      , definition = Builtin TD.Mask
      }
  dependsOn Proxy = []
  argTypes Proxy = []
  asTypeRef Proxy = TypeInst ''Mask [TypeNat (natToInteger @n)]
