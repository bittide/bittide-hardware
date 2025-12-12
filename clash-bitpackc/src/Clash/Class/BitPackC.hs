-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

#include "MachDeps.h"

-- | Tooling to pack and unpack Haskell types to and from C-FFI.
module Clash.Class.BitPackC (
  packC,
  maybeUnpackC,
  unpackOrErrorC,
  BitPackC (..),
  ByteOrder (..),
  Bytes,

  -- * Internal
  msbResize,
) where

import Clash.Class.BitPackC.Align (MultipleOf, Padding)
import Clash.Prelude
import Data.Data (Typeable, typeRep)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Proxy (Proxy (Proxy))
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics

{- | Maximum alignment boundary of any type. This is currently hardcoded to 8,
which is what our platforms use (riscv32). In the future this should be made
configurable to support platforms with larger or smaller maximum alignment
requirements.
-}
type MaximumAlignmentBoundary = 8 :: Nat

{- | Pack a type @a@ to a vector of bytes according to C packing conventions
(see 'BitPackC'), where each index corresponds to a byte address. If you want
to pack to words instead, consider using 'BitPackC.Padding.wordPackC'.
-}
packC :: (BitPackC a) => ByteOrder -> a -> Vec (ByteSizeC a) (BitVector 8)
packC byteOrder = unpack . packC# byteOrder

{- | Unpack a vector of bytes packed according to C packing conversions to a type
@a@. Returns a 'Nothing' if decoding fails. This can happen if the bytes mention
a constructor that does not exist, or if the bytes represent a value that does
not fit in the type @a@. Use 'unpackOrErrorC' to replace failed decodes with an
@errorX@ call. If you want to unpack from words instead, consider using
'BitPackC.Padding.wordUnpackC'.
-}
maybeUnpackC :: (BitPackC a) => ByteOrder -> Vec (ByteSizeC a) (BitVector 8) -> Maybe a
maybeUnpackC byteOrder = maybeUnpackC# byteOrder . pack

{- | Unpack a value of type @a@ from a vector of bytes, or throw an error
(@deepErrorX@) if the given bytes cannot be interpreted as a well-formed value.
After synthesis, all error handling will be optimized away.
-}
unpackOrErrorC ::
  forall a.
  (BitPackC a, Typeable a, NFDataX a) =>
  ByteOrder ->
  Vec (ByteSizeC a) (BitVector 8) ->
  a
unpackOrErrorC byteOrder bytes =
  -- TODO: Ideally, only fields that fail to decode should be replaced by calls
  --       to 'deepErrorX'.
  case maybeUnpackC byteOrder bytes of
    Just val -> val
    Nothing ->
      deepErrorX
        $ "Failed to unpack "
        <> show bytes
        <> " as "
        <> show (typeRep (Proxy @a))

data ByteOrder = LittleEndian | BigEndian deriving (Eq, Show)

type BitSizeInBytes a = DivRU (BitSize a) 8
type NextPowerOfTwo a = 2 ^ CLog 2 (Max 1 a)
type Bytes n = BitVector (n * 8)

{- | Calculate the size of a constructor in bytes, given the number of
constructors.
-}
type family ConstructorSize (nConstructors :: Nat) :: Nat where
  ConstructorSize 0 = 0
  ConstructorSize 1 = 0
  ConstructorSize n = DivRU (CLog 2 n) 8

{- | Given a @BitVector@ ordered as @BigEndian@ (Clash's native ordering), return a
@BitVector@ ordered as the given byte order.
-}
toEndianBV ::
  forall n. (KnownNat n) => ByteOrder -> BitVector (n * 8) -> BitVector (n * 8)
toEndianBV = fromEndianBV

{- | Given a @BitVector@ ordered as the given byte order, return a @BitVector@
ordered as @BigEndian@ (Clash's native ordering).
-}
fromEndianBV ::
  forall n. (KnownNat n) => ByteOrder -> BitVector (n * 8) -> BitVector (n * 8)
fromEndianBV BigEndian = id
fromEndianBV LittleEndian = pack . reverse . unpack @(Vec n (BitVector 8))

{- | Type class that can be implemented (or derived) to enable C-FFI safe
representation of Haskell types. It can be derived automatically as long as
'Generic' is implemented for the type.

= How this class works
This class follows \"how C does things\". This is slightly complicated by the
fact that C itself only supports product types (C\/Rust: /structs/). We'll get to
that later though, let's first look at how C packs product types:

1. Fields are laid out in the order they are declared.
2. Each field must be aligned according to its C alignment requirements.
3. Padding bytes are inserted between fields if necessary to meet alignment
   requirements.
4. The total size of the struct will be a multiple of the alignment of its most
   strictly aligned member (which is also the struct's overall alignment
   requirement).

The alignment requirements of atomic types (long, int, float, double, ...) follow
from their size. On some platforms, the alignment of an atomic type is influenced
by the platform's word size, but that is out of scope for this class.

As alluded to before, C does not support sum types (Rust: /field-less enums/)
or sum-of-product types (Rust: /enums/). We follow Rust's definition (see
[repr(C)](https://doc.rust-lang.org/nomicon/other-reprs.html)) of packing them.
Consider all possible sum-of-product types:

> data T
>   = C0 F0_0 F0_1 .. F0_n
>   | C1 F1_0 F1_1 .. F1_m
>   | ..
>   | Ck Fk_0 Fk_1 .. Fk_p

According to @repr(C)@, this is the same as packing all of the following types
in the same memory region (C: /union/, in Rust this is also called a union though
it is barely used within the language):

> type T0 = (C0, padding, (F0_0, F0_1, .., F0_n))
> type T1 = (C1, padding, (F1_0, F1_1, .., F1_m))
> ..
> type Tk = (Ck, padding, (Fk_0, Fk_1, .., Fk_p))

The padding between the constructor tag and the fields is equal to the padding
needed to align the fields to the most strictly aligned fields. Padding is the
same for every @T0@, @T1@, .., @Tk@.

Note that sum-of-product types are structs of structs and each struct must still
follow all the same rules as regular structs. Note that pure sum types are a
special case of sum-of-product types, where @n@, @m@, .., @p@ are all 0.
-}
class
  ( KnownNat (AlignmentBoundaryC a)
  , KnownNat (ByteSizeC a)
  , KnownNat (ConstructorSizeC a)
  ) =>
  BitPackC (a :: Type)
  where
  -- | Number of bytes the constructor takes up
  type ConstructorSizeC a :: Nat

  type ConstructorSizeC a = ConstructorSize (GConstructorCount (Rep a))

  -- | Alignment boundary for this type. This needs to be a power of two.
  type AlignmentBoundaryC a :: Nat

  type
    AlignmentBoundaryC a =
      Min MaximumAlignmentBoundary (Max (ConstructorSizeC a) (GAlignmentBoundaryC (Rep a)))

  -- | Size of this type in bytes
  type ByteSizeC (a :: Type) :: Nat

  type
    ByteSizeC a =
      MultipleOf
        ( -- Add the constructor tag
          ConstructorSizeC a
            -- Plus padding to align the fields
            + Padding (ConstructorSizeC a) (GAlignmentBoundaryC (Rep a))
            -- Plus all the fields
            + MultipleOf (GByteSizeC 0 (Rep a)) (GAlignmentBoundaryC (Rep a))
        )
        ( AlignmentBoundaryC a
        )

  packC# :: ByteOrder -> a -> Bytes (ByteSizeC a)
  default packC# ::
    ( Generic a
    , GBitPackC 0 (Rep a)
    , KnownNat (GByteSizeC 0 (Rep a))
    , KnownNat (GAlignmentBoundaryC (Rep a))
    ) =>
    ByteOrder ->
    a ->
    Bytes (ByteSizeC a)
  packC# byteOrder val = msbResize $ constructorBits ++# padding ++# alignedFields
   where
    alignedFields :: Bytes (MultipleOf (GByteSizeC 0 (Rep a)) (GAlignmentBoundaryC (Rep a)))
    alignedFields = msbResize fields

    constructor :: Int
    fields :: Bytes (GByteSizeC 0 (Rep a))
    (constructor, fields) = gPackFieldsC byteOrder d0 0 (from val)

    constructorBits :: Bytes (ConstructorSizeC a)
    constructorBits = resize (pack constructor)

    padding :: Bytes (Padding (ConstructorSizeC a) (GAlignmentBoundaryC (Rep a)))
    padding = 0

  maybeUnpackC# ::
    ByteOrder ->
    Bytes (ByteSizeC a) ->
    Maybe a
  default maybeUnpackC# ::
    ( Generic a
    , KnownNat (GConstructorCount (Rep a))
    , KnownNat (GByteSizeC 0 (Rep a))
    , KnownNat (GAlignmentBoundaryC (Rep a))
    , GBitPackC 0 (Rep a)
    ) =>
    ByteOrder ->
    Bytes (ByteSizeC a) ->
    Maybe a
  maybeUnpackC# byteOrder val
    | selectedConstructor >= natToNum @(GConstructorCount (Rep a)) = Nothing
    | otherwise = fmap to $ gUnpackFieldsC byteOrder d0 selectedConstructor 0 fields1
   where
    -- XXX: Number of constructors could in theory be larger than what fits in
    --      an 'Int'. In practice this won't happen (I hope?!).
    selectedConstructor :: Int
    selectedConstructor = unpack (resize selectedConstructorBits)

    -- Strip padding introduced by 'MultipleOf' size requirements
    fields1 :: Bytes (GByteSizeC 0 (Rep a))
    fields1 = msbResize fields0

    selectedConstructorBits :: Bytes (ConstructorSizeC a)
    _padding :: Bytes (Padding (ConstructorSizeC a) (GAlignmentBoundaryC (Rep a)))
    fields0 :: Bytes (MultipleOf (GByteSizeC 0 (Rep a)) (GAlignmentBoundaryC (Rep a)))
    (selectedConstructorBits, _padding, fields0) =
      unpack
        $
        -- Strip padding introduced by 'MultipleOf' size requirements
        msbResize val

{- | 'resize', but biased towards the most significant bit (MSB). That is, if
the bit size of the source type is larger than the destination type, it will
discard the least significant bits (LSBs) instead of the MSBs. If the bit size
of the source type is smaller than the destination type, it will pad with
zeroes on the LSB side.
-}
msbResize ::
  forall a b f.
  ( Resize f
  , Bits (f a)
  , Bits (f b)
  , BitPack (f a)
  , BitPack (f b)
  , KnownNat a
  , KnownNat b
  ) =>
  f a ->
  f b
msbResize val =
  case SNat @(BitSize (f a)) `cmpNat` SNat @(BitSize (f b)) of
    LTI -> shiftL (resize val) (bitSizeB - bitSizeA)
    EQI -> resize val
    GTI -> resize (shiftR val (bitSizeA - bitSizeB))
 where
  bitSizeA = natToNum @(BitSize (f a)) :: Int
  bitSizeB = natToNum @(BitSize (f b)) :: Int

-- BitVectors are packed like byte arrays, meaning that
--   - they have an alignment of 1
--   - they are not padded to a size of a power of 2
--   - packed to native endianess
instance (KnownNat n) => BitPackC (BitVector n) where
  type ConstructorSizeC (BitVector n) = 0
  type AlignmentBoundaryC (BitVector n) = 1
  type ByteSizeC (BitVector n) = BitSizeInBytes (BitVector n)

  packC# byteOrder = toEndianBV byteOrder . resize
  maybeUnpackC# byteOrder = checkFits . fromEndianBV byteOrder

instance (KnownNat n) => BitPackC (Unsigned n) where
  type ConstructorSizeC (Unsigned n) = 0
  type
    AlignmentBoundaryC (Unsigned n) =
      Min MaximumAlignmentBoundary (ByteSizeC (Unsigned n))
  type ByteSizeC (Unsigned n) = NextPowerOfTwo (BitSizeInBytes (Unsigned n))

  packC# byteOrder val = res
   where
    bv = pack val
    bvPadded = resize bv :: BitVector (ByteSizeC (Unsigned n) * 8)
    res = toEndianBV byteOrder bvPadded
  maybeUnpackC# byteOrder bv0 = un1
   where
    bv1 = fromEndianBV byteOrder bv0
    un0 = unpack bv1 :: Unsigned (ByteSizeC (Unsigned n) * 8)
    un1 = checkFits un0

instance (KnownNat n) => BitPackC (Signed n) where
  type ConstructorSizeC (Signed n) = 0
  type
    AlignmentBoundaryC (Signed n) =
      Min MaximumAlignmentBoundary (NextPowerOfTwo (BitSizeInBytes (Signed n)))
  type ByteSizeC (Signed n) = NextPowerOfTwo (BitSizeInBytes (Signed n))

  packC# byteOrder val = toEndianBV byteOrder bv
   where
    signExtendedVal = resize val :: Signed (ByteSizeC (Signed n) * 8)
    bv = pack signExtendedVal :: BitVector (ByteSizeC (Signed n) * 8)

  maybeUnpackC# byteOrder bv = checkFits signExtendedVal
   where
    signExtendedVal = unpack (fromEndianBV byteOrder bv) :: Signed (ByteSizeC (Signed n) * 8)

{- | Checks whether the argument fits within the bounds of the result type. Only
works when @BitSize (f a) <= @BitSize (f b)@.
-}
checkFits ::
  forall f a b.
  ( Ord (f a)
  , Bounded (f b)
  , Resize f
  , KnownNat a
  , KnownNat b
  ) =>
  f a ->
  Maybe (f b)
checkFits val
  | val > resize (maxBound :: f b) || val < resize (minBound :: f b) = Nothing
  | otherwise = Just (resize val)

instance (KnownNat n, 1 <= n) => BitPackC (Index n) where
  type ConstructorSizeC (Index n) = 0
  type
    AlignmentBoundaryC (Index n) =
      Min MaximumAlignmentBoundary (NextPowerOfTwo (BitSizeInBytes (Index n)))
  type ByteSizeC (Index n) = NextPowerOfTwo (BitSizeInBytes (Index n))

  packC# byteOrder val = bv2
   where
    bv0 = pack val
    bv1 = resize bv0 :: BitVector (ByteSizeC (Index n) * 8)
    bv2 = toEndianBV byteOrder bv1
  maybeUnpackC# byteOrder bv0 =
    if bv1 > maxVal
      then Nothing
      else Just (unpack $ resize bv1)
   where
    bv1 = fromEndianBV byteOrder bv0 :: BitVector (ByteSizeC (Index n) * 8)
    maxVal = resize (pack (maxBound :: Index n)) :: BitVector (ByteSizeC (Index n) * 8)

instance BitPackC Float where
  type ConstructorSizeC Float = 0
  type AlignmentBoundaryC Float = 4
  type ByteSizeC Float = 4

  packC# byteOrder = toEndianBV byteOrder . pack
  maybeUnpackC# byteOrder = Just . unpack . toEndianBV byteOrder

instance BitPackC Double where
  type ConstructorSizeC Double = 0
  type AlignmentBoundaryC Double = 8
  type ByteSizeC Double = 8

  packC# byteOrder = toEndianBV byteOrder . pack
  maybeUnpackC# byteOrder = Just . unpack . toEndianBV byteOrder

instance BitPackC Bool where
  type ConstructorSizeC Bool = 0
  type AlignmentBoundaryC Bool = 1
  type ByteSizeC Bool = 1

  packC# _byteOrder = \case
    True -> 1
    False -> 0
  maybeUnpackC# _byteOrder v = Just (testBit v 0)

instance BitPackC Bit where
  type ConstructorSizeC Bit = 0
  type AlignmentBoundaryC Bit = 1
  type ByteSizeC Bit = 1

  packC# _byteOrder = resize . pack
  maybeUnpackC# _byteOrder v = Just (boolToBit (testBit v 0))

instance BitPackC () where
  type ConstructorSizeC () = 0
  type AlignmentBoundaryC () = 1
  type ByteSizeC () = 0

  packC# _byteOrder _val = 0
  maybeUnpackC# _byteOrder _val = Just ()

instance (BitPackC a, KnownNat n) => BitPackC (Vec n a) where
  type ConstructorSizeC (Vec n a) = 0
  type AlignmentBoundaryC (Vec n a) = AlignmentBoundaryC a
  type ByteSizeC (Vec n a) = n * ByteSizeC a

  packC# byteOrder = pack . map (packC# byteOrder)
  maybeUnpackC# byteOrder = sequence . map (maybeUnpackC# byteOrder) . unpack

instance (BitPackC a) => BitPackC (Maybe a)
instance (BitPackC a, BitPackC b) => BitPackC (Either a b)
instance (BitPackC a, BitPackC b) => BitPackC (a, b)
instance (BitPackC a, BitPackC b, BitPackC c) => BitPackC (a, b, c)
instance (BitPackC a, BitPackC b, BitPackC c, BitPackC d) => BitPackC (a, b, c, d)

#define THROUGH_INST(T, TT) \
instance BitPackC T where { \
  type ConstructorSizeC T = ConstructorSizeC (TT); \
  type AlignmentBoundaryC T = AlignmentBoundaryC (TT); \
  type ByteSizeC T = ByteSizeC (TT); \
  packC# byteOrder = packC# byteOrder . numConvert @_ @(TT); \
  maybeUnpackC# byteOrder = fmap numConvert . maybeUnpackC# @(TT) byteOrder \
}

THROUGH_INST (Word8, Unsigned 8)
THROUGH_INST (Word16, Unsigned 16)
THROUGH_INST (Word32, Unsigned 32)
THROUGH_INST (Word64, Unsigned 64)
THROUGH_INST (Word, Unsigned WORD_SIZE_IN_BITS)

THROUGH_INST (Int8, Signed 8)
THROUGH_INST (Int16, Signed 16)
THROUGH_INST (Int32, Signed 32)
THROUGH_INST (Int64, Signed 64)
THROUGH_INST (Int, Signed WORD_SIZE_IN_BITS)

class GBitPackC start f where
  type GConstructorCount f :: Nat
  type GByteSizeC (start :: Nat) f :: Nat
  type GAlignmentBoundaryC f :: Nat

  gPackFieldsC ::
    forall a.
    -- | Endianness
    ByteOrder ->
    -- | Offset we're packing at
    SNat start ->
    -- | Current constructor
    Int ->
    f a ->
    ( -- Selected constructor
      Int
    , -- Packed fields
      Bytes (GByteSizeC start f)
    )

  gUnpackFieldsC ::
    forall a.
    -- | Endianness
    ByteOrder ->
    -- | Offset we're unpacking at
    SNat start ->
    -- | Selected constructor
    Int ->
    -- | Current constructor
    Int ->
    Bytes (GByteSizeC start f) ->
    Maybe (f a)

instance (GBitPackC start a) => GBitPackC start (M1 m d a) where
  type GByteSizeC start (M1 m d a) = GByteSizeC start a
  type GConstructorCount (M1 m d a) = GConstructorCount a
  type GAlignmentBoundaryC (M1 m d a) = GAlignmentBoundaryC a

  gPackFieldsC byteOrder start cc (M1 m1) = gPackFieldsC byteOrder start cc m1
  gUnpackFieldsC byteOrder start selectedConstr cc packed =
    fmap M1 $ gUnpackFieldsC byteOrder start selectedConstr cc packed

-- | Sum types
instance
  ( GBitPackC start f
  , GBitPackC start g
  , KnownNat (GByteSizeC start f)
  , KnownNat (GByteSizeC start g)
  , KnownNat (GConstructorCount f)
  ) =>
  GBitPackC start (f :+: g)
  where
  type GByteSizeC start (f :+: g) = Max (GByteSizeC start f) (GByteSizeC start g)
  type GConstructorCount (f :+: g) = GConstructorCount f + GConstructorCount g
  type GAlignmentBoundaryC (f :+: g) = Max (GAlignmentBoundaryC f) (GAlignmentBoundaryC g)

  gPackFieldsC byteOrder start cc (L1 l) =
    (sc, msbResize packed)
   where
    (sc, packed) = gPackFieldsC byteOrder start cc l
  gPackFieldsC byteOrder start cc (R1 r) =
    (sc, msbResize packed)
   where
    ccLeft = natToNum @(GConstructorCount f)
    (sc, packed) = gPackFieldsC byteOrder start (cc + ccLeft) r

  gUnpackFieldsC byteOrder start selectedConstr cc packed =
    if selectedConstr < cc + cLeft
      then fmap L1 $ gUnpackFieldsC byteOrder start selectedConstr cc packedF
      else fmap R1 $ gUnpackFieldsC byteOrder start selectedConstr (cc + cLeft) packedG
   where
    cLeft = natToNum @(GConstructorCount f)

    packedF :: Bytes (GByteSizeC start f)
    packedF = msbResize packed

    packedG :: Bytes (GByteSizeC start g)
    packedG = msbResize packed

{- | Product types: chain them together, with both the left and right part being
responsible for their own alignment.
-}
instance
  ( GBitPackC start f
  , GBitPackC (GByteSizeC start f + start) g
  , KnownNat (GByteSizeC start f)
  , KnownNat (GByteSizeC (start + GByteSizeC start f) g)
  ) =>
  GBitPackC start (f :*: g)
  where
  type
    GByteSizeC start (f :*: g) =
      GByteSizeC start f + GByteSizeC (start + GByteSizeC start f) g
  type GConstructorCount (f :*: g) = 1
  type GAlignmentBoundaryC (f :*: g) = Max (GAlignmentBoundaryC f) (GAlignmentBoundaryC g)

  gPackFieldsC byteOrder start0 cc (l0 :*: r0) = (cc, l1 ++# r1)
   where
    start1 = start0 `addSNat` SNat @(GByteSizeC start f)
    (_, l1) = gPackFieldsC byteOrder start0 cc l0
    (_, r1) = gPackFieldsC byteOrder start1 cc r0

  gUnpackFieldsC byteOrder start0 selectedConstr cc packed = do
    l0 <- gUnpackFieldsC byteOrder start0 selectedConstr cc l0packed
    r0 <- gUnpackFieldsC byteOrder start1 selectedConstr cc l1packed
    Just (l0 :*: r0)
   where
    start1 = start0 `addSNat` SNat @(GByteSizeC start f)
    (l0packed, l1packed) = unpack packed

instance (BitPackC c) => GBitPackC start (K1 i c) where
  type GByteSizeC start (K1 i c) = Padding start (AlignmentBoundaryC c) + ByteSizeC c
  type GConstructorCount (K1 i c) = 1
  type GAlignmentBoundaryC (K1 i c) = AlignmentBoundaryC c

  gPackFieldsC byteOrder SNat cc (K1 i) =
    (cc, padding ++# packC# byteOrder i)
   where
    padding :: Bytes (Padding start (AlignmentBoundaryC c))
    padding = 0

  gUnpackFieldsC byteOrder SNat _selectedConstr _currentConstr packed =
    K1 <$> maybeUnpackC# byteOrder (resize packed)

instance GBitPackC boundary U1 where
  type GByteSizeC boundary U1 = 0
  type GConstructorCount U1 = 1
  type GAlignmentBoundaryC U1 = 1

  gPackFieldsC _byteOrder _start cc _u = (cc, 0)
  gUnpackFieldsC _byteOrder _start _selectedConstr _currentConstr _packed = Just U1
