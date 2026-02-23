-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{- |
Utilities for working with subsets (views) of bits within a 'BitVector'. See "BitView" for
more details.
-}
module Bittide.BitView (
  -- * Construction
  BitView (..),
  bitFromBytes,
  aBitFromBytes,
  map,

  -- * Usage
  get,
  set,
  over,

  -- * Helper functions / internals
  setBitTo,
) where

import Clash.Prelude hiding (map, slice)

import qualified Clash.Prelude as C

{- $setup
>>> import Clash.Prelude hiding (map, slice)
-}

{- | A 'BitView' defines a view into a 'BitVector' that allows you to get and set a subset of
bits as a different type. The 'getter' function extracts the view from the 'BitVector',
while the 'setter' function takes a value of the view type and updates the corresponding
bits.

It bittide we use 'BitView' to select specific bits from a transceiver word and encode
metadata. Usually, you'd simply decode the 'BitVector' into its fields and pass that to
other functions, but using 'BitView' allows us to treat data as a 'BitVector' everywhere.
This is helpful when the various components (word align, magic detect, etc) are in
passthrough mode and therefore need access to the whole vector instead of the subset they
would otherwise work on.
-}
data BitView (n :: Nat) a = BitView
  { getter :: BitVector n -> a
  , setter :: a -> BitVector n -> BitVector n
  }

{- | A slice that selects a single bit (i) from every byte in a given 'BitVector'.

>>> let view = bitFromBytes (SNat @0) :: BitView 16 (BitVector 2)
>>> get view 0b0000_0001_0000_0000
0b10
>>> get view 0b0000_0001_0000_0001
0b11
>>> set view 0b10 0b0000_0000_0000_0000
0b0000_0001_0000_0000
>>> set view 0b11 0b1111_1110_1111_1110
0b1111_1111_1111_1111
-}
bitFromBytes :: forall n i. (KnownNat n, i <= 7) => SNat i -> BitView (n * 8) (BitVector n)
bitFromBytes i@SNat =
  BitView
    { getter = pack . C.map lsb . C.map (`shiftR` snatToNum i) . unpack @(Vec n (BitVector 8))
    , setter = \slice bv -> pack (zipWith (setBitTo i) (unpack @(Vec n (BitVector 8)) bv) (unpack slice))
    }

{- | Similar to 'bitFromBytes', but instead of returning a 'BitVector n' it returns a type
that is an instance of 'BitPack'. This allows you to treat the selected bits as a different
type, which can be more convenient when working with metadata that has a specific
structure. The type 'a' must be small enough to fit within the number of bits selected
by the view. If the type 'a' is larger than the number of bits selected, it will be
truncated to fit.

>>> let view = aBitFromBytes (SNat @1) :: BitView 16 (Unsigned 2)
>>> get view 0b0000_0010_0000_0000
2
>>> get view 0b0000_0010_0000_0010
3
>>> set view 2 0b0000_0000_0000_0000
0b0000_0010_0000_0000
-}
aBitFromBytes ::
  forall a n i.
  ( KnownNat n
  , BitPack a
  , BitSize a <= n
  , i <= 7
  ) =>
  SNat i ->
  BitView (n * 8) a
aBitFromBytes i = map (bitFromBytes @n i) (unpack . resize) (resize . pack)

{- | Map a 'BitView' of type 'a' to a 'BitView' of type 'b' using the provided conversion
functions.

>>> let bvView = bitFromBytes (SNat @0) :: BitView 8 (BitVector 1)
>>> let boolView = map bvView (bitToBool . unpack) (pack . boolToBit)
>>> get boolView 0b0000_0001
True
>>> get boolView 0b0000_0000
False
>>> set boolView True 0b0000_0000
0b0000_0001
-}
map :: BitView n a -> (a -> b) -> (b -> a) -> BitView n b
map (BitView{getter, setter}) aToB bToA =
  BitView
    { getter = aToB . getter
    , setter = \b -> setter (bToA b)
    }

{- | Extract a value from a 'BitVector' using a 'BitView'.

>>> let view = bitFromBytes (SNat @0) :: BitView 16 (BitVector 2)
>>> get view 0b0000_0001_0000_0001
0b11
-}
get :: BitView n a -> BitVector n -> a
get (BitView{getter}) = getter

{- | Set a value in a 'BitVector' using a 'BitView'.

>>> let view = bitFromBytes (SNat @0) :: BitView 16 (BitVector 2)
>>> set view 0b11 0b0000_0000_0000_0000
0b0000_0001_0000_0001
-}
set :: BitView n a -> a -> BitVector n -> BitVector n
set (BitView{setter}) = setter

{- | Apply a function to a value within a 'BitVector' using a 'BitView'.

>>> let view = aBitFromBytes (SNat @0) :: BitView 16 (Unsigned 2)
>>> over view (+1) 0b0000_0001_0000_0000
0b0000_0001_0000_0001
-}
over :: BitView n a -> (a -> a) -> BitVector n -> BitVector n
over BitView{getter, setter} f bv = setter (f (getter bv)) bv

{- | Set a specific bit in a 'BitVector' to the given 'Bit' value.

>>> setBitTo (SNat @0) (0b0000 :: BitVector 4) 1
0b0001
>>> setBitTo (SNat @2) (0b0000 :: BitVector 4) 1
0b0100
>>> setBitTo (SNat @1) (0b1111 :: BitVector 4) 0
0b1101
-}
setBitTo :: (KnownNat n, 1 <= n, i <= n) => SNat i -> BitVector n -> Bit -> BitVector n
setBitTo i x b = clearBit x (snatToNum i) .|. (shiftL (numConvert b) (snatToNum i))
