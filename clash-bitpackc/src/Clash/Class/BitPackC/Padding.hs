-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Clash.Class.BitPackC.Padding (
  packWordC,
  maybeUnpackWordC,
  unpackWordOrErrorC,
  SizeInWordsC,

  -- * Internal
  pad,
  unpad,
) where

import Clash.Prelude

import Clash.Class.BitPackC (
  BitPackC,
  ByteOrder,
  ByteSizeC,
  Bytes,
  maybeUnpackC,
  packC,
  unpackOrErrorC,
 )
import Data.Typeable (Typeable)

type SizeInWords wordSize nBytes = nBytes `DivRU` wordSize
type SizeInWordsC wordSize a = SizeInWords wordSize (ByteSizeC a)

-- | Like 'packC', but pads the input to a multiple of a given word size.
packWordC ::
  forall wordSize a.
  ( BitPackC a
  , KnownNat wordSize
  , 1 <= wordSize
  ) =>
  ByteOrder ->
  a ->
  Vec (SizeInWordsC wordSize a) (Bytes wordSize)
packWordC endian a = pad (packC endian a)

-- | Like 'maybeUnpackC', but unpads the input from a multiple of a given word size.
maybeUnpackWordC ::
  forall wordSize a.
  ( BitPackC a
  , KnownNat wordSize
  , 1 <= wordSize
  ) =>
  ByteOrder ->
  Vec (SizeInWordsC wordSize a) (Bytes wordSize) ->
  Maybe a
maybeUnpackWordC endian bytes = maybeUnpackC endian (unpad bytes)

{- | Like 'unpackOrErrorC', but unpads the input from a multiple of a given word
size.
-}
unpackWordOrErrorC ::
  forall wordSize a.
  ( BitPackC a
  , Typeable a
  , NFDataX a
  , KnownNat wordSize
  , 1 <= wordSize
  ) =>
  ByteOrder ->
  Vec (SizeInWordsC wordSize a) (Bytes wordSize) ->
  a
unpackWordOrErrorC endian bytes =
  unpackOrErrorC endian (unpad bytes)

-- | Pack bytes into a bunch of words. Pad with zero-bytes if necessary.
pad ::
  forall nBytes wordSize.
  ( KnownNat nBytes
  , KnownNat wordSize
  , 1 <= wordSize
  ) =>
  Vec nBytes (Bytes 1) ->
  Vec (SizeInWords wordSize nBytes) (Bytes wordSize)
pad v =
  case compareSNat (SNat @nBytes) (SNat @(SizeInWords wordSize nBytes * wordSize)) of
    SNatLE -> map pack (unconcatI (padVec 0 v))
    _ -> clashCompileError "Impossible: nBytes > SizeInWords wordSize nBytes"

{- | Unpack words into a vector of bytes, in such a way that the padding bytes
introduced by 'pad' are removed again.
-}
unpad ::
  forall nBytes wordSize.
  ( KnownNat nBytes
  , KnownNat wordSize
  , 1 <= wordSize
  ) =>
  Vec (SizeInWords wordSize nBytes) (Bytes wordSize) ->
  Vec nBytes (Bytes 1)
unpad v =
  case compareSNat (SNat @nBytes) (SNat @(SizeInWords wordSize nBytes * wordSize)) of
    SNatLE -> unpadVec (concat (map unpack v))
    _ -> clashCompileError "Impossible: nBytes > SizeInWords wordSize nBytes"

-- | Extend a vector with supplied default value to a given length
padVec ::
  forall n m a.
  ( KnownNat n
  , KnownNat m
  , n <= m
  ) =>
  a ->
  Vec n a ->
  Vec m a
padVec a vec = leToPlusKN @n @m (vec ++ repeat a)

-- | Truncate a vector to a given length
unpadVec ::
  forall n m a.
  ( KnownNat n
  , KnownNat m
  , n <= m
  ) =>
  Vec m a ->
  Vec n a
unpadVec vec = leToPlusKN @n @m (takeI @n vec)
