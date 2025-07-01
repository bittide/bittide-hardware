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
  msbResize,
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
pad = unpack . msbResize . pack

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
unpad = unpack . msbResize . pack
