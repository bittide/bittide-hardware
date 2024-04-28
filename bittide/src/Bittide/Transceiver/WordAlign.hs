-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE NamedFieldPuns #-}

-- | Fundamentally, our transceivers are sending over single bits. Through transceiver
-- IP we can send words (e.g., 32 bits) on one end and receive words on the other
-- end. The IP makes sure that any received are byte aligned. That means that a
-- stream:
--
-- > -----------------------------------------------
-- > | B3 B2 B1 B0 | B3 B2 B1 B0 | B3 B2 B1 B0 | ...
-- > -----------------------------------------------
--
-- ..might be received on the other end as:
--
-- > -----------------------------------------------
-- > | .. .. B3 B2 | B1 B0 B3 B2 | B1 B0 B3 B2 | ...
-- > -----------------------------------------------
--
-- or by any other shift (or none at all!). This module provides utilities to end
-- up with a word aligned stream. The basic idea is that, while \"booting\" the
-- connection the MSB of each byte is reserved, where an asserted MSB indicates
-- the start of a word. The other bits can be used to detect a valid connection
-- through PRBS streams.
--
-- TODO: Remove this module in favor of retry logic. That is, we can keep
--       resetting the transceivers until they're aligned. Its unclear how exactly
--       this should work though.
module Bittide.Transceiver.WordAlign
  ( alignBytesFromMsbs
  , alignBytes
  , align

  -- * Convenience functions
  , alignSymbol
  , splitMsbs
  , joinMsbs
  , dealignBytes
  ) where

import Clash.Prelude
import Data.Bifunctor (Bifunctor (bimap))
import Bittide.SharedTypes (Bytes, Byte)

data AlignState n a
  = Waiting
  | Aligning { prev :: Vec n a, offset :: Index n }
  deriving (Show, Generic, ShowX, NFDataX)

alignSymbol :: forall n . (KnownNat n, 1 <= n) => BitVector n
alignSymbol = bit (natToNum @(n - 1))

-- | Split the MSBs of a 'BitVector's \"bytes\" into a 'BitVector' of MSBs and a
-- 'BitVector' of the remaining bits.
splitMsbs ::
  forall nBytes byteWidth .
  ( KnownNat nBytes
  , KnownNat byteWidth
  , 1 <= byteWidth
  ) =>
  BitVector (nBytes * byteWidth) ->
  ( BitVector nBytes
  , BitVector (nBytes * (byteWidth - 1))
  )
splitMsbs =
    bimap pack pack
  . unzip
  . unpack @(Vec nBytes (Bit, BitVector (byteWidth - 1)))

-- | Opposite of 'splitMsbs'.
joinMsbs ::
  forall nBytes byteWidth .
  ( KnownNat nBytes
  , KnownNat byteWidth
  , 1 <= byteWidth
  ) =>
  BitVector nBytes ->
  BitVector (nBytes * (byteWidth - 1)) ->
  BitVector (nBytes * byteWidth)
joinMsbs msbs bvs = pack $
  zip
    (unpack @(Vec nBytes Bool) msbs)
    (unpack @(Vec nBytes (BitVector(byteWidth - 1))) bvs)

-- | Specialized version of 'align' that works on 'BitVector' and assumes that
-- the alignment bits are stored in the MSBs of the bytes.
alignBytesFromMsbs ::
  forall n dom .
  ( HiddenClockResetEnable dom
  , KnownNat n
  , 1 <= n
  ) =>
  -- | Data with alignment bits in the byte's MSBs
  Signal dom (Bytes n) ->
  -- | 'Nothing' until an alignment symbol has been observed. 'Just' if an alignment
  -- symbol has been observed.
  Signal dom (Maybe (Bytes n))
alignBytesFromMsbs dat = alignBytes (fst . splitMsbs <$> dat) dat

-- | Specialized version of 'align' that works on 'BitVector'
alignBytes ::
  forall n dom .
  ( HiddenClockResetEnable dom
  , KnownNat n
  , 1 <= n
  ) =>
  -- | One hot encoded alignment signal. Data is assumed to be invalid as long as
  -- all booleans are 'False'.
  Signal dom (BitVector n) ->
  -- | Data
  Signal dom (Bytes n) ->
  -- | 'Nothing' until an alignment symbol has been observed. 'Just' if an alignment
  -- symbol has been observed.
  Signal dom (Maybe (Bytes n))
alignBytes aligns =
    fmap (fmap pack)
  . align @Byte (unpack <$> aligns)
  . fmap unpack

-- |
align ::
  forall a n dom .
  ( HiddenClockResetEnable dom
  , KnownNat n
  , NFDataX a
  , 1 <= n
  ) =>
  -- | One hot encoded alignment signal. Data is assumed to be invalid as long as
  -- all booleans are 'False'.
  Signal dom (Vec n Bool) ->
  -- | Data
  Signal dom (Vec n a) ->
  -- | 'Nothing' until an alignment symbol has been observed. 'Just' if an
  -- alignment symbol has been observed.
  Signal dom (Maybe (Vec n a))
align = curry (delay Nothing . mealyB go (Waiting @n @a))
 where
  go :: AlignState n a -> (Vec n Bool, Vec n a) -> (AlignState n a, Maybe (Vec n a))
  go Waiting (alignment, as) = (, Nothing) $
    case elemIndex True alignment of
      Just offset -> Aligning{prev=as, offset}
      Nothing -> Waiting
  go (Aligning{prev, offset}) (_, current) =
    ( Aligning{prev=current, offset}
    , Just (takeI (rotateLeft (prev ++ current) offset))
    )

-- | Opposite of 'align'. Useful for testing. The first and last word are padded
-- with zeros. If the given offset is zero, the first and last word are all zeros.
-- This function mimics the behavior of the transceiver IP, i.e., it mimics reading
-- at a certain offset in the stream. (Also see this module's documentation.)
dealignBytes ::
  forall nBytes .
  (KnownNat nBytes) =>
  -- Offset in number of bytes
  Index nBytes ->
  [Bytes nBytes] ->
  [Bytes nBytes]
dealignBytes offset = go 0
 where
  go :: Bytes nBytes -> [Bytes nBytes] -> [Bytes nBytes]
  go prev [] = [dealignWord prev 0]
  go prev (bv : bvs) = dealignWord prev bv : go bv bvs

  dealignWord :: Bytes nBytes -> Bytes nBytes -> Bytes nBytes
  dealignWord bv1 bv2 = truncateB (shiftR (bv1 ++# bv2) (8 * fromIntegral offset))
