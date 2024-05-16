-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE NamedFieldPuns #-}

-- | Fundamentally, our transceivers are sending over single bits. Through transceiver
-- IP we can send words (e.g., 32 bits) on one end and receive words on the other
-- end. The IP makes sure that any received are byte aligned. That means that a
-- stream:
--
-- > ----------------------------------
-- > | A3 A2 A1 A0 | B3 B2 B1 B0 | ...
-- > ----------------------------------
--
-- ..might be received on the other end as:
--
-- > -----------------------------------------------
-- > | .. .. A3 A2 | A1 A0 B3 B2 | B1 B0 .. .. | ...
-- > -----------------------------------------------

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
  , align

  -- * Convenience functions
  , alignSymbol
  , splitMsbs
  , joinMsbs
  ) where

import Clash.Prelude

import Bittide.SharedTypes (Bytes)
import Clash.Explicit.Prelude (noReset)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Maybe (fromMaybe)
import Data.Tuple.Extra (curry3)

data State n m = State { prev :: BitVector (n * m), offset :: Index n }
  deriving (Show, Generic, ShowX, NFDataX)

defState :: (KnownNat n, KnownNat m) => State n m
defState = State{prev=0, offset=0}

alignSymbol :: forall n . (KnownNat n, 1 <= n) => BitVector n
alignSymbol = 1 +>>. 0

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
  ( HiddenClock dom
  , KnownNat n
  , 1 <= n
  ) =>
  -- | Freeze alignment symbol updates
  Signal dom Bool ->
  -- | Data with alignment bits in the byte's MSBs
  Signal dom (Bytes n) ->
  -- | Aligned data, according to alignment data saved in the last cycle
  Signal dom (Bytes n)
alignBytesFromMsbs freeze dat =
  let msbs = fst . splitMsbs @n <$> dat
   in align freeze (unpack <$> msbs) dat

-- | Align data according to an alignment signal. The alignment signal is a
-- one-hot encoded signal that indicates which bit is the alignment bit. Note
-- that the behavior of this function is undefined when:
--
--   * Switching from one alignment bit to another alignment bit
--   * No alignment bit or multiple are asserted
--
-- Alignment can be "frozen" by asserting the freeze signal. This is useful when
-- switching over from a handshake protocol to a data protocol.
align ::
  forall n m dom .
  ( HiddenClock dom
  , KnownNat n
  , KnownNat m
  , 1 <= n
  ) =>
  -- | Freeze alignment symbol updates
  Signal dom Bool ->
  -- | One hot encoded alignment signal. Behavior is undefined when multiple
  -- bits or none are asserted.
  Signal dom (Vec n Bool) ->
  -- | Data
  Signal dom (BitVector (n * m)) ->
  -- | Aligned data, according to alignment data saved in the last cycle
  Signal dom (BitVector (n * m))
align =
  withEnable enableGen $
    withReset noReset $
      curry3 (mealy go (defState @n @m) . bundle)
 where
  go :: State n m -> (Bool, Vec n Bool, BitVector (n * m)) -> (State n m, BitVector (n * m))
  go (State{prev, offset}) (freeze, alignment, current) =
    ( State{prev=current, offset=newOffset}
    , takeMsbs (shiftL (prev ++# current) (fromIntegral (offset `mul` mAsIndex)))
    )
   where
    mAsIndex = maxBound :: Index (m + 1)

    newOffset
      | freeze = offset
      | otherwise = fromMaybe 0 (elemIndex True alignment)

  takeMsbs :: forall i . KnownNat i => BitVector (i * 2) -> BitVector i
  takeMsbs bv = truncateB (shiftR bv (natToNum @i))
