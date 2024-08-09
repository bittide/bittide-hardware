-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE NamedFieldPuns #-}

-- or by any other shift (or none at all!). This module provides utilities to end
-- up with a word aligned stream. The basic idea is that, while \"booting\" the
-- connection the MSB of each byte is reserved, where an asserted MSB indicates
-- the start of a word. The other bits can be used to detect a valid connection
-- through PRBS streams.
--
-- TODO: Remove this module in favor of retry logic. That is, we can keep
--       resetting the transceivers until they're aligned. Its unclear how exactly
--       this should work though.

{- | Fundamentally, our transceivers are sending over single bits. Through transceiver
IP we can send words (e.g., 32 bits) on one end and receive words on the other
end. The IP makes sure that any received are byte aligned. That means that a
stream:

> ---------------------------------------------------
> ... | A3 A2 A1 A0 | B3 B2 B1 B0 | C3 C2 C1 C0 | ...
> ---------------------------------------------------

..might be, assuming LSB first transmission, received on the other end as:

> -----------------------------------------------------------------
> ... | A1 A0 .. .. | A3 A2 B1 B0 | C1 C0 B3 B2 | .. .. C3 C2 | ...
> -----------------------------------------------------------------
-}
module Bittide.Transceiver.WordAlign (
  alignBytesFromMsbs,

  -- * Convenience functions
  alignSymbol,
  splitMsbs,
  joinMsbs,

  -- * Core functions and utilities
  AlignmentFn,
  aligner,
  alignLsbFirst,
  alignMsbFirst,

  -- * Dealigning (for testing purposes)
  dealignLsbFirst,
  dealignMsbFirst,
) where

import Clash.Prelude

import Bittide.SharedTypes (Bytes)
import Clash.Explicit.Prelude (noReset)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Maybe (fromMaybe)
import Data.Tuple.Extra (curry3)

{- | Split the MSBs of a 'BitVector's \"bytes\" into a 'BitVector' of MSBs and a
'BitVector' of the remaining bits.
-}
splitMsbs ::
  forall nBytes byteWidth.
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
  forall nBytes byteWidth.
  ( KnownNat nBytes
  , KnownNat byteWidth
  , 1 <= byteWidth
  ) =>
  BitVector nBytes ->
  BitVector (nBytes * (byteWidth - 1)) ->
  BitVector (nBytes * byteWidth)
joinMsbs msbs bvs =
  pack
    $ zip
      (unpack @(Vec nBytes Bool) msbs)
      (unpack @(Vec nBytes (BitVector (byteWidth - 1))) bvs)

alignSymbol :: forall n. (KnownNat n, 1 <= n) => BitVector n
alignSymbol = 1 +>>. 0

{- | Specialized version of 'aligner' that assumes an 'alignSymbol' is stored in
the MSB of each byte.
-}
alignBytesFromMsbs ::
  forall n dom.
  ( HiddenClock dom
  , KnownNat n
  , 1 <= n
  ) =>
  -- | Alignment function (typically 'alignLsbFirst' or 'alignMsbFirst')
  AlignmentFn n ->
  -- | Freeze alignment symbol updates (i.e., keep the current alignment)
  Signal dom Bool ->
  -- | Data with alignment bits in the byte's MSBs
  Signal dom (Bytes n) ->
  -- | Aligned data, according to alignment data saved in the last cycle
  Signal dom (Bytes n)
alignBytesFromMsbs alignFn freeze dat =
  aligner alignFn freeze (oneHotDecoder <$> msbs) dat
 where
  msbs = unpack . fst . splitMsbs @n <$> dat
  oneHotDecoder = fromMaybe 0 . elemIndex True

data State n = State {prev :: Bytes n, offset :: Index n}
  deriving (Show, Generic, ShowX, NFDataX)

-- | Alignment circuit that is generic in its alignment function
aligner ::
  forall n dom.
  ( HiddenClock dom
  , KnownNat n
  , 1 <= n
  ) =>
  -- | Alignment function (e.g., 'alignLsbFirst'). Can also be used to dealign.
  AlignmentFn n ->
  -- | Freeze offset signal (i.e., keep the current offset). Note: it takes one
  -- cycle to flush the state of this circuit, so if you don't want to see garbage
  -- data, you should make sure the offset signal has been stable for at least
  -- two cycles before freezing it.
  Signal dom Bool ->
  -- | Offset
  Signal dom (Index n) ->
  -- | Data
  Signal dom (Bytes n) ->
  -- | Aligned data, according to alignment data saved in the last cycle
  Signal dom (Bytes n)
aligner alignFn =
  withEnable enableGen
    $ withReset noReset
    $ curry3 (mealy go State{prev = 0, offset = 0} . bundle)
 where
  go :: State n -> (Bool, Index n, Bytes n) -> (State n, Bytes n)
  go (State{prev, offset}) (freeze, suggestedOffset, current) =
    ( State{prev = current, offset = newOffset}
    , alignFn offset prev current
    )
   where
    newOffset = if freeze then offset else suggestedOffset

-- | (De-)alignment function that can be used in 'aligner'
type AlignmentFn n =
  (KnownNat n) =>
  -- | Offset
  Index n ->
  -- | \"Old\" data
  Bytes n ->
  -- | \"New\" data
  Bytes n ->
  -- | (De-)aligned data
  Bytes n

-- | De-align bytes for to emulate LSB-first transmission
dealignLsbFirst :: AlignmentFn n
dealignLsbFirst offset old new = takeLsbs $ shiftBytesR (new ++# old) offset

-- | Align bytes for LSB-first transmission
alignLsbFirst :: AlignmentFn n
alignLsbFirst offset old new = takeMsbs $ shiftBytesL (new ++# old) offset

-- | De-align bytes for to emulate MSB-first transmission
dealignMsbFirst :: AlignmentFn n
dealignMsbFirst offset old new = takeLsbs $ shiftBytesR (old ++# new) offset

-- | Align bytes for MSB-first transmission
alignMsbFirst :: AlignmentFn n
alignMsbFirst offset old new = takeMsbs $ shiftBytesL (old ++# new) offset

-- | Like 'shiftR', but for 'Bytes'
shiftBytesR :: forall n. (KnownNat n) => Bytes (2 * n) -> Index n -> Bytes (2 * n)
shiftBytesR bv n = shiftR bv (8 * fromIntegral n)

-- | Like 'shiftL', but for 'Bytes'
shiftBytesL :: forall n. (KnownNat n) => Bytes (2 * n) -> Index n -> Bytes (2 * n)
shiftBytesL bv n = shiftL bv (8 * fromIntegral n)

-- | Take upper bits of given 'BitVector'
takeMsbs :: forall n. (KnownNat n) => Bytes (2 * n) -> Bytes n
takeMsbs = fst . unpack @(Bytes n, Bytes n)

-- | Take lower bits of given 'BitVector'
takeLsbs :: forall n. (KnownNat n) => Bytes (2 * n) -> Bytes n
takeLsbs = truncateB
