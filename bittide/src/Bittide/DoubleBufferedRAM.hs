{-|
Copyright:           Copyright © 2022, Google LLC
License:             Apache-2.0
Maintainer:          devops@qbaylogic.com
|-}
module Bittide.DoubleBufferedRAM where

import Clash.Prelude

import Bittide.SharedTypes
-- | The double buffered RAM component is a memory component that internally uses a single
-- blockram, but enables the user to write to one part of the ram and read from another.
-- When the metacycle indicate (the first argument) is True, the read buffer and write buffer
-- are swapped. This signal should be True for the first cycle of every metacycle.
doubleBufferedRAM ::
  forall dom memDepth a .
  (HiddenClockResetEnable dom, KnownNat memDepth, 1 <= memDepth, NFDataX a) =>
  -- | The initial contents of the first buffer. The second buffer is undefined.
  Vec memDepth a ->
  -- | Indicates when a new metacycle has started.
  Signal dom Bool ->
  -- | Read address.
  Signal dom (Index memDepth) ->
  -- | Incoming data frame.
  Signal dom (Maybe (Index memDepth, a)) ->
  -- | Outgoing data
  Signal dom a
doubleBufferedRAM initialContent switch readAddr writeFrame = output
  where
    outputSelect = register False readSelect
    readSelect = mux switch (not <$> outputSelect) outputSelect
    writeSelect = not <$> readSelect

    writeEntries bufSelect frame
      | bufSelect = (Nothing, frame)
      | otherwise = (frame, Nothing)
    (newEntry0, newEntry1) = unbundle (writeEntries <$> writeSelect <*> writeFrame)
    buffer0 = blockRam initialContent readAddr newEntry0
    buffer1 = blockRam initialContent readAddr newEntry1

    output = mux outputSelect buffer1 buffer0

-- | The byte adressable doublebuffered RAM component is a memory component that has a memory width which is a multiple of 8 bits.
-- It contains a blockram per byte and uses the one hot byte select signal to determine which bytes will be written to the blockram.
-- This component is double buffered such that it returns the read data from both buffers in a tuple where the first element
-- contains the read data from the "active" buffer, and the second element contains the read data from the "inactive" buffer.
-- Writing to this component will always write to the inactive buffer.
doubleBufferedRAMByteAddressabe :: forall dom memDepth bytes .
 (KnownNat bytes, KnownNat memDepth,  HiddenClockResetEnable dom) =>
  -- | The initial contents of the first buffer. The second buffer is undefined.
  Vec memDepth (BitVector (bytes*8)) ->
  -- | Indicates when a new metacycle has started.
  Signal dom Bool ->
  -- | Read address.
  Signal dom (Index memDepth) ->
  -- | Incoming data frame.
  Signal dom (Maybe (Index memDepth, BitVector (bytes*8))) ->
  -- | One hot byte select for writing only
  Signal dom (BitVector bytes) ->
  -- | Outgoing data
  Signal dom (BitVector (bytes*8), BitVector (bytes*8))
doubleBufferedRAMByteAddressabe initialContent switch readAddr writeFrame byteSelect = output
 where
    outputSelect  = register @dom False readSelect
    readSelect    = mux switch (not <$> outputSelect) outputSelect
    writeSelect   = not <$> readSelect

    writeEntries bufSelect frame = if bufSelect then (Nothing, frame) else (frame, Nothing)
    (newEntry0, newEntry1) = unbundle (writeEntries <$> writeSelect <*> writeFrame)
    buffer0       = blockRamByteAddressable initialContent readAddr newEntry0 byteSelect
    buffer1       = blockRamByteAddressable initialContent readAddr newEntry1 byteSelect

    output = mux outputSelect (bundle (buffer1,buffer0)) (bundle (buffer0,buffer1))

blockRamByteAddressable :: forall dom depth bytes .
  (HiddenClockResetEnable dom, KnownNat depth, KnownNat bytes) =>
  Vec depth (BitVector (bytes*8)) ->
  Signal dom (Index depth) ->
  Signal dom (Maybe (Index depth, BitVector (bytes*8))) ->
  Signal dom (BitVector bytes) ->
  Signal dom (BitVector (bytes*8))
blockRamByteAddressable initRAM readAddr newEntry byteSelect = pack <$> bundle outBytes
 where
   initBytes = transpose $ unpack @(Vec bytes (BitVector 8)) . pack <$> initRAM
   newEntries = unbundle $ splitWrites <$> newEntry <*> byteSelect
   outBytes = (`blockRam` readAddr) <$> initBytes <*>  newEntries

splitWrites :: forall bytes maxIndex .
  (KnownNat bytes, KnownNat maxIndex) =>
  WriteBytes maxIndex bytes ->
  ByteEnable bytes ->
  Vec bytes (WriteBytes maxIndex 1)
splitWrites (Just (addr, bv)) byteSelect = writes
 where
   bvs = unpack bv :: Vec bytes (BitVector 8)
   bools = unpack byteSelect :: Vec bytes Bool
   writes =   (\ (b,bv') -> if b then Just (addr, bv') else Nothing) <$> zip bools bvs

splitWrites Nothing _ = repeat Nothing
