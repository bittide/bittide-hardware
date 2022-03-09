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
  (HiddenClockResetEnable dom, KnownNat memDepth, NFDataX a) =>
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
doubleBufferedRAMByteAddressable ::
 ( KnownNat depth,  HiddenClockResetEnable dom, KnownNat upperByteWidth, KnownNat extraBytes
 , BitPack a, BitSize a ~ (upperByteWidth + (extraBytes * 8))) =>
  -- | The initial contents of the first buffer. The second buffer is undefined.
  Vec depth a ->
  -- | Indicates when a new metacycle has started.
  Signal dom Bool ->
  -- | Read address.
  Signal dom (Index depth) ->
  -- | Incoming data frame.
  Signal dom (WriteAny depth a) ->
  -- | One hot byte select for writing only
  Signal dom (ByteEnable (extraBytes + 1)) ->
  -- | Outgoing data
  Signal dom a
doubleBufferedRAMByteAddressable initialContent switch readAddr writeFrame byteSelect = output
 where
    outputSelect  = register False readSelect
    readSelect    = mux switch (not <$> outputSelect) outputSelect
    writeSelect   = not <$> readSelect

    writeEntries bufSelect frame = if bufSelect then (Nothing, frame) else (frame, Nothing)
    (newEntry0, newEntry1) = unbundle (writeEntries <$> writeSelect <*> writeFrame)
    buffer0       = blockRamByteAddressable initialContent readAddr newEntry0 byteSelect
    buffer1       = blockRamByteAddressable initialContent readAddr newEntry1 byteSelect

    output = mux outputSelect buffer1 buffer0

blockRamByteAddressable ::
  ( HiddenClockResetEnable dom, KnownNat depth, KnownNat bytes, bytes ~ (extraBytes + 1), KnownNat upperByteWidth
  , BitPack a, BitSize a ~ (upperByteWidth + (extraBytes * 8))) =>
  Vec depth a ->
  Signal dom (Index depth) ->
  Signal dom (WriteAny depth a) ->
  Signal dom (ByteEnable bytes) ->
  Signal dom a
blockRamByteAddressable initRAM readAddr newEntry byteSelect = (\u l -> unpack $ u ++# pack l)  <$> upperByte <*> bundle lowerBytes
 where
   (upperInitBytes, lowerInitBytes) = fmap transpose . unzip $ (fmap unpack . split . pack) <$> initRAM

   (upperEntry, lowerEntries) = unbundle $ splitWriteInBytes <$> newEntry <*> byteSelect
   upperByte  = blockRam upperInitBytes readAddr upperEntry
   lowerBytes = (`blockRam` readAddr) <$> lowerInitBytes <*> (unbundle lowerEntries)

splitWriteInBytes ::
  (BitSize writeData ~ (upperByteWidth + (extraBytes * 8)), KnownNat bytes, bytes ~ (extraBytes + 1), KnownNat upperByteWidth, BitPack writeData) =>
  WriteAny maxIndex writeData ->
  ByteEnable (1 + extraBytes) ->
  (WriteBits maxIndex upperByteWidth, Vec extraBytes (WriteByte maxIndex))
splitWriteInBytes (Just (addr, writeData)) byteSelect = (upperWrite, lowerWrites)
 where
   (upperByte, lowerBytes) = split $ pack writeData
   (upperBool :> lowerBools) = unpack byteSelect
   lowerWrites =   (\ (b,bv') -> if b then Just (addr, bv') else Nothing) <$> zip lowerBools (unpack lowerBytes)
   upperWrite = if upperBool then Just (addr, upperByte) else Nothing

splitWriteInBytes Nothing _ = (Nothing, repeat Nothing)
