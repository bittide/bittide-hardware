-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# OPTIONS_GHC -fconstraint-solver-iterations=7#-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Bittide.DoubleBufferedRAM where

import Clash.Prelude

import Contranomy.Wishbone
import Data.Maybe
import Bittide.SharedTypes
-- | The double buffered RAM component is a memory component that internally uses a single
-- blockRam, but enables the user to write to one part of the ram and read from another.
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

-- | The byte addressable double buffered RAM component is a memory component that has a memory width which is a multiple of 8 bits.
-- It contains a blockRam per byte and uses the one hot byte select signal to determine which bytes will be written to the blockRam.
-- This component is double buffered such that it returns the read data from both buffers in a tuple where the first element
-- contains the read data from the "active" buffer, and the second element contains the read data from the "inactive" buffer.
-- Writing to this component will always write to the inactive buffer.
doubleBufferedRAMByteAddressable ::
  forall dom depth a .
  ( KnownNat depth, HiddenClockResetEnable dom, Paddable a, ShowX a) =>
  -- | The initial contents of the first buffer.
  Vec depth a ->
  -- | Indicates when a new metacycle has started.
  Signal dom Bool ->
  -- | Read address.
  Signal dom (Index depth) ->
  -- | Incoming data frame.
  Signal dom (Maybe (Located  depth a)) ->
  -- | One hot byte select for writing only
  Signal dom (ByteEnable (Regs a 8)) ->
  -- | Outgoing data
  Signal dom a
doubleBufferedRAMByteAddressable initialContent switch readAddr writeFrame byteSelect = output
 where
    outputSelect  = register False readSelect
    readSelect    = mux switch (not <$> outputSelect) outputSelect
    writeSelect   = not <$> readSelect

    writeEntries bufSelect frame = if bufSelect then (Nothing, frame) else (frame, Nothing)
    (newEntry0, newEntry1) = unbundle (writeEntries <$> writeSelect <*> writeFrame)
    buffer0 = blockRamByteAddressable initialContent readAddr newEntry0 byteSelect
    buffer1 = blockRamByteAddressable initialContent readAddr newEntry1 byteSelect

    output = mux outputSelect buffer1 buffer0

-- | Blockram similar to 'blockRam' with the addition that it takes a byte select signal
-- that controls which bytes at the write address are updated.
blockRamByteAddressable ::
  forall dom depth a .
  (HiddenClockResetEnable dom, KnownNat depth, Paddable a, ShowX a) =>
  Vec depth a ->
  Signal dom (Index depth) ->
  Signal dom (Maybe (Located depth a)) ->
  Signal dom (ByteEnable (Regs a 8)) ->
  Signal dom a
blockRamByteAddressable initRAM readAddr newEntry byteSelect =
   -- (\x y z-> trace (showX (x, y)) z) <$> readAddr <*> bundle writeBytes <*>
    registersToData @_ @8 . RegisterBank <$> readBytes
 where
   initBytes = transpose $ getBytes <$> initRAM
   getBytes (getRegs -> RegisterBank vec) = vec
   writeBytes = unbundle $ splitWriteInBytes <$> newEntry <*> byteSelect
   readBytes = bundle $ (`blockRam` readAddr) <$> initBytes <*> writeBytes

data RegisterWritePriority = CircuitPriority | WishbonePriority

-- | register with additional wishbone interface, this component has a configurable
-- priority that determines which value gets stored in the register during a write conflict.
-- With 'CircuitPriority', the incoming value in the fourth argument gets stored on a
-- collision and the wishbone bus gets acknowledged, but the value is silently ignored.
-- With 'WishbonePriority', the incoming wishbone write gets accepted and the value in the
-- fourth argument gets ignored.
registerWB ::
  forall dom a bs aw .
  ( HiddenClockResetEnable dom
  , Paddable a
  , KnownNat bs
  , 1 <= bs
  , KnownNat aw
  , 2 <= aw) =>
  RegisterWritePriority ->
  a ->
  Signal dom (WishboneM2S bs aw) ->
  Signal dom (Maybe a) ->
  (Signal dom a, Signal dom (WishboneS2M bs))
registerWB writePriority initVal wbIn sigIn =
  registerWBE writePriority initVal wbIn sigIn (pure maxBound)

-- | register with additional wishbone interface, this component has a configurable
-- priority that determines which value gets stored in the register during a write conflict.
-- With 'CircuitPriority', the incoming value in the fourth argument gets stored on a
-- collision and the wishbone bus gets acknowledged, but the value is silently ignored.
-- With 'WishbonePriority', the incoming wishbone write gets accepted and the value in the
-- fourth argument gets ignored. This version has an additional argument for circuit write
-- byte enables.
registerWBE ::
  forall dom a bs aw .
  ( HiddenClockResetEnable dom
  , Paddable a
  , KnownNat bs
  , 1 <= bs
  , KnownNat aw
  , 2 <= aw) =>
  -- | Determines the write priority on write collisions
  RegisterWritePriority ->
  -- | Initial value.
  a ->
  -- | Wishbone bus (master to slave)
  Signal dom (WishboneM2S bs aw) ->
  -- | New circuit value.
  Signal dom (Maybe a) ->
  -- | Explicit Byte enables for new circuit value
  Signal dom (BitVector (Regs a 8)) ->
  -- |
  -- 1. Outgoing stored value
  -- 2. Outgoing wishbone bus (slave to master)
  (Signal dom a, Signal dom (WishboneS2M bs))
registerWBE writePriority initVal wbIn sigIn sigByteEnables = (regOut, wbOut)
 where
  regOut = registerByteAddressable initVal regIn byteEnables
  (byteEnables, wbOut, regIn) = unbundle (go <$> regOut <*> sigIn <*> sigByteEnables <*> wbIn)
  go ::
    a  ->
    Maybe a ->
    BitVector (Regs a 8) ->
    WishboneM2S bs aw ->
    (BitVector (Regs a 8), WishboneS2M bs, a)
  go regOut0 sigIn0 sigbyteEnables0 WishboneM2S{..} =
    (byteEnables0, WishboneS2M{acknowledge, err, readData}, regIn0)
   where
    (alignedAddress, alignment) = split @_ @(aw - 2) @2 addr
    addressRange = maxBound :: Index (Max 1 (Regs a (bs * 8)))
    invalidAddress = (alignedAddress > resize (pack addressRange)) || not (alignment == 0)
    masterActive = strobe && busCycle
    err = masterActive && invalidAddress
    acknowledge = masterActive && not err
    wbWriting = writeEnable && acknowledge
    wbAddr = unpack . resize $ pack alignedAddress :: Index (Max 1 (Regs a (bs * 8)))
    readData = case paddedToRegisters $ Padded regOut0 of
      RegisterBank vec -> reverse vec !! wbAddr

    wbByteEnables =
      resize . pack . reverse $ replace wbAddr busSelect (repeat @(Regs a (bs*8)) 0)
    sigRegIn = fromMaybe (errorX "registerWB: sigIn is Nothing when Just is expected.") sigIn0
    wbRegIn = registersToData . RegisterBank $ repeat writeData
    (byteEnables0, regIn0) = case (writePriority, isJust sigIn0, wbWriting) of
      (CircuitPriority , True , _)     -> (sigbyteEnables0, sigRegIn)
      (CircuitPriority , False, True)  -> (wbByteEnables, wbRegIn)
      (WishbonePriority, _    , True)  -> (wbByteEnables, wbRegIn)
      (WishbonePriority, True , False) -> (sigbyteEnables0, sigRegIn)
      (_               , False, False) -> (0, errorX "registerWB: register input not defined.")

-- | Registor similar to 'register' with the addition that it takes a byte select signal
-- that controls which bytes are updated.
registerByteAddressable ::
  forall dom a .
  (HiddenClockResetEnable dom, Paddable a) =>
  a ->
  Signal dom a ->
  Signal dom (ByteEnable (Regs a 8)) ->
  Signal dom a
registerByteAddressable initVal newVal byteEnables =
  registersToData @_ @8 . RegisterBank <$> bundle regsOut
 where
  initBytes = getBytes initVal
  newBytes = unbundle $ getBytes <$> newVal
  regsOut = (`andEnable` register) <$> unbundle (unpack <$> byteEnables) <*> initBytes <*> newBytes
  getBytes (getRegs -> RegisterBank vec) = vec

-- | Takes singular write operation (Maybe (Index maxIndex, writeData)) and splits it up
-- according to a supplied byteselect bitvector into a vector of byte sized write operations
-- (Maybe (Index maxIndex, Byte)).
splitWriteInBytes ::
  forall maxIndex writeData .
  (Paddable writeData) =>
  Maybe (Located maxIndex writeData) ->
  ByteEnable (Regs writeData 8) ->
  Vec (Regs writeData 8) (Maybe (LocatedByte maxIndex))
splitWriteInBytes (Just (addr, writeData)) byteSelect =
  case paddedToRegisters $ Padded writeData of
    RegisterBank vec -> splitWrites <$> unpack byteSelect <*> vec
     where
      splitWrites :: Bool -> Byte -> Maybe (LocatedByte maxIndex)
      splitWrites b bv = if b then Just (addr, bv) else Nothing

splitWriteInBytes Nothing _ = repeat Nothing
