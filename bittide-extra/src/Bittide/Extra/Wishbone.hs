-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- See: http://cdn.opencores.org/downloads/wbspec_b4.pdf
module Bittide.Extra.Wishbone where

import Clash.Prelude
import Clash.Signal.Internal
import qualified Data.IntMap as I

data WishboneM2S bytes addressWidth = WishboneM2S
  { -- | ADR
    addr :: "ADR" ::: BitVector addressWidth,
    -- | DAT
    writeData :: "DAT_MOSI" ::: BitVector (8 * bytes),
    -- | SEL
    busSelect :: "SEL" ::: BitVector bytes,
    -- | CYC
    busCycle :: "CYC" ::: Bool,
    -- | STB
    strobe :: "STB" ::: Bool,
    -- | WE
    writeEnable :: "WE" ::: Bool,
    -- | CTI
    cycleTypeIdentifier :: "CTI" ::: CycleTypeIdentifier,
    -- | BTE
    burstTypeExtension :: "BTE" ::: BurstTypeExtension
  }
  deriving (Generic, NFDataX, Show, Eq, ShowX)

data WishboneS2M bytes = WishboneS2M
  { -- | DAT
    readData :: "DAT_MISO" ::: BitVector (8 * bytes),
    -- | ACK
    acknowledge :: "ACK" ::: Bool,
    -- | ERR
    err :: "ERR" ::: Bool
  }
  deriving (Generic, NFDataX, Show, Eq, ShowX)

newtype CycleTypeIdentifier = CycleTypeIdentifier (BitVector 3) deriving (Generic, NFDataX, Show, Eq, ShowX)

pattern Classic, ConstantAddressBurst, IncrementingBurst, EndOfBurst :: CycleTypeIdentifier
pattern Classic = CycleTypeIdentifier 0
pattern ConstantAddressBurst = CycleTypeIdentifier 1
pattern IncrementingBurst = CycleTypeIdentifier 2
pattern EndOfBurst = CycleTypeIdentifier 7

data BurstTypeExtension
  = LinearBurst
  | Beat4Burst
  | Beat8Burst
  | Beat16Burst
  deriving (Generic, NFDataX, Show, Eq, ShowX)

emptyWishboneM2S :: (KnownNat bytes, KnownNat addressWidth) => WishboneM2S bytes addressWidth
emptyWishboneM2S =
  WishboneM2S
    { addr = deepErrorX "emptyWishboneM2S: address of idle bus is undefined.",
      writeData = deepErrorX "emptyWishboneM2S: writeData of idle bus is undefined.",
      busSelect = deepErrorX "emptyWishboneM2S: busSelect of idle bus is undefined.",
      busCycle = False,
      strobe = False,
      writeEnable = False,
      cycleTypeIdentifier = Classic,
      burstTypeExtension = LinearBurst
    }

wishboneS2M :: KnownNat bytes => WishboneS2M bytes
wishboneS2M =
  WishboneS2M
    { readData = deepErrorX "wishboneS2M: readData of idle bus is undefined.",
      acknowledge = False,
      err = False
    }

-- | The wishbone storage is a simulation only memory element that communicates via the
-- Wishbone protocol : http://cdn.opencores.org/downloads/wbspec_b4.pdf .
-- It receives a name for error identification, an Intmap of BitVector 8 as initial content.
-- The storage is byte addressable.
wishboneStorage ::
  String ->
  I.IntMap (BitVector 8) ->
  Signal dom (WishboneM2S 4 32) ->
  Signal dom (WishboneS2M 4)
wishboneStorage name initial inputs = wishboneStorage' name state inputs
  where
    state = (initial, False)

wishboneStorage' ::
  String ->
  (I.IntMap (BitVector 8), Bool) ->
  Signal dom (WishboneM2S 4 32) ->
  Signal dom (WishboneS2M 4)
wishboneStorage' name state inputs = dataOut :- (wishboneStorage' name state' inputs')
  where
    input :- inputs' = inputs
    state' = (file', ack')
    (file, ack) = state
    WishboneM2S
      { addr,
        writeData,
        busSelect,
        busCycle,
        strobe,
        writeEnable
      } = input
    file'
      | writeEnable = I.fromList assocList <> file
      | otherwise = file
    ack' = busCycle && strobe
    address = fromIntegral (unpack $ addr :: Unsigned 32)
    readData =
      if not writeEnable
        then
          (file `lookup'` (address + 3))
            ++# (file `lookup'` (address + 2))
            ++# (file `lookup'` (address + 1))
            ++# (file `lookup'` address)
        else 0
    lookup' x addr' = I.findWithDefault (error $ name <> ": Uninitialized Memory Address = " <> show addr') addr' x
    assocList = case busSelect of
      $(bitPattern "0001") -> [byte0]
      $(bitPattern "0010") -> [byte1]
      $(bitPattern "0100") -> [byte2]
      $(bitPattern "1000") -> [byte3]
      $(bitPattern "0011") -> half0
      $(bitPattern "1100") -> half1
      _ -> word0
    byte0 = (address, slice d7 d0 writeData)
    byte1 = (address + 1, slice d15 d8 writeData)
    byte2 = (address + 2, slice d23 d16 writeData)
    byte3 = (address + 3, slice d31 d24 writeData)
    half0 = [byte0, byte1]
    half1 = [byte2, byte3]
    word0 = [byte0, byte1, byte2, byte3]
    dataOut = WishboneS2M {readData = readData, acknowledge = ack, err = False}

-- | Wrapper for the wishboneStorage that allows two ports to be connected.
-- Port A can only be used for reading, port B can read and write to the te storage.
-- Writing from port A is illegal and write attempts will set the err signal.
instructionStorage ::
  String ->
  I.IntMap (BitVector 8) ->
  Signal dom (WishboneM2S 4 32) ->
  Signal dom (WishboneM2S 4 32) ->
  (Signal dom (WishboneS2M 4), Signal dom (WishboneS2M 4))
instructionStorage name initial aM2S bM2S = (aS2M, bS2M)
  where
    storageOut = wishboneStorage name initial storageIn
    aActive = strobe <$> aM2S .&&. busCycle <$> aM2S
    bActive = strobe <$> bM2S .&&. busCycle <$> bM2S
    aWriting = aActive .&&. writeEnable <$> aM2S

    storageIn = mux (not <$> bActive) (noWrite <$> aM2S) bM2S

    aS2M = mux (not <$> bActive) (writeIsErr <$> storageOut <*> aWriting) (noAck <$> storageOut)
    bS2M = storageOut

    noAck wb = wb {acknowledge = False, err = False}
    noWrite wb = wb {writeEnable = False}
    writeIsErr wb write = wb {err = err wb || write}
