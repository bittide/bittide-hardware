-- SPDX-FileCopyrightText: 2022 Google LLC
-- SPDX-FileCopyrightText: 2020 Christiaan Baaij
--
-- SPDX-License-Identifier: Apache-2.0

{-|
See: http://cdn.opencores.org/downloads/wbspec_b4.pdf
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE PatternSynonyms  #-}

module Contranomy.Wishbone where

import           Clash.Prelude
import           Clash.Signal.Internal
import qualified Data.IntMap                 as I

import           Contranomy.Core.SharedTypes (AddressWidth, Bytes)
import           Text.Printf                 (printf)

data WishboneM2S nBytes addrW
  = WishboneM2S
  { -- | ADR
    addr                :: "ADR" ::: BitVector addrW
    -- | DAT
  , writeData           :: "DAT_MOSI" ::: BitVector (8 * nBytes)
    -- | SEL
  , busSelect           :: "SEL" ::: BitVector nBytes
    -- | CYC
  , busCycle            :: "CYC" ::: Bool
    -- | STB
  , strobe              :: "STB" ::: Bool
    -- | WE
  , writeEnable         :: "WE" ::: Bool
    -- | CTI
  , cycleTypeIdentifier :: "CTI" ::: CycleTypeIdentifier
    -- | BTE
  , burstTypeExtension  :: "BTE" ::: BurstTypeExtension
  } deriving (Generic, NFDataX, Show, Eq, ShowX)

data WishboneS2M nBytes
  = WishboneS2M
  { -- | DAT
    readData    :: "DAT_MISO" ::: BitVector (8 * nBytes)
    -- | ACK
  , acknowledge :: "ACK" ::: Bool
    -- | ERR
  , err         :: "ERR" ::: Bool
  } deriving (Generic, NFDataX, Show, Eq, ShowX)

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
wishboneM2S ::
  ( KnownNat nBytes
  , KnownNat addrW) =>
  WishboneM2S nBytes addrW
wishboneM2S
  = WishboneM2S
  { addr = deepErrorX "wishboneM2S: addr undefined."
  , writeData = deepErrorX "wishboneM2S: writeData undefined."
  , busSelect = deepErrorX "wishboneM2S: busSelect undefined."
  , busCycle = False
  , strobe = False
  , writeEnable = False
  , cycleTypeIdentifier = Classic
  , burstTypeExtension = LinearBurst
  }

wishboneS2M ::
  KnownNat nBytes =>
  WishboneS2M nBytes
wishboneS2M
  = WishboneS2M
  { readData = deepErrorX "wishboneM2S: readData undefined."
  , acknowledge = False
  , err = False
  }

-- | The wishbone storage is a simulation only memory element that communicates via the
-- Wishbone protocol : http://cdn.opencores.org/downloads/wbspec_b4.pdf .
-- It receives a name for error identification, an Intmap of BitVector 8 as initial content.
-- The storage is byte addressable.
wishboneStorage
  :: String
  -> I.IntMap (BitVector 8)
  -> Signal dom (WishboneM2S Bytes AddressWidth)
  -> Signal dom (WishboneS2M 4)
wishboneStorage name initial inputs = wishboneStorage' name state inputs
 where
  state = (initial, False)

wishboneStorage'
  :: String
  -> (I.IntMap (BitVector 8), Bool)
  -> Signal dom (WishboneM2S Bytes AddressWidth)
  -> Signal dom (WishboneS2M 4)
wishboneStorage' name state inputs = dataOut :- (wishboneStorage' name state' inputs')
 where
  input :- inputs' = inputs
  state' = (file', ack')
  (file, ack) = state
  WishboneM2S{ addr
  , writeData
  , busSelect
  , busCycle
  , strobe
  , writeEnable
  } = input
  file' | writeEnable = I.fromList assocList <> file
        | otherwise   = file
  ack' = busCycle && strobe
  address = fromIntegral (unpack $ addr :: Unsigned 32)
  readData = (file `lookup'` (address+3)) ++# (file `lookup'` (address+2)) ++# (file `lookup'` (address+1)) ++# (file `lookup'` address)
  lookup' x addr' =
    I.findWithDefault
      (error $ printf "%s : Uninitialized Memory Address = 0x%X" name addr')
      addr'
      x
  assocList = case busSelect of
    $(bitPattern "0001") -> [byte0]
    $(bitPattern "0010") -> [byte1]
    $(bitPattern "0100") -> [byte2]
    $(bitPattern "1000") -> [byte3]
    $(bitPattern "0011") -> half0
    $(bitPattern "1100") -> half1
    _                    -> word0
  byte0 = (address, slice d7 d0 writeData)
  byte1 = (address+1, slice d15 d8 writeData)
  byte2 = (address+2, slice d23 d16 writeData)
  byte3 = (address+3, slice d31 d24 writeData)
  half0 = [byte0, byte1]
  half1 = [byte2, byte3]
  word0  = [byte0, byte1, byte2, byte3]
  dataOut = WishboneS2M{readData = readData, acknowledge = ack, err = False}
