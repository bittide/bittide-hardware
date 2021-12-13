{-|
Copyright  :  (C) 2020, Christiaan Baaij
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

See: http://cdn.opencores.org/downloads/wbspec_b4.pdf
-}

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Contranomy.Wishbone where

import Clash.Prelude
import qualified Data.IntMap as I
import qualified Data.List as L
import           Clash.Signal.Internal
import qualified Data.Sequence as Seq
import Debug.Trace
data WishboneM2S bytes addressWidth
  = WishboneM2S
  { -- | ADR
    addr :: "ADR" ::: BitVector addressWidth
    -- | DAT
  , writeData :: "DAT_MOSI" ::: BitVector (8 * bytes)
    -- | SEL
  , busSelect :: "SEL" ::: BitVector bytes
    -- | CYC
  , busCycle :: "CYC" ::: Bool
    -- | STB
  , strobe :: "STB" ::: Bool
    -- | WE
  , writeEnable :: "WE" ::: Bool
    -- | CTI
  , cycleTypeIdentifier :: "CTI" ::: CycleTypeIdentifier
    -- | BTE
  , burstTypeExtension :: "BTE" ::: BurstTypeExtension
  }

data WishboneS2M bytes
  = WishboneS2M
  { -- | DAT
    readData :: "DAT_MISO" ::: BitVector (8 * bytes)
    -- | ACK
  , acknowledge :: "ACK" ::: Bool
    -- | ERR
  , err :: "ERR" ::: Bool
  } deriving (Generic, NFDataX)

newtype CycleTypeIdentifier = CycleTypeIdentifier (BitVector 3)

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

wishboneM2S :: SNat bytes -> SNat addressWidth -> WishboneM2S bytes addressWidth
wishboneM2S SNat SNat
  = WishboneM2S
  { addr = undefined
  , writeData = undefined
  , busSelect = undefined
  , busCycle = False
  , strobe = False
  , writeEnable = False
  , cycleTypeIdentifier = Classic
  , burstTypeExtension = LinearBurst
  }

wishboneS2M :: SNat bytes -> WishboneS2M bytes
wishboneS2M SNat
  = WishboneS2M
  { readData = 0
  , acknowledge = False
  , err = False
  }

-- wishboneStorage size WishboneM2S{ addr
--   , writeData
--   , busSelect
--   , busCycle
--   , strobe
--   , writeEnable
--   , cycleTypeIdentifier
--   , burstTypeExtension
--   } = WishboneS2M{readData, acknowledge, err}
--     where
--       acknowledge = register False $ busCycle && strobe
--       storageArray = listArray $ replicate size 0

--wishboneStorage highestIndex init input =  wishboneStorage' ((A.listArray (0, highestIndex) $ L.replicate (highestIndex+1) init), False)
-- wishboneStorage' (file, ack) WishboneM2S{ addr
--   , writeData
--   , busSelect
--   , busCycle
--   , strobe
--   , writeEnable
--   , cycleTypeIdentifier
--   , burstTypeExtension
--   } = ((file', ack'),dataOut) where
--   file' | writeEnable = file' A.// assocList
--         | otherwise   = file
--   ack' = busCycle && strobe
--   address = unpack addr
--   readData = file' A.! address
--   assocList = case busSelect of
--     $(bitPattern "0001")  -> [byte0]
--     $(bitPattern "0010")  -> [byte1]
--     $(bitPattern "0100")  -> [byte2]
--     $(bitPattern "1000")  -> [byte3]
--     $(bitPattern "0011")  -> half0
--     $(bitPattern "1100")  -> half1
--     _                     -> word0
--   byte0 = (address, slice d7 d0 writeData)
--   byte1 = (address+1, slice d15 d8 writeData)
--   byte2 = (address+2, slice d23 d16 writeData)
--   byte3 = (address+3, slice d31 d24 writeData)
--   half0 = [byte0, byte1]
--   half1 = [byte2, byte3]
--   word0  = [byte0, byte1, byte2, byte3]
--   dataOut = WishboneS2M{readData = readData, acknowledge = ack, err = False}

-- wishboneStorage
--   :: forall a dom . (A.Ix a, Num a, BitPack a, KnownDomain dom) =>
--      a
--      -> BitVector 8
--      -> Signal dom (WishboneM2S 4 (BitSize a))
--      -> Signal dom (WishboneS2M 4)

wishboneStorage
  :: I.IntMap (BitVector 8)
  -> Signal dom (WishboneM2S 4 30)
  -> Signal dom (WishboneS2M 4)
wishboneStorage init inputs = wishboneStorage' state inputs where
  state = (init, False)

wishboneStorage'
  :: (I.IntMap (BitVector 8), Bool)
  -> Signal dom (WishboneM2S 4 30)
  -> Signal dom (WishboneS2M 4)
wishboneStorage' state inputs = dataOut :- (wishboneStorage' state' inputs')
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
  , cycleTypeIdentifier
  , burstTypeExtension
  } = input
  file' | writeEnable = I.fromList assocList <> file
        | otherwise   = file
  ack' = busCycle && strobe
  address = fromIntegral (unpack $ addr :: Unsigned 30 {- addressWidth -}) * 4
  readData = (file `lookup'` (address+3)) ++# (file `lookup'` (address+2)) ++# (file `lookup'` (address+1)) ++# (file `lookup'` address)
  lookup' x addr = I.findWithDefault (error $ "Uninitialized Memory Address: " <> show addr) addr x
  assocList = case busSelect of
    $(bitPattern "0001")  -> [byte0]
    $(bitPattern "0010")  -> [byte1]
    $(bitPattern "0100")  -> [byte2]
    $(bitPattern "1000")  -> [byte3]
    $(bitPattern "0011")  -> half0
    $(bitPattern "1100")  -> half1
    _                     -> word0
  byte0 = (address, slice d7 d0 writeData)
  byte1 = (address+1, slice d15 d8 writeData)
  byte2 = (address+2, slice d23 d16 writeData)
  byte3 = (address+3, slice d31 d24 writeData)
  half0 = [byte0, byte1]
  half1 = [byte2, byte3]
  word0  = [byte0, byte1, byte2, byte3]
  dataOut = WishboneS2M{readData = readData, acknowledge = ack, err = False}
