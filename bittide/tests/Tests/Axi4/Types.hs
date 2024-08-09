-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Tests.Axi4.Types where

import Clash.Prelude

import Bittide.Extra.Maybe
import Clash.Sized.Vector (unsafeFromList)
import Data.Maybe
import Protocols.Axi4.Stream

import qualified Data.List as L

type Packet = [Unsigned 8]

-- | Bytes in Axi4StreamM2S transfer
data AxiByte
  = Null
  | Data (Unsigned 8)
  | Position (Unsigned 8)
  deriving (Generic, NFDataX, Show, Eq)

-- | Different types of bytes in Axi4StreamM2S transfer
data AxiByteType
  = NullByte
  | DataByte
  | PositionByte
  deriving (Generic, NFDataX, Show, Eq)

type Keep = Bool
type Strobe = Bool

-- | Get a list of bytes for a given Axi4StreamM2S transaction
getTransferBytes :: Axi4StreamM2S conf a -> Vec (DataWidth conf) AxiByte
getTransferBytes Axi4StreamM2S{..} = zipWith3 getByte _tkeep _tstrb _tdata

-- | Get a byte based on a keep, strobe and data value
getByte :: Keep -> Strobe -> Unsigned 8 -> AxiByte
getByte True True d = Data d
getByte True False p = Position p
getByte False False _ = Null
getByte False True _ = deepErrorX "Reserved byte"

{- | Get a list of byte types from an Axi4StreamM2S packet.
Input must satisfy `isSinglePacket`
-}
getPacketByteTypes :: [Axi4StreamM2S conf a] -> [AxiByteType]
getPacketByteTypes = L.concatMap (toList . fmap getByteType . getTransferBytes)

-- | Get the byte type based on a keep and strobe value
getByteType :: AxiByte -> AxiByteType
getByteType (Data _) = DataByte
getByteType (Position _) = PositionByte
getByteType Null = NullByte

-- | Get the keep and strobe values based on a byte type
getKeepStrobe :: AxiByteType -> (Keep, Strobe)
getKeepStrobe NullByte = (False, False)
getKeepStrobe DataByte = (True, True)
getKeepStrobe PositionByte = (True, False)

-- | Transform a list of Axi Stream operations into a 'Packet'.
axiStreamToPackets ::
  (KnownNat nBytes) =>
  [Axi4StreamM2S ('Axi4StreamConfig nBytes 0 0) ()] ->
  [Packet]
axiStreamToPackets = L.reverse . snd . L.foldl go ([], [])
 where
  go (partialPacket, packets) Axi4StreamM2S{..}
    | _tlast = ([], L.reverse newPartial : packets)
    | otherwise = (newPartial, packets)
   where
    newPartial = L.reverse (catMaybes (toList $ orNothing <$> _tkeep <*> _tdata)) <> partialPacket

-- Transform a 'Packet' into a list of Axi Stream operations.
packetToAxiStream ::
  forall nBytes.
  SNat nBytes ->
  Packet ->
  [Maybe (Axi4StreamM2S ('Axi4StreamConfig nBytes 0 0) ())]
packetToAxiStream w@SNat !bs
  | rest /= [] = Just axis : packetToAxiStream w rest
  | otherwise = [Just axis]
 where
  busWidth = natToNum @nBytes
  (firstWords, rest) = L.splitAt busWidth bs
  word = L.take busWidth (firstWords <> L.repeat 0)
  axis =
    Axi4StreamM2S
      { _tdata = unsafeFromList word
      , _tkeep = keeps
      , _tstrb = repeat True
      , _tlast = null rest
      , _tid = 0
      , _tdest = 0
      , _tuser = deepErrorX ""
      }
  keeps =
    unsafeFromList
      $ L.replicate (L.length bs) True
      <> L.repeat False

-- | Separate a list of Axi4Stream operations into a list of Axi4Stream packets.
separatePackets ::
  forall nBytes.
  (KnownNat nBytes) =>
  [Axi4StreamM2S ('Axi4StreamConfig nBytes 0 0) ()] ->
  [ Either
      [Axi4StreamM2S ('Axi4StreamConfig nBytes 0 0) ()]
      [Axi4StreamM2S ('Axi4StreamConfig nBytes 0 0) ()]
  ]
separatePackets [] = []
separatePackets axis = case L.break _tlast axis of
  (payload, l : rest) -> Right (payload <> [l]) : separatePackets rest
  (remainder, _) -> [Left remainder]
