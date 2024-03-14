{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE FlexibleContexts #-}
module Protocols.PacketStream.Axi4 where

import Clash.Prelude
import Protocols
import Protocols.Axi4.Stream
import Protocols.PacketStream
import Data.Maybe
import Protocols.Avalon.MemMap (AvalonMmSharedConfig(dataWidth))
import Bittide.Axi4 (axi4MergeLast)

type Abort = Bool

-- | Converts PacketStream to Axi4Stream with an Abort signal on _tuser
psToAxi4Stream ::
  forall dom dataWidth .
  (KnownNat dataWidth) =>
  Circuit (PacketStream dom dataWidth ()) (Axi4Stream dom ('Axi4StreamConfig dataWidth 0 0) Abort)
psToAxi4Stream = Circuit $ unbundle . fmap go . bundle
 where
  go (Nothing, Axi4StreamS2M rdy) = (PacketStreamS2M rdy, Nothing)
  go (Just psM2S, Axi4StreamS2M rdy) = (PacketStreamS2M rdy , Just Axi4StreamM2S{..})
   where
    _tdata = fmap unpack psM2S._data
    _tuser = psM2S._abort
    _tlast = isJust psM2S._last
    _tkeep = maybe (repeat True) genKeep psM2S._last
    _tstrb = repeat True
    _tid = 0
    _tdest = 0

-- | Converts Axi4Stream with an Abort signal on _tuser to PacketStream.
-- Sparse transactions are not supported
psFromAxi4Stream ::
  forall dom dataWidth .
  (HiddenClockResetEnable dom, KnownNat dataWidth) =>
  Circuit (Axi4Stream dom ('Axi4StreamConfig dataWidth 0 0) Abort) (PacketStream dom dataWidth ())
psFromAxi4Stream = axi4MergeLast |> Circuit (unbundle . fmap go . bundle)
  where
    go (Nothing, _) = (deepErrorX "", Nothing)
    go (Just Axi4StreamM2S{..}, PacketStreamS2M{..}) = (Axi4StreamS2M _ready, Just PacketStreamM2S{..})
     where
      _data = fmap pack _tdata

      -- Nothing indicates an unspported sparse transaction
      validBytes :: Maybe (Index dataWidth)
      validBytes = elemIndex _tkeep keepVecs

      _last = if _tlast then Just (fromMaybe 0 validBytes) else Nothing
      _meta = ()
      _abort = _tuser || isNothing validBytes


{- | Generates a keep vector for the AXI4-Stream interface with the first (n + 1) bits set to True
>>> genKeep 3 :: Vec 5 Bool
True :> True :> True :> True :> False :> Nil
>>> genKeep 0 :: Vec 5 Bool
True :> False :> False :> False :> False :> Nil
>>> genKeep 4 :: Vec 5 Bool
True :> True :> True :> True :> True :> Nil
>>> genKeep minBound :: Vec 5 Bool
True :> False :> False :> False :> False :> Nil
>>> genKeep maxBound :: Vec 5 Bool
True :> True :> True :> True :> True :> Nil
-}
genKeep :: forall n . KnownNat n => Index n -> Vec n Bool
genKeep n = vec
 where
  bv = pack ((complement 0, 0) :: (BitVector n, BitVector n))
  shiftedBV = resize $ shiftR bv (fromIntegral n)
  vec = unpack shiftedBV

-- | Generates a vector of vectors where each next vector has one more True than the previous one
-- The first entry contains a vector that starts with one True and the rest False.
keepVecs :: KnownNat n => Vec n (Vec n Bool)
keepVecs = drop d1 $ iterateI (True +>>) (repeat False)
