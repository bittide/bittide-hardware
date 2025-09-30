-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Df (asciiDebugMux, wbToDf) where

import Clash.Prelude
import Protocols

import Bittide.PacketStream (timeout)
import Bittide.SharedTypes (Byte, Bytes)
import Clash.Class.BitPackC (BitPackC, ByteOrder)
import Data.Bifunctor
import Data.Char (ord)
import Data.Coerce (coerce)
import Data.Functor ((<&>))
import GHC.Stack (HasCallStack)
import Protocols.Df (CollectMode (Skip))
import Protocols.MemoryMap.FieldType (ToFieldType)
import Protocols.MemoryMap.Registers.WishboneStandard (
  BusActivity,
  registerWb,
  registerWbDf,
 )
import Protocols.PacketStream (
  PacketStream,
  PacketStreamM2S (..),
  PacketStreamS2M (..),
  packetizerC,
 )
import Protocols.PacketStream.Base (truncateAbortedPackets)
import Protocols.PacketStream.Routing (packetArbiterC)
import Protocols.Vec (vecCircuits)
import Protocols.Wishbone

import qualified Protocols.MemoryMap as MM
import qualified Protocols.MemoryMap.Registers.WishboneStandard as MM

{- | Muxes multiple 'Df' streams of bytes into a single 'Df' stream of bytes,
interleaving the streams from the different sources on newline characters. As
an example, if two streams are supplied trying to write the messages @Hello\n@
and @World\n@, respectively, the output will be:

> [LBL0] Hello
> [LBL1] World

Here, @LBL0@ and @LBL1@ are the labels supplied to the circuit. The circuit could
also output the lines in reverse, depending on the timing of the input streams.

A timeout is given as an argument and is used to determine the end of a line if a
stream does not write a new character within that time. If a stream does timeout,
a newline character is inserted. This prevents the mux from getting stuck if a
stream stops writing characters.
-}
asciiDebugMux ::
  forall timeout dom n m.
  ( HiddenClockResetEnable dom
  , KnownNat n
  , KnownNat m
  , 1 <= n
  , 1 <= m
  ) =>
  -- | Timeout after /n/ cycles. Recommended setting is 1024 cycles for CPUs.
  SNat timeout ->
  -- | Labels to insert before each message
  Vec n (Vec m Byte) ->
  Circuit
    (Vec n (Df dom Byte))
    (Df dom Byte)
asciiDebugMux sTimeout labels0 = circuit $ \dfs -> do
  packets <- timeouts <| bytesToPacketStreams -< dfs
  muxedDfs <- packetArbiterC Skip -< packets
  -- Safety: though the function below is marked unsafe, it is safe to use here,
  -- because we both construct and deconstruct the PacketStreams internally
  -- and don't generate /abort/ transfers.
  unsafeBytesFromPacketStream (labels1 !!) -< muxedDfs
 where
  bytesToPacketStreams = vecCircuits $ bytesToPacketStream (== newline) <$> indicesI @n
  timeouts = vecCircuits $ timeout sTimeout <$> breakupM2Ss
  labels1 = (\l -> openBracket :> l ++ closeBracket :> space :> Nil) <$> labels0

  breakupM2Ss :: Vec n (PacketStreamM2S 1 (Index n))
  breakupM2Ss =
    indicesI <&> \ix ->
      PacketStreamM2S
        { _data = newline :> Nil
        , _meta = ix
        , _last = Just 1
        , _abort = False
        }

  -- Characters needed to format the an output to "[${label}] ${message}\n"
  (newline, openBracket, closeBracket, space) =
    ( fromIntegral $ ord '\n'
    , fromIntegral $ ord '['
    , fromIntegral $ ord ']'
    , fromIntegral $ ord ' '
    )

{- | Convert a 'Df' stream of bytes into a packet stream of bytes. The supplied
predicate is used to determine the end of a packet. The meta information for
the packet stream is constant and is supplied as an argument.
-}
bytesToPacketStream ::
  -- | Predicate to determine packet delimiter
  (Byte -> Bool) ->
  -- | Meta information for the packet stream
  meta ->
  Circuit (Df dom Byte) (PacketStream dom 1 meta)
bytesToPacketStream predicate meta = Circuit (unbundle . fmap go . bundle)
 where
  go (m2s, s2m) = (coerce s2m, fmap toM2S m2s)

  toM2S dat =
    PacketStreamM2S
      { _data = dat :> Nil
      , _meta = meta
      , _last = if predicate dat then Just 1 else Nothing
      , _abort = False
      }

{- | Convert a packet stream of bytes into a 'Df' stream of bytes. Is marked
unsafe, because it drops transfers marked /abort/ and loses information about
packet boundaries.
-}
unsafeBytesFromPacketStream ::
  ( HiddenClockResetEnable dom
  , KnownNat n
  , 1 <= n
  ) =>
  (meta -> Vec n Byte) ->
  Circuit (PacketStream dom 1 meta) (Df dom Byte)
unsafeBytesFromPacketStream f =
  packetizerC (const ()) f |> truncateAbortedPackets |> unsafePacketStreamToDf
 where
  unsafePacketStreamToDf = Circuit (unbundle . fmap go . bundle)
   where
    go (m2s, ack)
      | Just PacketStreamM2S{_abort = True} <- m2s = (coerce True, Nothing)
      | Just PacketStreamM2S{_last = Just 0} <- m2s = (coerce True, Nothing)
      | otherwise = (coerce ack, fmap (pack . (._data)) m2s)

{- | Component to create a `Df` stream from a wishbone interface. This component
features two registers, one to accumulate the data and one to create the `Df` transaction.
-}
wbToDf ::
  forall dom a addrW nBytes.
  ( HiddenClockResetEnable dom
  , KnownNat nBytes
  , 1 <= nBytes
  , KnownNat addrW
  , Show a
  , ShowX a
  , ToFieldType a
  , NFDataX a
  , BitPack a
  , BitPackC a
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  , HasCallStack
  ) =>
  String ->
  Circuit
    (MM.ConstBwd MM.MM, Wishbone dom 'Standard addrW (Bytes nBytes))
    (Df dom a)
wbToDf name = circuit $ \(mm, wb) -> do
  [wbData, wbCommit] <- MM.deviceWb name -< (mm, wb)

  (dat, _0) <- registerWb hasClock hasReset cfgData (unpack 0) -< (wbData, Fwd noWrite)
  (_1, df) <- registerWbDf hasClock hasReset cfgCommit () -< (wbCommit, Fwd noWrite)

  replaceDfData -< (dat, df)
 where
  noWrite = pure Nothing

  -- Given a Df stream and a raw signal, replace the contents of the Df with the
  -- contents of the raw signal. Usage of this function is only valid as long as
  -- the raw signal stays stable whenever the Df stream is active. In our case
  -- this is true, as the circuit doesn't write to either register and the wishbone
  -- bus can only do one access at a time.
  replaceDfData :: Circuit (CSignal dom a, Df dom (BusActivity ())) (Df dom a)
  replaceDfData = Circuit (first unbundle . unbundle . fmap go . bundle . first bundle)
   where
    go :: ((a, Maybe (BusActivity ())), Ack) -> (((), Ack), Maybe a)
    go ((a, busActivity), ack) = (((), ack), fmap (const a) busActivity)

  cfgData =
    (MM.registerConfig "data")
      { MM.access = MM.WriteOnly
      , MM.description = "Data register for " <> name
      }

  cfgCommit =
    (MM.registerConfig "commit")
      { MM.access = MM.WriteOnly
      , MM.description = "Commit register for " <> name
      }
