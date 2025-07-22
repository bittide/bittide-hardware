-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.PacketStream (timeout) where

import Clash.Prelude
import Protocols
import Protocols.PacketStream (
  PacketStream,
  PacketStreamM2S (_last),
  PacketStreamS2M (PacketStreamS2M, _ready),
 )

import qualified Clash.Explicit.Prelude as E

data TimeoutState n
  = WaitingForPacket
  | InPacket (Index (n + 1))
  | InTimeout
  deriving (Show, Generic, NFDataX)

{- | Timeout circuit for a packet stream. It enforces that the transfers in a
packet don't take \"too long\". If the manager doesn't send a transfer within
the specified number of cycles, the circuit will inject a user-supplied
transfer to signal a timeout.
-}
timeout ::
  forall nCycles n meta dom.
  ( HiddenClock dom
  , HiddenReset dom
  ) =>
  -- | Number of allowed empty cycles before a timeout occurs
  SNat nCycles ->
  -- | Transfer to send on timeout
  PacketStreamM2S n meta ->
  Circuit
    (PacketStream dom n meta)
    (PacketStream dom n meta)
timeout SNat timeoutM2S =
  Circuit (unbundle . E.mealy hasClock hasReset enableGen goMealy WaitingForPacket . bundle)
 where
  goMealy ::
    TimeoutState nCycles ->
    (Maybe (PacketStreamM2S n meta), PacketStreamS2M) ->
    ( TimeoutState nCycles
    , (PacketStreamS2M, Maybe (PacketStreamM2S n meta))
    )
  -- If we've reached the timeout state, we need to wait until the subordinate
  -- acknowledges the 'timeoutM2S' transfer. This has priority over any other
  -- state, hence it going first.
  goMealy InTimeout (_, s2m) =
    ( case (s2m._ready, timeoutM2S._last) of
        (False, _) -> InTimeout -- no change
        (True, Just _) -> WaitingForPacket -- end of packet
        (True, Nothing) -> InPacket maxBound -- no end of packet, wait for timeout
    , (PacketStreamS2M False, Just timeoutM2S)
    )
  -- If we're not in a timeout state, any incoming transfer will take precedence
  -- over timeout detection. Depending on whether the incoming transfer ends the
  -- packet, we transition to either 'WaitingForPacket' or 'InPacket'. Note that
  -- this means that we only timeout if we are waiting for a transfer -- if the
  -- subordinate keeps stalling, that doesn't trigger a timeout.
  goMealy _ (Just m2s, s2m)
    | Just _ <- m2s._last = (WaitingForPacket, (s2m, Just m2s))
    | otherwise = (InPacket maxBound, (s2m, Just m2s))
  -- If we're waiting for a packet and no incoming transfer is present, we stay
  -- in 'WaitingForPacket'. I.e., timeout detection only applies to active packets.
  goMealy WaitingForPacket (Nothing, s2m) = (WaitingForPacket, (s2m, Nothing))
  -- In a packet, no transfer, and timeout reached.
  goMealy (InPacket 0) (_m2s, _s2m) =
    (InTimeout, (PacketStreamS2M False, Nothing))
  -- In a packet, no transfer, and timeout not reached.
  goMealy (InPacket n) (Nothing, s2m) =
    (InPacket (n - 1), (s2m, Nothing))
