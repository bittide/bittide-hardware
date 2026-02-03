-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Protocols.Df.Extra where

import Clash.Prelude
import Protocols

andAck :: forall dom a. Signal dom Bool -> Circuit (Df dom a) (Df dom a)
andAck extraAcks = Circuit go0
 where
  go0 :: (Signal dom (Maybe a), Signal dom Ack) -> (Signal dom Ack, Signal dom (Maybe a))
  go0 (as, acks) = (go1 <$> acks <*> extraAcks, as)

  go1 :: Ack -> Bool -> Ack
  go1 (Ack ack) extraAck = Ack (ack && extraAck)

-- | Indicates whether the downstream is ready to accept data
type Ready = Bool

data SkidState a
  = InReset
  | Empty
  | Full a
  deriving (Generic, NFDataX)

{- | A skid buffer that can store a single element and will acknowledge data if it can
store it. Also indicates when it is ready to accept data. In practice, the ready signal is
the exact same as the ack signal, but 'Df' disallows interpreting the ack signal as a
this-component-is-ready-to-accept-data signal.

This skid buffer is implemented in terms of 'moore', so it cuts up combinational paths.
-}
skid ::
  forall dom a.
  (NFDataX a, HiddenClockResetEnable dom) =>
  Circuit (Df dom a) (Df dom a, CSignal dom Ready)
skid = Circuit go
 where
  go (dataIn, (ackIn, _)) = (ackOut, (dataOut, readyOut))
   where
    (ackOut, dataOut, readyOut) = mooreB goState goOutput InReset (dataIn, ackIn)

  goState :: SkidState a -> (Maybe a, Ack) -> SkidState a
  goState InReset _ = Empty
  goState Empty (Nothing, _) = Empty
  goState Empty (Just dat, _) = Full dat
  goState s@(Full _) (_, ~(Ack ack)) = if ack then Empty else s

  goOutput :: SkidState a -> (Ack, Maybe a, Bool)
  goOutput s = let ack = isEmpty s in (Ack ack, toMaybe s, ack)

  isEmpty :: SkidState a -> Bool
  isEmpty Empty = True
  isEmpty _ = False

  toMaybe :: SkidState a -> Maybe a
  toMaybe (Full a) = Just a
  toMaybe _ = Nothing

ackWhen :: Signal dom Bool -> Circuit (Df dom a) ()
ackWhen canDrop = Circuit $ \_ -> (Ack <$> canDrop, ())
