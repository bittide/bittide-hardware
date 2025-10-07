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
