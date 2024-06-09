-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Bittide.Transceiver.Comma where

import Clash.Explicit.Prelude

import Bittide.Arithmetic.Time (IndexMs)
import Bittide.SharedTypes (Bytes, Byte)
import Clash.Class.Counter (Counter(countSuccOverflow))

-- | Number of milliseconds to generate commas for
defTimeout :: SNat 1
defTimeout = SNat

-- | Generate commas (transceiver alignment symbols) for a number of milliseconds
generator ::
  forall ms nBytes dom .
  ( KnownDomain dom
  , KnownNat nBytes
  , 1 <= ms ) =>
  SNat ms ->
  Clock dom ->
  Reset dom ->
  -- | (comma, tx control)
  ( Signal dom (Maybe (Bytes nBytes))
  , Signal dom (BitVector nBytes)
  )
generator _nCycles@SNat clk rst = unbundle $
  mux
    (counter .==. pure maxBound)
    (pure (Nothing, 0))
    (pure (Just commas, maxBound))
 where
  comma :: Byte
  comma = 0xbc

  commas :: Bytes nBytes
  commas = pack (repeat comma)

  counter =
    register
      clk rst enableGen
      (0 :: Index ms, 0 :: IndexMs dom 1)
      (next <$> counter)

  next n = case countSuccOverflow n of
    (True, _) -> n
    (_, nextN) -> nextN
