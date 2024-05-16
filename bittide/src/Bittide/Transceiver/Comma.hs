module Bittide.Transceiver.Comma where

import Clash.Explicit.Prelude

import Bittide.SharedTypes (Bytes, Byte)

-- | Number of cycles to generate commas for, pulled from Xilinx example code.
defCycles :: SNat 125000
defCycles = SNat

-- | Generate commas (transceiver alignment symbols) for a number of cycles
generator ::
  forall nCycles nBytes dom .
  ( KnownDomain dom
  , KnownNat nBytes
  , 1 <= nCycles ) =>
  SNat nCycles ->
  Clock dom ->
  Reset dom ->
  -- | (comma, tx control)
  ( Signal dom (Maybe (Bytes nBytes))
  , Signal dom (BitVector nBytes)
  )
generator _nCycles@SNat clk rst =
  ( mux sendCommas (pure $ Just commas) (pure Nothing)
  , mux sendCommas (pure maxBound)      (pure 0)
  )
 where
  comma :: Byte
  comma = 0xbc

  commas :: Bytes nBytes
  commas = pack (repeat comma)

  sendCommas = counter ./=. pure maxBound
  counter = register clk rst enableGen (0 :: Index nCycles) (satSucc SatBound <$> counter)
