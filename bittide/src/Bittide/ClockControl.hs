-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

-- | Clock controller types and some constants/defaults.
module Bittide.ClockControl (
  RelDataCount,
  SpeedChange (..),
  sign,
  targetDataCount,
)
where

import Clash.Explicit.Prelude

import Clash.Class.BitPackC (BitPackC)
import Protocols.MemoryMap.TypeDescription

{- | The (virtual) type of the FIFO's data counter. Setting this to
'Unsigned' captures the real implementation of the FIFO, while
setting it to 'Signed' results in a virtual correction shifting the
FIFO's center to be always at @0@.

_(remember to also modify 'targetDataCount' below if the
representation of 'RelDataCount' gets changed.)_
-}
type RelDataCount n = Signed n

{- | The target data count within a (virtual) FIFO. It is usually set
to be at the FIFO's center.

_(recommended values are @0@ if 'RelDataCount' is 'Signed' and @shiftR
maxBound 1 + 1@ if it is 'Unsigned')_
-}
targetDataCount :: (KnownNat n) => RelDataCount n
targetDataCount = 0

-- | Safer version of FINC/FDEC signals present on the Si5395/Si5391 clock multipliers.
data SpeedChange
  = NoChange
  | SlowDown
  | SpeedUp
  deriving (Eq, Show, Generic, BitPack, ShowX, NFDataX, BitPackC)

deriveTypeDescription ''SpeedChange

{- | Converts speed changes into a normalized scalar, which reflects
their effect on clock control.
-}
sign :: (Num a) => SpeedChange -> a
sign = \case
  SpeedUp -> 1
  NoChange -> 0
  SlowDown -> -1
