-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

-- | Clock controller types and some constants/defaults.
module Bittide.ClockControl (
  FINC,
  FDEC,
  RelDataCount,
  SpeedChange (..),
  sign,
  speedChangeToFincFdec,
  speedChangeToPins,
  targetDataCount,
)
where

import Clash.Explicit.Prelude hiding (PeriodToCycles)

import Bittide.Arithmetic.Time (PeriodToCycles)
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

data ToFincFdecState dom
  = Wait (Index (PeriodToCycles dom (Microseconds 1)))
  | Pulse (Index (PeriodToCycles dom (Nanoseconds 100))) SpeedChange
  | Idle
  deriving (Generic, NFDataX)

type FINC = Bool
type FDEC = Bool

{- | Convert 'SpeedChange' to a pair of (FINC, FDEC). This is currently hardcoded
to work on the Si5395 constraints:

  * Minimum Pulse Width: 100 ns
  * Update Rate: 1 us

TODO: De-hardcode
-}
speedChangeToFincFdec ::
  forall dom.
  (KnownDomain dom) =>
  Clock dom ->
  Reset dom ->
  Signal dom SpeedChange ->
  Signal dom (FINC, FDEC)
speedChangeToFincFdec clk rst =
  dflipflop clk . fmap speedChangeToPins . mealy clk rst enableGen go (Wait maxBound)
 where
  go :: ToFincFdecState dom -> SpeedChange -> (ToFincFdecState dom, SpeedChange)
  go (Wait n) _s
    | n == 0 = (Idle, NoChange)
    | otherwise = (Wait (n - 1), NoChange)
  go (Pulse n s) _s
    | n == 0 = (Wait maxBound, s)
    | otherwise = (Pulse (n - 1) s, s)
  go Idle NoChange = (Idle, NoChange)
  go Idle s = (Pulse maxBound s, NoChange)

speedChangeToPins :: SpeedChange -> (FINC, FDEC)
speedChangeToPins = \case
  SpeedUp -> (True, False)
  SlowDown -> (False, True)
  NoChange -> (False, False)
