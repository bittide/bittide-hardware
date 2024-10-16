-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans -fconstraint-solver-iterations=10 #-}

-- | Clock controller types and some constants/defaults.
module Bittide.ClockControl (
  ClockControlConfig (..),
  FINC,
  FDEC,
  RelDataCount,
  SettlePeriod,
  SpeedChange (..),
  Si539xHoldTime,
  clockPeriodFs,
  defClockConfig,
  settleCycles,
  sign,
  speedChangeToFincFdec,
  speedChangeToPins,
  speedChangeToStickyPins,
  stickyBits,
  targetDataCount,
)
where

import Clash.Explicit.Prelude hiding (PeriodToCycles)
import Clash.Signal.Internal (Femtoseconds (..))
import Data.Aeson (ToJSON (toJSON))
import Data.Csv
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import GHC.Stack (HasCallStack)

import Bittide.Arithmetic.Time (PeriodToCycles, microseconds)

type SettlePeriod = Femtoseconds

-- | Configuration passed to 'clockControl'
data ClockControlConfig dom n m c = ClockControlConfig
  { cccSettleCycles :: Unsigned 32
  -- ^  Like 'cccPessimisticPeriod', but expressed as number of cycles.
  --
  -- TODO: Should be removed, it follows from other fields + domain.
  , cccSettlePeriod :: Femtoseconds
  -- ^ Period it takes for a clock frequency request to settle. This is not
  -- modelled, but an error is thrown if a request is submitted more often than
  -- this. 'clockControl' should therefore not request changes more often.
  --
  -- This is a PLL property.
  , cccBufferSize :: SNat n
  -- ^ Size of elastic buffers. Used to observe bounds and 'targetDataCount'.
  , cccStabilityCheckerMargin :: SNat m
  -- ^ Bound on the number of elements the elastic buffer is allowed
  -- to deviate from while still being considered "stable".
  , cccStabilityCheckerFramesize :: SNat c
  -- ^ The minimum number of clock cycles an elastic buffer must
  -- remain within the @cccStabilityCheckerMargin@ to be considered
  -- "stable".
  , cccEnableReframing :: Bool
  -- ^ Enable reframing. Reframing allows a system to resettle buffers around
  -- their midpoints, without dropping any frames. For more information, see
  -- [arXiv:2303.11467](https://arxiv.org/abs/2303.11467).
  , cccReframingWaitTime :: Unsigned 32
  -- ^ Number of pessimistic settle cycles to wait until reframing
  -- takes place after stability has been detected, as it is used by
  -- the "detect, store, and wait" reframing approach
  }
  deriving (Lift)

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
  deriving (Eq, Show, Generic, BitPack, ShowX, NFDataX)

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
  Signal dom (Bool, Bool)
speedChangeToFincFdec clk rst =
  dflipflop clk . fmap conv . mealy clk rst enableGen go (Wait maxBound)
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

  --               FINC   FDEC
  conv NoChange = (False, False)
  conv SpeedUp = (True, False)
  conv SlowDown = (False, True)

type FINC = Bool
type FDEC = Bool

speedChangeToPins :: SpeedChange -> (FINC, FDEC)
speedChangeToPins = \case
  SpeedUp -> (True, False)
  SlowDown -> (False, True)
  NoChange -> (False, False)

instance ToField SpeedChange where
  toField SpeedUp = "speedUp"
  toField SlowDown = "slowDown"
  toField NoChange = "noChange"

instance (KnownNat n) => ToField (Unsigned n) where
  toField = toField . toInteger

instance (KnownNat n) => ToJSON (Unsigned n) where
  toJSON = toJSON . toInteger

instance (KnownNat n) => ToField (Signed n) where
  toField = toField . toInteger

instance (KnownNat n) => ToJSON (Signed n) where
  toJSON = toJSON . toInteger

instance ToField Femtoseconds where
  toField (Femtoseconds fs) = toField fs

instance ToJSON Femtoseconds where
  toJSON (Femtoseconds fs) = toJSON fs

clockPeriodFs :: forall dom. (KnownDomain dom) => Proxy dom -> Femtoseconds
clockPeriodFs Proxy = Femtoseconds (1000 * snatToNum (clockPeriod @dom))

defClockConfig :: forall dom. (KnownDomain dom) => ClockControlConfig dom 12 8 1500000
defClockConfig =
  ClockControlConfig
    { cccSettleCycles = settleCycles self
    , cccSettlePeriod = microseconds 1
    , cccBufferSize = d12 -- 2**12 ~ 4096
    , cccStabilityCheckerMargin = SNat
    , cccStabilityCheckerFramesize = SNat
    , cccEnableReframing = True
    , cccReframingWaitTime = 160000
    }
 where
  self = defClockConfig @dom

{- | Number of cycles to wait on a given clock frequency and clock settings in
order for the settle period to pass.
-}
settleCycles ::
  forall dom n m c.
  ( HasCallStack
  , KnownDomain dom
  ) =>
  ClockControlConfig dom n m c ->
  -- | It would take a 10 GHz clock only a 10_000 cycles to wait 1 µs. This can be
  -- met by an @Unsigned 14@: @2^14 ~ 16384@. To massively overkill it we bump it
  -- up to 32 bits.
  Unsigned 32
settleCycles ClockControlConfig{cccSettlePeriod} =
  checkedFromIntegral nCycles
 where
  nCycles = (settlePeriod `div` period) + 1
  Femtoseconds settlePeriod = cccSettlePeriod
  Femtoseconds period = clockPeriodFs @dom Proxy

{- | Holds any @a@ which has any bits set for @stickyCycles@ clock cycles.
On receiving a new @a@ with non-zero bits, it sets the new incoming value as it output
and holds it for @stickyCycles@ clock cycles.
-}
stickyBits ::
  forall dom stickyCycles a.
  ( KnownDomain dom
  , NFDataX a
  , BitPack a
  , 1 <= stickyCycles
  ) =>
  Clock dom ->
  Reset dom ->
  Enable dom ->
  SNat stickyCycles ->
  Signal dom a ->
  Signal dom a
stickyBits clk rst ena SNat = mealy clk rst ena go (0, unpack 0)
 where
  go :: (Index stickyCycles, a) -> a -> ((Index stickyCycles, a), a)
  go (count, storedBits) incomingBits = ((nextCount, nextStored), storedBits)
   where
    newIncoming = pack incomingBits /= 0
    predCount = satPred SatZero count
    holdingBits = count /= 0
    (nextStored, nextCount)
      | newIncoming = (incomingBits, maxBound)
      | holdingBits = (storedBits, predCount)
      | otherwise = (unpack 0, predCount)

{- | The minimum hold time for FINC/FDEC pulses for the Si539* boards Bittide works
with is specified as 100ns. An additional 50ns is included for margin of error.
-}
type Si539xHoldTime = Nanoseconds 150

{- | Takes the clock modification from a Callisto clock control implementation and
converts it into clock control pin signals stickied for a specified hold time.
-}
speedChangeToStickyPins ::
  forall dom prd.
  ( KnownDomain dom
  , KnownNat prd
  , 1 <= PeriodToCycles dom prd
  ) =>
  Clock dom ->
  Reset dom ->
  Enable dom ->
  SNat prd ->
  Signal dom (Maybe SpeedChange) ->
  Signal dom (Bool, Bool)
speedChangeToStickyPins clk rst ena SNat msc = speedChangeToPins <$> stickySC
 where
  sc = fromMaybe NoChange <$> msc
  stickySC = stickyBits clk rst ena (SNat @(PeriodToCycles dom prd)) sc
