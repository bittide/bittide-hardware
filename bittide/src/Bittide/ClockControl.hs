-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans -fconstraint-solver-iterations=10 #-}

-- | Clock controller types and some constants/defaults.
module Bittide.ClockControl
  ( ClockControlConfig (..)
  , DataCount
  , SettlePeriod
  , SpeedChange (..)
  , defClockConfig
  , pessimisticSettleCycles
  , targetDataCount
  , clockPeriodFs
  , speedChangeToFincFdec
  )
where

import Clash.Explicit.Prelude
import Clash.Signal.Internal (Femtoseconds(..))
import Data.Aeson (ToJSON(toJSON))
import Data.Proxy (Proxy(..))
import Foreign.C.Types (CUInt)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable(..))
import GHC.Stack (HasCallStack)

import Bittide.Arithmetic.Ppm
import Bittide.Arithmetic.Time (PeriodToCycles, Nanoseconds, Microseconds, microseconds)
import Bittide.Simulate.RustFFI.Sizes

import Data.Csv

type SettlePeriod = Femtoseconds

-- | Configuration passed to 'clockControl'
data ClockControlConfig dom n m c = ClockControlConfig
  { -- | The quickest a clock could possibly run at. Used to (pessimistically)
    -- estimate when a new command can be issued.
    --
    -- TODO: Should be removed, it follows from other fields + domain.
    --
    cccPessimisticPeriod :: Femtoseconds

    -- |  Like 'cccPessimisticPeriod', but expressed as number of cycles.
    --
    -- TODO: Should be removed, it follows from other fields + domain.
    --
  , cccPessimisticSettleCycles :: Unsigned 32

    -- | Period it takes for a clock frequency request to settle. This is not
    -- modelled, but an error is thrown if a request is submitted more often than
    -- this. 'clockControl' should therefore not request changes more often.
    --
    -- This is a PLL property.
    --
  , cccSettlePeriod :: Femtoseconds

    -- | Maximum deviation from "factory tuning". E.g., a clock tuned to 200 MHz
    -- and a maximum deviation of +- 100 ppm can produce a signal anywhere
    -- between 200 MHz +- 20 KHz.
    --
    -- This is an oscillator + PLL property.
    --
  , cccDeviation :: Ppm

    -- | Step size for frequency increments / decrements
    --
    -- This is a setting of the PLL. Note that though this is a setting, it is
    -- programmed over I2C. For the time being, we expect it to be programmed
    -- once after which only the FDEC/FINC pins will be used.
    --
    -- TODO: Should be expressed as PPM.
    --
  , cccStepSize :: Femtoseconds

    -- | Size of elastic buffers. Used to observe bounds and 'targetDataCount'.
  , cccBufferSize :: SNat n

    -- | Bound on the number of elements the elastic buffer is allowed
    -- to deviate from while still being considered "stable".
  , cccStabilityCheckerMargin :: SNat m

    -- | The minimum number of clock cycles an elastic buffer must
    -- remain within the @cccStabilityCheckerMargin@ to be considered
    -- "stable".
  , cccStabilityCheckerFramesize :: SNat c

    -- | Enable reframing. Reframing allows a system to resettle buffers around
    -- their midpoints, without dropping any frames. For more information, see
    -- [arXiv:2303.11467](https://arxiv.org/abs/2303.11467).
  , cccEnableReframing :: Bool

    -- | Number of pessimistic settle cycles to wait until reframing
    -- takes place after stability has been detected, as it is used by
    -- the "detect, store, and wait" reframing approach
  , cccReframingWaitTime :: Unsigned 32

    -- | If set, then the clock control algorithm is simulated via the Rust FFI.
  , cccEnableRustySimulation :: Bool
  } deriving (Lift)

-- | The (virtual) type of the FIFO's data counter. Setting this to
-- 'Unsigned' captures the real implementation of the FIFO, while
-- setting it to 'Signed' results in a virtual correction shifting the
-- FIFO's center to be always at @0@.
--
-- _(remember to also modify 'targetDataCount' below if the
-- representation of 'DataCount' gets changed.)_
type DataCount n = Signed n

-- | The target data count within a (virtual) FIFO. It is usually set
-- to be at the FIFO's center.
--
-- _(recommended values are @0@ if 'DataCount' is 'Signed' and @shiftR
-- maxBound 1 + 1@ if it is 'Unsigned')_
targetDataCount :: KnownNat n => DataCount n
targetDataCount = 0

-- | Safer version of FINC/FDEC signals present on the Si5395/Si5391 clock multipliers.
data SpeedChange
  = SpeedUp
  | SlowDown
  | NoChange
  deriving (Eq, Show, Generic, ShowX, NFDataX, Enum)

type instance SizeOf SpeedChange =
  SizeOf CUInt

type instance Alignment SpeedChange =
  Alignment CUInt

instance (SizeOf SpeedChange ~ 4, Alignment SpeedChange ~ 4)
  => Storable SpeedChange where
  sizeOf = const $ natToNum @(SizeOf SpeedChange)
  alignment = const $ natToNum @(Alignment SpeedChange)

  peek p = from <$> peek (castPtr p :: Ptr CUInt)
   where
    from = \case
      0 -> SpeedUp
      1 -> SlowDown
      2 -> NoChange
      _ -> error "out of range"

  poke p = poke (castPtr p :: Ptr CUInt) . to
   where
    to = \case
      SpeedUp  -> 0
      SlowDown -> 1
      NoChange -> 2

data ToFincFdecState dom
  = Wait (Index (PeriodToCycles dom (Microseconds 1)))
  | Pulse (Index (PeriodToCycles dom (Nanoseconds 100))) SpeedChange
  | Idle
  deriving (Generic, NFDataX)

-- | Convert 'SpeedChange' to a pair of (FINC, FDEC). This is currently hardcoded
-- to work on the Si5395 constraints:
--
--   * Minimum Pulse Width: 100 ns
--   * Update Rate: 1 us
--
-- TODO: De-hardcode
speedChangeToFincFdec ::
  forall dom .
  KnownDomain dom =>
  Clock dom ->
  Reset dom ->
  Signal dom SpeedChange ->
  Signal dom (Bool, Bool)
speedChangeToFincFdec clk rst =
  dflipflop clk . fmap conv . mealy clk rst enableGen go (Wait maxBound)
 where
  go :: ToFincFdecState dom -> SpeedChange -> (ToFincFdecState dom, SpeedChange)
  go (Wait n) _s
    | n == 0    = (Idle,         NoChange)
    | otherwise = (Wait (n - 1), NoChange)

  go (Pulse n s) _s
    | n == 0    = (Wait maxBound, s)
    | otherwise = (Pulse (n - 1) s, s)

  go Idle NoChange = (Idle, NoChange)
  go Idle s        = (Pulse maxBound s, NoChange)

  --               FINC   FDEC
  conv NoChange = (False, False)
  conv SpeedUp  = (True,  False)
  conv SlowDown = (False, True)

instance ToField SpeedChange where
  toField SpeedUp = "speedUp"
  toField SlowDown = "slowDown"
  toField NoChange = "noChange"

instance KnownNat n => ToField (Unsigned n) where
  toField = toField . toInteger

instance KnownNat n => ToJSON (Unsigned n) where
  toJSON = toJSON . toInteger

instance KnownNat n => ToField (Signed n) where
  toField = toField . toInteger

instance KnownNat n => ToJSON (Signed n) where
  toJSON = toJSON . toInteger

instance ToField Femtoseconds where
  toField (Femtoseconds fs) = toField fs

instance ToJSON Femtoseconds where
  toJSON (Femtoseconds fs) = toJSON fs

clockPeriodFs :: forall dom. KnownDomain dom => Proxy dom -> Femtoseconds
clockPeriodFs Proxy = Femtoseconds (1000 * snatToNum (clockPeriod @dom))

defClockConfig :: forall dom. KnownDomain dom => ClockControlConfig dom 12 8 1500000
defClockConfig = ClockControlConfig
  { cccPessimisticPeriod         = pessimisticPeriod
  , cccPessimisticSettleCycles   = pessimisticSettleCycles self
  , cccSettlePeriod              = microseconds 1
  , cccStepSize                  = stepSize
  , cccBufferSize                = d12 -- 2**12 ~ 4096
  , cccDeviation                 = Ppm 100
  , cccStabilityCheckerMargin    = SNat
  , cccStabilityCheckerFramesize = SNat
  , cccEnableReframing           = True
  , cccReframingWaitTime         = 100000
  , cccEnableRustySimulation     = False
  }
 where
  self = defClockConfig @dom
  stepSize = diffPeriod (Ppm 1) (clockPeriodFs @dom Proxy)
  pessimisticPeriod = adjustPeriod (cccDeviation self) (clockPeriodFs @dom Proxy)

-- | Number of cycles to wait on a given clock frequency and clock settings in
-- order for the settle period to pass. /Pessimistic/ means that it calculates
-- this for the fastest possible clock.
--
pessimisticSettleCycles ::
  forall dom n m c.
  ( HasCallStack
  , KnownDomain dom ) =>
  ClockControlConfig dom n m c->
  -- | It would take a 10 GHz clock only a 10_000 cycles to wait 1 µs. This can be
  -- met by an @Unsigned 14@: @2^14 ~ 16384@. To massively overkill it we bump it
  -- up to 32 bits.
  Unsigned 32
pessimisticSettleCycles ClockControlConfig{cccSettlePeriod, cccDeviation} =
  checkedFromIntegral nCycles
 where
  nCycles = (settlePeriod `div` pessimisticPeriod) + 1
  Femtoseconds settlePeriod = cccSettlePeriod
  period = clockPeriodFs @dom Proxy
  Femtoseconds pessimisticPeriod = adjustPeriod cccDeviation period
