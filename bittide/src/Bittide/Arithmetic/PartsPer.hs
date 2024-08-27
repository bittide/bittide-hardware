-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores #-}

{- | A more-or-less ad-hoc collection of definitions to work with measuring
clock frequency differences without loss-of-precision.
-}
module Bittide.Arithmetic.PartsPer where

import Bittide.Arithmetic.Time (PeriodToCycles)
import Clash.Prelude hiding (PeriodToCycles)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Data (Proxy (..))

{- $setup
>>> import Clash.Prelude
-}

{- | A relative measure of frequency. E.g., a 'PartsPer' of @1 ppm@ of some ideal
frequency means that for every million ticks in an ideal frequency, there are
a million and one ticks in the observed frequency.

It is internally represented as a 64-bit signed integer, representing
_parts per trillion_.
-}
newtype PartsPer = Ppt (Signed 64)
  deriving (Show, Eq, Ord)
  deriving newtype (Num)

instance ToJSON PartsPer where
  toJSON (Ppt x) = toJSON (toInteger x)

instance FromJSON PartsPer where
  parseJSON = fmap (Ppt . fromInteger) . parseJSON

ppt :: Signed 64 -> PartsPer
ppt = Ppt . (* 1)

ppb :: Signed 64 -> PartsPer
ppb = Ppt . (* 1_000)

ppm :: Signed 64 -> PartsPer
ppm = Ppt . (* 1_000_000)

toPpt :: PartsPer -> Float
toPpt = toSteps (ppt 1)

toPpb :: PartsPer -> Float
toPpb = toSteps (ppb 1)

toPpm :: PartsPer -> Float
toPpm = toSteps (ppm 1)

{- | Convert a 'PartsPer' to a 'Float'. Useful for pretty printing 'PartsPer' to
a human-readable step size (usually, 1 ppm). Has no representation in hardware.

>>> toSteps (ppb 1) (ppm 3)
3000.0
>>> toSteps (ppm 1) (cyclesToPartsPer (125_000_000 :: Integer) 125_000_879)
7.032
-}
toSteps ::
  -- | Step size
  PartsPer ->
  -- | Value
  PartsPer ->
  -- | Number of steps to get to the value
  Float
toSteps (Ppt stepSize) (Ppt value) = fromIntegral value / fromIntegral stepSize

{- | Given an ideal number of clock cycles - passed indirectly using a combination
of a domain and a measurement period - and an observed number of clock cycles,
calculate the difference in 'PartsPer'. Has no representation in hardware.
-}
cyclesToPartsPerI ::
  forall dom ps a.
  (KnownDomain dom, KnownNat ps, Integral a) =>
  -- | Clock domain. Proxy for an ideal frequency.
  Proxy dom ->
  -- | Period observed for in picoseconds (see 'Seconds', 'Milliseconds', etc.). E.g.,
  -- if you want to pass in 1 millisecond, you would pass in:
  --
  -- > cyclesToPartsPerI (Proxy @dom) (Proxy @(Milliseconds 1))
  Proxy ps ->
  -- | Observed number of clock cycles
  a ->
  PartsPer
cyclesToPartsPerI _ _ = cyclesToPartsPer (natToNum @(PeriodToCycles dom ps))

{- | Given an ideal number of clock cycles and an observed number of clock cycles,
calculate the difference in 'PartsPer'. Has no representation in hardware.

>>> cyclesToPartsPer 125_000_000 125_000_250
Ppt 2000000
>>> toPpm (cyclesToPartsPer 125_000_000 125_000_250)
2.0
-}
cyclesToPartsPer ::
  (Integral a) =>
  -- | Ideal number of clock cycles
  a ->
  -- | Observed number of clock cycles
  a ->
  PartsPer
cyclesToPartsPer (toInteger -> ideal) (toInteger -> observed) =
  Ppt (fromInteger (((observed * factor) `div` ideal) - factor))
 where
  factor = 1_000_000_000_000
