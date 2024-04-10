-- SPDX-FileCopyrightText: 2022-2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module Bittide.Instances.Pnr.Si539xSpi where

import Clash.Prelude

import Clash.Annotations.TH (makeTopEntity)

import Bittide.Arithmetic.Time
import Bittide.ClockControl
import Bittide.ClockControl.Callisto
import Bittide.ClockControl.Si5395J
import Bittide.ClockControl.Si539xSpi
import Bittide.ClockControl.StabilityChecker
import Bittide.ElasticBuffer
import Bittide.Instances.Domains
import Bittide.SharedTypes
import Data.Maybe

si5391Spi ::
  "CLK_125MHZ_P" ::: Clock Basic125 ->
  "reset" ::: Reset Basic125 ->
  "extOp" ::: Signal Basic125 (Maybe RegisterOperation) ->
  "MISO" ::: Signal Basic125 Bit ->
  "" :::
    ( "readByte" ::: Signal Basic125 (Maybe Byte)
    , "BUSY" ::: Signal Basic125 Busy
    , "STATE" ::: Signal Basic125 (ConfigState Basic125 TestConfig6_200_on_0a_TotalRegs)
    , "" :::
      ( "SCLK" ::: Signal Basic125 Bool
      , "MOSI" ::: Signal Basic125 Bit
      , "CSB" ::: Signal Basic125 Bool)
  )
si5391Spi clk rst extOp miso = withClockResetEnable clk rst enableGen $
  si539xSpi testConfig6_200_on_0a_1ppb (SNat @50000) extOp miso

makeTopEntity 'si5391Spi

-- | An instance that combines the following components:
--
--     * 'Si539xSpi' core to configure the @Si5395J@ chip over SPI and enable
--       further SPI communication
--
--     * a `resettableXilinxElasticBuffer` to obtain the datacounts for `callisto`.
--
--     * `callistoClockControl` to run the control algorithm that adjusts the
--       clock frequency.
--
--     * A `spiFrequencyController` to convert `callisto`s `SpeedChange`s into
--      `RegisterOperation`s for the `Si539xSpi`.
--
--     * A `stabilityChecker` to indicate when the clocks are considered to be
--       synchronized.
callistoSpi ::
  "CLK_125MHZ"      ::: Clock Basic125 ->
  "clkRecovered"    ::: Clock Internal ->
  "clkControlled"   ::: Clock External ->
  "reset125"        ::: Reset Basic125 ->
  "locked"          ::: Signal External Bool ->
  "MISO"            ::: Signal Basic125 Bit ->
  "" :::
    ( "BUSY"        ::: Signal Basic125 Busy
    , "DONE"        ::: Signal Basic125 Bool
    , "EBPASS"      ::: Signal External Bool
    , "EBSTABLE"    ::: Signal External Bool
    , "" :::
      ( "SCLK"      ::: Signal Basic125 Bool
      , "MOSI"      ::: Signal Basic125 Bit
      , "CSB"       ::: Signal Basic125 Bool)
    )
callistoSpi clk125 clkRecovered clkControlled rst125 locked miso =
  (spiBusy, configState .==. pure Finished, ebMode .==. pure Pass, isStable, spiOut)
 where
  -- Spi core, the maximum clock period of 75 Nanoseconds leads to a nice 5 to 1 clock
  -- divider at 125MHz, resulting in a SPI clock frequencu of 12.5MHz. The
  -- SPI core _should_ be able to support a SPI clock frequency of 20MHz.
  (_, spiBusy, configState, spiOut) = withClockResetEnable clk125 configReset enableGen
    si539xSpi testConfig6_200_5_20 (SNat @(Nanoseconds 75)) freqOp miso

  -- Reset the spi core when configuration fails to try again.
  configReset = withClockResetEnable clk125 rst125 enableGen $
    forceReset . holdTrue d3 $ flip fmap configState \case
      Error _ -> True
      _       -> False

  -- Convert the SpeedChange produced by Callisto into a RegisterOperation for Si539x
  freqOp = spiFrequencyController d128 d127
    clkControlled rstControlled enableGen
    clk125 rst125 enableGen speedChange200 spiBusy

  -- Produce a SpeedChange based on the elastic buffer's datacount.
  speedChange200 =
    (fromMaybe NoChange . maybeSpeedChange) <$>
      callistoClockControl @1 @12 clkControlled clockControlReset enableGen
        clockConfig (pure maxBound) (bufferOccupancy :> Nil)

  -- ALl circuitry in the controlled domain should be in reset while the the PLL is not locked.
  rstControlled = unsafeFromActiveLow locked

  -- Callisto comes out of reset while the elastic buffer is in Pass mode.
  clockControlReset = unsafeFromActiveLow $ ebMode .==. pure Pass

  -- The elastic buffer.
  (bufferOccupancy, _, _, ebMode, _) =
    withReset rstControlled $
      resettableXilinxElasticBuffer clkControlled clkRecovered (unsafeFromActiveLow $ pure True) (pure False)

  -- Determine if the controlled clock is synchronized "enough" with the static clock.
  isStable =
    withClockResetEnable clkControlled (unsafeFromActiveLow $ pure True) enableGen $
      settled <$> stabilityChecker d5 (SNat @1_000_000) bufferOccupancy

  -- Configuration for Callisto
  clockConfig :: ClockControlConfig External 12 8 1500000
  clockConfig = $(lift ((defClockConfig @External){cccPessimisticSettleCycles = 20000} ))

makeTopEntity 'callistoSpi
