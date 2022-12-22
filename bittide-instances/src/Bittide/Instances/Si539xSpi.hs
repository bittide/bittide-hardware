-- SPDX-FileCopyrightText: 2022-2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module Bittide.Instances.Si539xSpi where

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

si5391Spi ::
  "CLK_125MHZ_P" ::: Clock Basic125 ->
  "reset" ::: Reset Basic125 ->
  "extOp" ::: Signal Basic125 (Maybe RegisterOperation) ->
  "MISO" ::: Signal Basic125 Bit ->
  "" :::
    ( "readByte" ::: Signal Basic125 (Maybe Byte)
    , "BUSY" ::: Signal Basic125 Busy
    , "STATE" ::: Signal Basic125 (ConfigState Basic125 592)
    , "" :::
      ( "SCLK" ::: Signal Basic125 Bool
      , "MOSI" ::: Signal Basic125 Bit
      , "CSB" ::: Signal Basic125 Bool)
  )
si5391Spi clk rst extOp miso = withClockResetEnable clk rst enableGen $
  si539xSpi testConfigAll200 (SNat @50000) extOp miso

makeTopEntity 'si5391Spi

callistoSpi ::
  "CLK_125MHZ"    ::: Clock Basic125 ->
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
  (_, spiBusy, configState, spiOut) = withClockResetEnable clk125 configReset enableGen
    si539xSpi testConfig6_200_5_20 (SNat @(Nanoseconds 75)) freqOp miso

  configReset = withClockResetEnable clk125 rst125 enableGen $
    forceReset . holdTrue d3 $ flip fmap configState \case
      Error _ -> True
      _       -> False

  freqOp = spiFrequencyController d128 d127
    clkControlled rstControlled enableGen
    clk125 rst125 enableGen speedChange200 spiBusy

  speedChange200 = callistoClockControl @1 @12 clkControlled clockControlReset enableGen
    clockConfig (bufferOccupancy :> Nil)

  rstControlled = unsafeFromLowPolarity locked
  clockControlReset = unsafeFromLowPolarity $ ebMode .==. pure Pass


  (bufferOccupancy, _, _, ebMode) =
    withReset rstControlled $
      resettableXilinxElasticBuffer clkControlled clkRecovered (unsafeFromLowPolarity $ pure True)

  isStable =
    withClockResetEnable clkControlled (unsafeFromLowPolarity $ pure True) enableGen $
      stabilityChecker d5 (SNat @1_000_000) bufferOccupancy


  clockConfig :: ClockControlConfig External 12
  clockConfig = $(lift ((defClockConfig @External){cccPessimisticSettleCycles = 20000} ))

makeTopEntity 'callistoSpi
