-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE RecordWildCards #-}

module Bittide.ClockControl.Si5395J where

import Bittide.ClockControl.ParseRegisters
import Bittide.ClockControl.Si539xSpi
import Clash.Prelude

type Si5395RegisterMap = Si539xRegisterMap 3 584 5

type TestConfig6_200_on_0a_RegisterMap = Si539xRegisterMap 3 590 5
type TestConfig6_200_on_0a_TotalRegs = 3 + 590 + 5

{- | Configuration for Si5395J with the following configuration:

 out0a:  200MHz LVDS 1.8V
 out9:  12.5MHz LVDS 1.8V
 out9a:  200MHz LVDS 1.8V

 all of them doing 1ppb steps on Finc/Fdec
-}
testConfig6_200_on_0a_1ppb :: TestConfig6_200_on_0a_RegisterMap
testConfig6_200_on_0a_1ppb = $(parseFromFileToRegisterMap "Si5395J-200MHz-1ppb-Registers")

-- | Same as 'testConfig6_200_on_0a_1ppb', but with a 10 ppb step size
testConfig6_200_on_0a_10ppb :: TestConfig6_200_on_0a_RegisterMap
testConfig6_200_on_0a_10ppb = $(parseFromFileToRegisterMap "Si5395J-200MHz-10ppb-Registers")

-- | Same as 'testConfig6_200_on_0a_1ppb', but with a 100 ppb step size
testConfig6_200_on_0a_100ppb :: TestConfig6_200_on_0a_RegisterMap
testConfig6_200_on_0a_100ppb = $(parseFromFileToRegisterMap "Si5395J-200MHz-100ppb-Registers")

-- | Same as 'testConfig6_200_on_0a_1ppb', but with a 500 ppb step size
testConfig6_200_on_0a_500ppb :: TestConfig6_200_on_0a_RegisterMap
testConfig6_200_on_0a_500ppb = $(parseFromFileToRegisterMap "Si5395J-200MHz-500ppb-Registers")

-- | Same as 'testConfig6_200_on_0a_1ppb', but with a 1 ppm step size
testConfig6_200_on_0a_1ppm :: TestConfig6_200_on_0a_RegisterMap
testConfig6_200_on_0a_1ppm = $(parseFromFileToRegisterMap "Si5395J-200MHz-1ppm-Registers")

{- | Configuration for Si5395J with
  out0a: 200MHz LVDS 1.8V  connected to GTH SMA clk input (clk0 on quad 226)
  out1:  200MHz LVDS 1.8V  connected to User SMA clk input on node 7 only
  out9:   20MHZ LVDS 1.8V
  out9a: 200MHz LVDS 1.8V
  all of them doing 10ppb steps on Finc/Fdec
-}
testConfig6_200_on_0a_10ppb_and_1 :: Si5395RegisterMap
testConfig6_200_on_0a_10ppb_and_1 = $(parseFromFileToRegisterMap "Si5395J-200MHz-10ppb-and-out1")
