-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# OPTIONS_GHC -fconstraint-solver-iterations=8 #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MagicHash #-}

module Bittide.Instances.Switch where

import Clash.Prelude

import Bittide.Calendar
import Bittide.Instances.Domains
import Bittide.Instances.Hacks
import Bittide.SharedTypes
import Bittide.Switch
import Data.Bifunctor
import Protocols.Wishbone

type WishboneWidth = 4
type WishboneAddrWidth = 32
type FrameWidth = 64
type NLinks = 16

{-# ANN switch_16_64
  (Synthesize
    { t_name = "switch_16_64"
    , t_inputs =
        [ PortName "clk"
        , PortName "rst"
        , PortName "calM2S"
        , PortName "rxM2Ss"
        , PortName "txM2Ss"
        , PortName "linksIn"
        ]
    , t_output = PortProduct ""
        [ PortName "linksOu"
        , PortName "s2Ms"
        ]
    }
  )#-}

switch_16_64 ::
  Clock Basic200 ->
  Reset Basic200 ->
  Signal Basic200 (WishboneM2S WishboneAddrWidth WishboneWidth (Bytes WishboneWidth)) ->
  Vec NLinks (Signal Basic200 (WishboneM2S WishboneAddrWidth WishboneWidth (Bytes WishboneWidth))) ->
  Vec NLinks (Signal Basic200 (WishboneM2S WishboneAddrWidth WishboneWidth (Bytes WishboneWidth))) ->
  Vec NLinks (Signal Basic200 (DataLink FrameWidth)) ->
  ( Vec NLinks  (Signal Basic200 (DataLink FrameWidth))
  , Vec (1 + (2 * NLinks)) (Signal Basic200 (WishboneS2M (Bytes WishboneWidth))))
switch_16_64 clk rst = exposeClockResetEnable (mkSwitch switchConfig) clk rst enableGen
 where
  switchConfig = SwitchConfig{preamble, calendarConfig}
  preamble = concatBitVector#
    (0xDEADBABE :> 0xDEADBEEF :> 0xA5A5A5A5 :> 0xABABABAB :> Nil) :: BitVector 1024
  calendarConfig = CalendarConfig
    (SNat @2)
    (ValidEntry{veEntry=repeat 0, veRepeat = 0 :: Unsigned 0} :> Nil)
    (ValidEntry{veEntry=repeat 0, veRepeat = 0 :: Unsigned 0} :> Nil)

{-# NOINLINE switch_16_64 #-}

switch_16_64ReducedPins ::
  Clock Basic200 -> Reset Basic200 -> Signal Basic200 Bit -> Signal Basic200 Bit
switch_16_64ReducedPins clk rst bitIn = bitOut
 where
  bitOut = withClockResetEnable clk rst enableGen (reducePins switch_16_64' bitIn)

  switch_16_64' allInputs = bundle $ bimap bundle bundle (switch_16_64 clk rst a b c d)
   where
    (a, unbundle -> (unbundle -> b, unbundle -> c, unbundle -> d)) = unbundle allInputs
