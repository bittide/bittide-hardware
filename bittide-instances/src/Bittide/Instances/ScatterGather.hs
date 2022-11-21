-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -fconstraint-solver-iterations=7 #-}

module Bittide.Instances.ScatterGather where

import Clash.Prelude

import Bittide.Calendar
import Bittide.Instances.Domains
import Bittide.Instances.Hacks
import Bittide.ScatterGather
import Bittide.SharedTypes
import Protocols.Wishbone

type FrameWidth = 64
type NLinks = 16
type WishboneAddrWidth = 32
type WishboneWidth = 4

scatterCal1K :: ScatterConfig WishboneWidth WishboneAddrWidth
scatterCal1K = ScatterConfig cal
 where
  cal :: CalendarConfig WishboneWidth WishboneAddrWidth (Index 1024)
  cal = CalendarConfig (SNat @1024)
    (ValidEntry{veEntry=0, veRepeat = 0 :: Unsigned 8} :> Nil)
    (ValidEntry{veEntry=0, veRepeat = 0 :: Unsigned 8} :> Nil)

gatherCal1K :: GatherConfig WishboneWidth WishboneAddrWidth
gatherCal1K = GatherConfig cal
 where
  cal :: CalendarConfig WishboneWidth WishboneAddrWidth (Index 1024)
  cal = CalendarConfig (SNat @1024)
    (ValidEntry{veEntry=0, veRepeat = 0 :: Unsigned 8} :> Nil)
    (ValidEntry{veEntry=0, veRepeat = 0 :: Unsigned 8} :> Nil)

{-# ANN scatterUnit1K
  (Synthesize
    { t_name = "scatterUnit1K"
    , t_inputs =
      [ PortName "clk"
      , PortName "rst"
      , PortName "wbInCal"
      , PortName "linkIn"
      , PortName "wbInSu"
      ]
    , t_output = PortProduct ""
      [ PortName "wbOutSu"
      , PortName "wbOutCal"
      ]
    }
  )#-}
scatterUnit1K ::
  Clock Basic200 ->
  Reset Basic200 ->
  Signal Basic200 (WishboneM2S WishboneAddrWidth WishboneWidth (Bytes WishboneWidth)) ->
  Signal Basic200 (DataLink 64) ->
  Signal Basic200 (WishboneM2S WishboneAddrWidth WishboneWidth (Bytes WishboneWidth)) ->
  ( Signal Basic200 (WishboneS2M (Bytes WishboneWidth))
  , Signal Basic200 (WishboneS2M (Bytes WishboneWidth)))
scatterUnit1K clk rst = withClockResetEnable clk rst enableGen $ scatterUnitWb scatterCal1K
{-# NOINLINE scatterUnit1K #-}

scatterUnit1KReducedPins ::
  Clock Basic200 -> Reset Basic200 -> Signal Basic200 Bit -> Signal Basic200 Bit
scatterUnit1KReducedPins clk rst =
  withClockResetEnable clk rst enableGen $ reducePins scatterUnit1K'
 where
  scatterUnit1K' (unbundle -> (a,b,c)) = bundle $ scatterUnit1K clk rst a b c

{-# ANN gatherUnit1K
  (Synthesize
    { t_name = "gatherUnit1K"
    , t_inputs =
        [ PortName "clk"
        , PortName "rst"
        , PortName "wbInCal"
        , PortName "wbInGu"
        ]
    , t_output = PortProduct ""
        [ PortName "linkOut"
        , PortName "wbOutGu"
        , PortName "wbOutCal"
        ]
    }
  )#-}
gatherUnit1K ::
  Clock Basic200 ->
  Reset Basic200 ->
  Signal Basic200 (WishboneM2S WishboneAddrWidth WishboneWidth (Bytes WishboneWidth)) ->
  Signal Basic200 (WishboneM2S WishboneAddrWidth WishboneWidth (Bytes WishboneWidth)) ->
  ( Signal Basic200 (DataLink 64)
  , Signal Basic200 (WishboneS2M (Bytes WishboneWidth))
  , Signal Basic200 (WishboneS2M (Bytes WishboneWidth)))
gatherUnit1K clk rst = withClockResetEnable clk rst enableGen $ gatherUnitWb gatherCal1K
{-# NOINLINE gatherUnit1K #-}

gatherUnit1KReducedPins ::
  Clock Basic200 -> Reset Basic200 -> Signal Basic200 Bit -> Signal Basic200 Bit
gatherUnit1KReducedPins clk rst =
  withClockResetEnable clk rst enableGen $ reducePins gatherUnit1K'
 where
  gatherUnit1K' (unbundle -> (a,b)) = bundle $ gatherUnit1K clk rst a b
