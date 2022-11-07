-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# OPTIONS_GHC -fconstraint-solver-iterations=7 #-}

module Bittide.Instances.Link where

import Clash.Prelude

import Bittide.Instances.Domains
import Bittide.Instances.Hacks
import Bittide.Link
import Bittide.SharedTypes
import Protocols.Wishbone

type FrameWidth = 64
type SequenceCounterWidth = 64
type WishboneAddrWidth = 32
type WishboneWidth = 4

preamble :: BitVector 1024
preamble = pack $ iterate d16 ((*3) . succ) (0 :: Unsigned 64)

{-# ANN txUnit64_1K
  (Synthesize
    { t_name = "txUnit64_1K"
    , t_inputs =
        [ PortName "clk"
        , PortName "rst"
        , PortName "localSequenceCounter"
        , PortName "linkIn"
        , PortName "wbM2S"
        ]
    , t_output = PortProduct ""
        [ PortName "wbS2M"
        , PortName "linkOut"
        ]
    }
  )#-}

txUnit64_1K ::
  Clock Basic200 ->
  Reset Basic200 ->
  Signal Basic200 (Unsigned SequenceCounterWidth) ->
  Signal Basic200 (DataLink FrameWidth) ->
  Signal Basic200 (WishboneM2S WishboneAddrWidth WishboneWidth (Bytes WishboneWidth)) ->
  ( Signal Basic200 (WishboneS2M (Bytes WishboneWidth))
  , Signal Basic200 (DataLink FrameWidth))
txUnit64_1K clk rst = withClockResetEnable clk rst enableGen (txUnit preamble)
{-# NOINLINE txUnit64_1K #-}

txUnit64_1KReducedPins :: Clock Basic200 -> Reset Basic200 -> Signal Basic200 Bit -> Signal Basic200 Bit
txUnit64_1KReducedPins clk rst = withClockResetEnable clk rst enableGen (reducePins txUnit64_1K')
 where
  txUnit64_1K' (unbundle -> (a,b,c)) = bundle $ txUnit64_1K clk rst a b c

{-# ANN rxUnit64_1K
  (Synthesize
    { t_name = "rxUnit64_1K"
    , t_inputs =
        [ PortName "clk"
        , PortName "rst"
        , PortName "localSequenceCounter"
        , PortName "linkIn"
        , PortName "wbM2S"
        ]
    , t_output = PortName "wbS2M"
    }
  )#-}

rxUnit64_1K ::
  Clock Basic200 ->
  Reset Basic200 ->
  Signal Basic200 (Unsigned SequenceCounterWidth) ->
  Signal Basic200 (DataLink FrameWidth) ->
  Signal Basic200 (WishboneM2S WishboneAddrWidth WishboneWidth (Bytes WishboneWidth)) ->
  Signal Basic200 (WishboneS2M (Bytes WishboneWidth))
rxUnit64_1K clk rst = withClockResetEnable clk rst enableGen (rxUnit preamble)
{-# NOINLINE rxUnit64_1K #-}

rxUnit64_1KReducedPins :: Clock Basic200 -> Reset Basic200 -> Signal Basic200 Bit -> Signal Basic200 Bit
rxUnit64_1KReducedPins clk rst = withClockResetEnable clk rst enableGen (reducePins rxUnit64_1K')
 where
  rxUnit64_1K' (unbundle -> (a,b,c)) = rxUnit64_1K clk rst a b c
