-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module Clash.Cores.Xilinx.GTH.Internal where

import Clash.Prelude

import Clash.Annotations.Primitive (Primitive (InlineYamlPrimitive), hasBlackBox)
import Data.String.Interpolate (__i)

import Clash.Cores.Xilinx.GTH.BlackBoxes

type TX_DATA_WIDTH = 64
type RX_DATA_WIDTH = 64

type GthCore txUser2 rxUser2 refclk0 freerun txS rxS serializedData =
  ( KnownDomain txUser2
  , KnownDomain rxUser2
  , KnownDomain refclk0
  , KnownDomain freerun
  , KnownDomain txS
  , KnownDomain rxS
  ) =>
  -- | channel
  String ->
  -- | refClkSpec
  String ->
  "gthrxn_in" ::: Signal rxS serializedData ->
  "gthrxp_in" ::: Signal rxS serializedData ->
  "gtwiz_reset_clk_freerun_in" ::: Clock freerun ->
  "gtwiz_reset_all_in" ::: Reset freerun ->
  "gtwiz_reset_rx_datapath_in" ::: Reset freerun ->
  "gtwiz_userdata_tx_in" ::: Signal txUser2 (BitVector TX_DATA_WIDTH) ->
  "txctrl2_in" ::: Signal txUser2 (BitVector (DivRU TX_DATA_WIDTH 8)) ->
  "gtrefclk0_in" ::: Clock refclk0 ->
  ( "gthtxn_out" ::: Signal txS serializedData
  , "gthtxp_out" ::: Signal txS serializedData
  , "gtwiz_userclk_tx_usrclk2_out" ::: Clock txUser2
  , "gtwiz_userclk_rx_usrclk2_out" ::: Clock rxUser2
  , "gtwiz_userdata_rx_out" ::: Signal rxUser2 (BitVector RX_DATA_WIDTH)
  , "gtwiz_reset_tx_done_out" ::: Signal txUser2 (BitVector 1)
  , "gtwiz_reset_rx_done_out" ::: Signal rxUser2 (BitVector 1)
  , "gtwiz_userclk_tx_active_out" ::: Signal txUser2 (BitVector 1)
  , "rxctrl0_out" ::: Signal rxUser2 (BitVector 16)
  , "rxctrl1_out" ::: Signal rxUser2 (BitVector 16)
  , "rxctrl2_out" ::: Signal rxUser2 (BitVector 8)
  , "rxctrl3_out" ::: Signal rxUser2 (BitVector 8)
  )

gthCore :: GthCore txUser2 rxUser2 refclk0 freerun txS rxS (BitVector 1)
gthCore
  !_channel
  !_refClkSpec
  !_gthrxn_in
  !_gthrxp_in
  !_gtwiz_reset_clk_freerun_in
  !_gtwiz_reset_all_in
  !_gtwiz_reset_rx_datapath_in
  !_gtwiz_userdata_tx_in
  !_txctrl2_in
  !_gtrefclk0_in =
    ( undefined
    , undefined
    , undefined
    , undefined
    , undefined
    , undefined
    , undefined
    , undefined
    , undefined
    , undefined
    , undefined
    , undefined
    )
{-# OPAQUE gthCore #-}
{-# ANN gthCore hasBlackBox #-}
{-# ANN
  gthCore
  ( let primName = 'gthCore
        tfName = 'gthCoreBBF
     in InlineYamlPrimitive
          [minBound ..]
          [__i|
        BlackBoxHaskell:
            name: #{primName}
            templateFunction: #{tfName}
            workInfo: Always
        |]
  )
  #-}

ibufds_gte3 :: (KnownDomain dom) => DiffClock dom -> Clock dom
ibufds_gte3 !_clk = clockGen
{-# OPAQUE ibufds_gte3 #-}
{-# ANN ibufds_gte3 hasBlackBox #-}
{-# ANN
  ibufds_gte3
  ( let primName = 'ibufds_gte3
        tfName = 'ibufds_gte3BBF
     in InlineYamlPrimitive
          [minBound ..]
          [__i|
        BlackBoxHaskell:
            name: #{primName}
            templateFunction: #{tfName}
            workInfo: Always
        |]
  )
  #-}
