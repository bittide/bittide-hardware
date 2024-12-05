-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module Clash.Cores.Xilinx.GTH.Internal where

import Clash.Explicit.Prelude

import Clash.Annotations.Primitive (
  Primitive (InlineYamlPrimitive),
  hasBlackBox,
 )
import Data.String.Interpolate (__i)

import Clash.Cores.Xilinx.GTH.BlackBoxes
import Clash.Cores.Xilinx.Xpm.Cdc.Internal (
  ClockPort (..),
  Param (..),
  Port (..),
  ResetPort (..),
  inst,
  instConfig,
  unPort,
 )

type TX_DATA_WIDTH = 64
type RX_DATA_WIDTH = 64

type GthCore txUser txUser2 rxUser rxUser2 refclk0 freerun txS rxS serializedData =
  ( KnownDomain txUser
  , KnownDomain txUser2
  , KnownDomain rxUser
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
  "txusrclk_in" ::: Clock txUser ->
  "txusrclk2_in" ::: Clock txUser2 ->
  "gtwiz_userclk_tx_active_in" ::: Signal txUser2 (BitVector 1) ->
  "rxusrclk_in" ::: Clock rxUser ->
  "rxusrclk2_in" ::: Clock rxUser2 ->
  "gtwiz_userclk_rx_active_in" ::: Signal rxUser2 (BitVector 1) ->
  ( "gthtxn_out" ::: Signal txS serializedData
  , "gthtxp_out" ::: Signal txS serializedData
  , "txoutclk_out" ::: Clock txUser
  , "rxoutclk_out" ::: Clock rxUser
  , "gtwiz_userdata_rx_out" ::: Signal rxUser2 (BitVector RX_DATA_WIDTH)
  , "gtwiz_reset_tx_done_out" ::: Signal txUser2 (BitVector 1)
  , "gtwiz_reset_rx_done_out" ::: Signal rxUser2 (BitVector 1)
  , "txpmaresetdone_out" ::: Signal txUser (BitVector 1)
  , "rxpmaresetdone_out" ::: Signal rxUser (BitVector 1)
  , "rxctrl0_out" ::: Signal rxUser2 (BitVector 16)
  , "rxctrl1_out" ::: Signal rxUser2 (BitVector 16)
  , "rxctrl2_out" ::: Signal rxUser2 (BitVector 8)
  , "rxctrl3_out" ::: Signal rxUser2 (BitVector 8)
  )

gthCore :: GthCore txUser txUser2 rxUser rxUser2 refclk0 freerun txS rxS (BitVector 1)
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
  !_gtrefclk0_in
  !_
  !_
  !_
  !_
  !_
  !_ =
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

xilinxGthUserClockNetworkTx ::
  forall user user2.
  (KnownDomain user, KnownDomain user2) =>
  Clock user ->
  Reset user2 ->
  (Clock user, Clock user2, Signal user2 (BitVector 1))
xilinxGthUserClockNetworkTx clkIn rstIn = (unPort usrclk_out, unPort usrclk2_out, pack <$> unPort tx_active_out)
 where
  (usrclk_out, usrclk2_out, tx_active_out) = go (Param 2) (ClockPort clkIn) (ResetPort rstIn)
  go ::
    Param "P_FREQ_RATIO_USRCLK_TO_USRCLK2" Integer ->
    ClockPort "gtwiz_userclk_tx_srcclk_in" user ->
    ResetPort "gtwiz_userclk_tx_reset_in" ActiveHigh user2 ->
    ( ClockPort "gtwiz_userclk_tx_usrclk_out" user
    , ClockPort "gtwiz_userclk_tx_usrclk2_out" user2
    , Port "gtwiz_userclk_tx_active_out" user2 Bit
    )
  go = inst (instConfig "gtwizard_ultrascale_v1_7_13_gtwiz_userclk_tx")
{-# NOINLINE xilinxGthUserClockNetworkTx #-}

xilinxGthUserClockNetworkRx ::
  forall user user2.
  (KnownDomain user, KnownDomain user2) =>
  Clock user ->
  Reset user2 ->
  (Clock user, Clock user2, Signal user2 (BitVector 1))
xilinxGthUserClockNetworkRx clkIn rstIn = (unPort usrclk_out, unPort usrclk2_out, pack <$> unPort rx_active_out)
 where
  (usrclk_out, usrclk2_out, rx_active_out) = go (Param 2) (ClockPort clkIn) (ResetPort rstIn)
  go ::
    Param "P_FREQ_RATIO_USRCLK_TO_USRCLK2" Integer ->
    ClockPort "gtwiz_userclk_rx_srcclk_in" user ->
    ResetPort "gtwiz_userclk_rx_reset_in" ActiveHigh user2 ->
    ( ClockPort "gtwiz_userclk_rx_usrclk_out" user
    , ClockPort "gtwiz_userclk_rx_usrclk2_out" user2
    , Port "gtwiz_userclk_rx_active_out" user2 Bit
    )
  go = inst (instConfig "gtwizard_ultrascale_v1_7_13_gtwiz_userclk_rx")
{-# NOINLINE xilinxGthUserClockNetworkRx #-}

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
