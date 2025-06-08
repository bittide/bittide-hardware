-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

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

{- | Data wires from/to transceivers. No logic should be inserted on these
wires. Should be considered asynchronous to one another - even though their
domain encodes them as related.
-}
type Wires (line :: Domain) n = Signal line (BitVector n)

-- | Equivalent to 'Wires', but as a simulation only construct
type SimWires (logic :: Domain) n = SimOnly (Vec n (Signal logic (BitVector 64)))

-- | Data wire from/to transceivers
type Wire (line :: Domain) = Signal line (BitVector 1)

-- | Equivalent to 'Wire', but as a simulation only construct
type SimWire (logic :: Domain) = SimOnly (Signal logic (BitVector 64))

-- | Strip "SimOnly" constructor
unSimOnly :: SimOnly a -> a
unSimOnly (SimOnly x) = x

type GthCore txUser txUser2 rxUser rxUser2 refclk0 freerun txS rxS =
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
  "gthrx_in" ::: SimWire rxUser2 ->
  "gthrxn_in" ::: Wire rxS ->
  "gthrxp_in" ::: Wire rxS ->
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
  ( "gthtx_out" ::: SimWire txUser2
  , "gthtxn_out" ::: Wire txS
  , "gthtxp_out" ::: Wire txS
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

gthCore :: GthCore txUser txUser2 rxUser rxUser2 refclk0 freerun txS rxS
gthCore
  !_channel
  !_refClkSpec
  gthrx_in
  !_gthrxn_in
  !_gthrxp_in
  !_gtwiz_reset_clk_freerun_in
  !_gtwiz_reset_all_in
  !_gtwiz_reset_rx_datapath_in
  !gtwiz_userdata_tx_in
  !_txctrl2_in
  !_gtrefclk0_in
  !_
  !_
  !_
  !_
  !_
  !_ =
    ( SimOnly gtwiz_userdata_tx_in
    , pure 0
    , pure 0
    , clockGen
    , clockGen
    , unSimOnly gthrx_in
    , pure 1
    , pure 1
    , pure 1
    , pure 1
    , pure 0
    , pure 0
    , pure 0
    , pure 0
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
