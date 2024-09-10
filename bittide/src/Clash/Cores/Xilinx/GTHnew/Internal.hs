-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module Clash.Cores.Xilinx.GTHnew.Internal where

import Clash.Explicit.Prelude

import Clash.Annotations.Primitive (Primitive (InlineYamlPrimitive), hasBlackBox)
import Data.String.Interpolate (__i)

import Clash.Cores.Xilinx.GTHnew.BlackBoxes
-- import Clash.Cores.Xilinx.GTH.Internal qualified as Old
import Clash.Cores.Xilinx.Extra (bufgGt)

type TX_DATA_WIDTH = 64
type RX_DATA_WIDTH = 64

type GthCoreNew txUser txUser2 rxUser rxUser2 refclk0 freerun txS rxS serializedData =
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

  -- , "gtwiz_userclk_tx_usrclk2_out" ::: Clock txUser2
  -- , "gtwiz_userclk_rx_usrclk2_out" ::: Clock rxUser2
  , "txoutclk_out" ::: Clock txUser -- TODO check domain
  , "rxoutclk_out" ::: Clock rxUser -- TODO check domain

  , "gtwiz_userdata_rx_out" ::: Signal rxUser2 (BitVector RX_DATA_WIDTH)
  , "gtwiz_reset_tx_done_out" ::: Signal txUser2 (BitVector 1)
  , "gtwiz_reset_rx_done_out" ::: Signal rxUser2 (BitVector 1)

  -- , "gtwiz_userclk_tx_active_out" ::: Signal txUser2 (BitVector 1)

  , "rxctrl0_out" ::: Signal rxUser2 (BitVector 16)
  , "rxctrl1_out" ::: Signal rxUser2 (BitVector 16)
  , "rxctrl2_out" ::: Signal rxUser2 (BitVector 8)
  , "rxctrl3_out" ::: Signal rxUser2 (BitVector 8)
  )

gthCoreNew :: GthCoreNew txUser txUser2 rxUser rxUser2 refclk0 freerun txS rxS (BitVector 1)
gthCoreNew
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
    )
{-# OPAQUE gthCoreNew #-}
{-# ANN gthCoreNew hasBlackBox #-}
{-# ANN
  gthCoreNew
  ( let primName = 'gthCoreNew
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

-- Same old version, but with extra txUser and rxUser domains?
type GthCore txUser txUser2 rxUser rxUser2 refclk0 freerun txS rxS serializedData =
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
gthCore :: GthCore txUser txUser2 rxUser rxUser2 refclk0 freerun txS rxS (BitVector 1)
gthCore
  channel
  refClkSpec
  gthrxn_in
  gthrxp_in
  gtwiz_reset_clk_freerun_in
  gtwiz_reset_all_in
  gtwiz_reset_rx_datapath_in
  gtwiz_userdata_tx_in
  txctrl2_in
  gtrefclk0_in
  -- txusrclk_in
  -- txusrclk2_in
  -- gtwiz_userclk_tx_active_in
  -- rxusrclk_in
  -- rxusrclk2_in
  -- gtwiz_userclk_rx_active_in
   = ( gthtxn_out
      , gthtxp_out
      , txUsrClk2
      , rxUsrClk2
      , gtwiz_userdata_rx_out
      , gtwiz_reset_tx_done_out
      , gtwiz_reset_rx_done_out
      , undefined
      , rxctrl0_out
      , rxctrl1_out
      , rxctrl2_out
      , rxctrl3_out
      )
 where
  (txUsrClk,txUsrClk2,txUsrClkActive) = gthUserClockNetwork txoutclk_out txUsrClkRst
  (rxUsrClk,rxUsrClk2,rxUsrClkActive) = gthUserClockNetwork rxoutclk_out rxUsrClkRst
  ( gthtxn_out
   , gthtxp_out
   , txoutclk_out
   , rxoutclk_out
   , gtwiz_userdata_rx_out
   , gtwiz_reset_tx_done_out
   , gtwiz_reset_rx_done_out
   , rxctrl0_out
   , rxctrl1_out
   , rxctrl2_out
   , rxctrl3_out
   ) = gthCoreNew
    channel
    refClkSpec
    gthrxn_in
    gthrxp_in
    gtwiz_reset_clk_freerun_in
    gtwiz_reset_all_in
    gtwiz_reset_rx_datapath_in
    gtwiz_userdata_tx_in
    txctrl2_in
    gtrefclk0_in
    txUsrClk
    txUsrClk2
    txUsrClkActive
    rxUsrClk
    rxUsrClk2
    rxUsrClkActive



-- | This mimics what PG182 calls the "[RX,TX] User Clocking Network Helper Block"
--
-- It has a hardcoded to do no division for  @usrclk@ and divide by 2 for @usrclk2@.
-- So it'll only when the external RX/TX GTH interfaces uses twice the width of the internal width.
-- See: https://docs.amd.com/r/en-US/pg182-gtwizard-ultrascale/Transmitter-User-Clocking-Network-Helper-Block-Ports
gthUserClockNetwork ::
  forall user user2.
  (KnownDomain user, KnownDomain user2) =>
  Clock user ->
  Reset user2 ->
  (Clock user, Clock user2, Signal user2 (BitVector 1))
gthUserClockNetwork clkIn rstIn =
  (usrClk, usrClk2, active)
 where
  rstIn1 :: Reset user
  rstIn1 = unsafeSynchronizerReset usrClk2 usrClk rstIn
  usrClk  = bufgGt d0 clkIn rstIn1
  usrClk2 :: Clock user2
  usrClk2 = bufgGt d1 clkIn rstIn1
  active = reg (reg (pure 1))
  reg = register usrClk2 rstIn enableGen 0

  unsafeSynchronizerReset :: (KnownDomain dom1, KnownDomain dom2) => Clock dom1 -> Clock dom2 -> Reset dom1 -> Reset dom2
  unsafeSynchronizerReset clkIn clkOut rstIn = unsafeFromActiveHigh $ unsafeSynchronizer clkIn clkOut (unsafeToActiveHigh rstIn)
