-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE RecordWildCards #-}
module Bittide.Ethernet.Gmii where

import Clash.Prelude hiding (tlast, (:<))

import Bittide.Extra.Maybe
import Clash.Annotations.Primitive
import Clash.Cores.Xilinx.Ethernet.Gmii.Internal
import Data.List.Infinite (Infinite((:<)), (...))
import Data.Maybe
import Data.String.Interpolate (__i)
import Protocols
import Protocols.Axi4.Stream

-- TODO: The arguments of the black boxes seem to match up, but the instances
-- have not been tested.`
axiGmiiRxC ::
  KnownDomain dom =>
  Clock dom ->
  Reset dom ->
  Signal dom Bool ->
  Signal dom Bool ->
  Signal dom Bool ->
  Circuit (CSignal dom Gmii) (Axi4Stream dom ('Axi4StreamConfig 1 0 0) AxiGmiiUser)
axiGmiiRxC clk rst clkEnable cfgEnable miiSelect = Circuit go
 where
  go (gmiiRx, _axi2M) = (pure (), axiM2S)
   where
    axiM2S = axiGmiiRx clk rst clkEnable cfgEnable miiSelect gmiiRx

data AxiGmiiUser = AxiGmiiUser
  { gmiiRxStartPacket :: "StartPacket" ::: Bool
  , gmiiRxBadFrame    :: "BadFrame" ::: Bool
  , gmiiRxBadFcs      :: "BadFcs" ::: Bool
  , gmiiRxTUser       :: "TUser" ::: Bool
  }

axiGmiiRx ::
  forall dom .
  ( KnownDomain dom
  ) =>
  -- | Clock
  Clock dom ->
  -- | Reset
  Reset dom ->
  -- | Clock enable
  Signal dom Bool ->
  -- | Cfg rx enable
  Signal dom Bool ->
  -- | MII select
  Signal dom Bool ->
  -- | Incoming GMII signal
  Signal dom Gmii ->
  -- | Received Gmii data with control signals
  Signal dom (Maybe (Axi4StreamM2S ('Axi4StreamConfig 1 0 0) AxiGmiiUser))
axiGmiiRx clk rst clkEnable cfgEnable miiSelect gmiiRx = fmap f $ bundle axiM2S
 where
  axiM2S =
    axiGmiiRxBb @_ @8 clk rst cfgEnable clkEnable miiSelect
    (pack . gmiiData <$> gmiiRx :: Signal dom (BitVector 8)) (bitCoerce . gmiiValid <$> gmiiRx) (bitCoerce . gmiiError  <$> gmiiRx)

  f (unpack -> _tdata, _tvalid, _tlast, gmiiRxTUser, gmiiRxStartPacket, gmiiRxBadFrame, gmiiRxBadFcs) =
    orNothing _tvalid Axi4StreamM2S{..}
   where
    _tuser = AxiGmiiUser{..}
    _tkeep = True :> Nil
    _tstrb = True :> Nil
    _tid = 0
    _tdest = 0

-- | Contains the blackbox for the GMII to Axi bridge
axiGmiiRxBb ::
  ( KnownDomain dom
  , KnownNat dataWidth
  ) =>
  -- | Clock
  Clock dom ->
  -- | Reset
  Reset dom ->
  -- | Clock enable
  Signal dom Bool ->
  -- | Cfg rx enable
  Signal dom Bool ->
  -- | MII select
  Signal dom Bool ->
  -- | Incoming GMII signal
  Signal dom (BitVector dataWidth) ->
  -- | GMII data valid
  Signal dom Bool ->
  -- | GMII error
  Signal dom Bool ->
  -- |
  -- 1. Axi4 stream data
  -- 2. Axi4 stream valid
  -- 3. Axi4 stream tlast
  -- 4. Axi4 stream tuser (packet invalid)
  -- 5. start packet
  -- 6. bad frame
  -- 7. bad fcs
  ( Signal dom (BitVector dataWidth)
  , Signal dom Bool
  , Signal dom Bool
  , Signal dom Bool
  , Signal dom Bool
  , Signal dom Bool
  , Signal dom Bool
  )
axiGmiiRxBb !_ !_ !_ !_ !_ !_ !_ !_ = deepErrorX "No simulation model for axiGmiiBb"
{-# NOINLINE axiGmiiRxBb #-}
{-# ANN axiGmiiRxBb hasBlackBox #-}
{-# ANN axiGmiiRxBb (
  let
    ( _dom
      :< dataWidth
      :< clk
      :< rst
      :< clkEn
      :< cfgRxEnable
      :< miiSel
      :< gmiiData
      :< gmiiValid
      :< gmiiErr
      :< rx_axidata
      :< rx_axivalid
      :< rx_axilast
      :< rx_axiuser
      :< rx_start_packet
      :< rx_error_bad_frame
      :< rx_error_bad_fcs
      :< _
     ) = ((0::Int)...)
    funcName = 'axiGmiiRxBb
  in
    InlineYamlPrimitive [Verilog] [__i|
      BlackBox:
        kind: Declaration
        name: #{funcName}
        template: |-
          wire [~ARG[#{dataWidth}]:0] ~GENSYM[rx_axidata][#{rx_axidata}];
          wire ~GENSYM[rx_axivalid][#{rx_axivalid}];
          wire ~GENSYM[rx_axilast][#{rx_axilast}];
          wire ~GENSYM[rx_axiuser][#{rx_axiuser}];
          wire ~GENSYM[rx_start_packet][#{rx_start_packet}];
          wire ~GENSYM[rx_error_bad_frame][#{rx_error_bad_frame}];
          wire ~GENSYM[rx_error_bad_fcs][#{rx_error_bad_fcs}];
          assign ~RESULT =
            { ~SYM[#{rx_axidata}]
            , ~SYM[#{rx_axivalid}]
            , ~SYM[#{rx_axilast}]
            , ~SYM[#{rx_axiuser}]
            , ~SYM[#{rx_start_packet}]
            , ~SYM[#{rx_error_bad_frame}]
            , ~SYM[#{rx_error_bad_fcs}]};
          axi_gmii_rx \#(
            .DATA_WIDTH(~ARG[#{dataWidth}])
          )
          axi_gmii_rx_inst (
              .clk(~ARG[#{clk}]),
              .rst(~ARG[#{rst}]),
              .gmii_rxd(~ARG[#{gmiiData}]),
              .gmii_rx_dv(~ARG[#{gmiiValid}]),
              .gmii_rx_er(~ARG[#{gmiiErr}]),
              .m_axidata(~SYM[#{rx_axidata}]),
              .m_axivalid(~SYM[#{rx_axivalid}]),
              .m_axilast(~SYM[#{rx_axilast}]),
              .m_axiuser(~SYM[#{rx_axiuser}]),
              .clk_enable(~ARG[#{clkEn}]),
              .mii_select(~ARG[#{miiSel}]),
              .cfg_rx_enable(~ARG[#{cfgRxEnable}]),
              .start_packet(~SYM[#{rx_start_packet}]),
              .error_bad_frame(~SYM[#{rx_error_bad_frame}]),
              .error_bad_fcs(~SYM[#{rx_error_bad_fcs}])
          );
  |]) #-}

axiGmiiTxC ::
  KnownDomain dom =>
  -- | Clock
  Clock dom ->
  -- | Reset
  Reset dom ->
  -- | Clock enable
  Signal dom Bool ->
  -- | Cfg enable
  Signal dom Bool ->
  -- | MII select
  Signal dom Bool ->
  Circuit
    (Axi4Stream dom ('Axi4StreamConfig 1 0 0) Bool)
    (CSignal dom Gmii)
axiGmiiTxC clk rst clkEnable cfgEnable miiSelect = Circuit go
 where
  go (axiM2S, _) = (axiS2M, gmiiTx)
   where
    (axiS2M, gmiiTx, _, _) = axiGmiiTx clk rst clkEnable cfgEnable miiSelect axiM2S


axiGmiiTx ::
  KnownDomain dom =>
  -- | Clock
  Clock dom ->
  -- | Reset
  Reset dom ->
  -- | Clock enable
  Signal dom Bool ->
  -- | Cfg enable
  Signal dom Bool ->
  -- | MII select
  Signal dom Bool ->
  -- | AXI data
  Signal dom (Maybe (Axi4StreamM2S ('Axi4StreamConfig 1 0 0) Bool)) ->
  -- |
  -- 1. AXI ready
  -- 2. GMII bus
  -- 3. Start packet
  -- 4. Underflow
  ( Signal dom Axi4StreamS2M
  , Signal dom Gmii
  , Signal dom Bool
  , Signal dom Bool
  )
axiGmiiTx clk rst clkEnable cfgEnable miiSelect axiM2S =
  (axiS2M, gmiiTx, startPacket, underFlow)
 where
  axiS2M = Axi4StreamS2M <$> axiReady
  gmiiTx = Gmii <$> gmiiData <*> fmap bitCoerce gmiiEnable <*> fmap bitCoerce gmiiError

  (gmiiData, gmiiEnable, gmiiError, startPacket, underFlow, axiReady) =
    axiGmiiTxBb clk rst clkEnable cfgEnable miiSelect tdata tlast tvalid tuser

  (tdata, tlast, tuser) = unbundle $ (\Axi4StreamM2S{..} -> (pack _tdata, _tlast, _tuser)) . fromJust <$> axiM2S
  tvalid = isJust <$> axiM2S

-- | Contains the blackbox for the Axi to GMII bridge
axiGmiiTxBb ::
  ( KnownDomain dom
  , KnownNat dataWidth
  ) =>
  -- | Clock
  Clock dom ->
  -- | Reset
  Reset dom ->
  -- | Clock enable
  Signal dom Bool ->
  -- | Cfg enable
  Signal dom Bool ->
  -- | MII select
  Signal dom Bool ->
  -- | AXI stream data
  Signal dom (BitVector dataWidth) ->
  -- | AXI stream valid
  Signal dom Bool ->
  -- | AXI stream last
  Signal dom Bool ->
  -- | AXI stream user
  Signal dom Bool ->
  -- |
  -- 1. GMII data
  -- 2. GMII valid
  -- 3. GMII error
  -- 4. Start packet
  -- 5. Underflow
  -- 6. Axistream ready
  ( Signal dom (BitVector dataWidth)
  , Signal dom Bool
  , Signal dom Bool
  , Signal dom Bool
  , Signal dom Bool
  , Signal dom Bool
  )
axiGmiiTxBb !_ !_ !_ !_ !_ !_ !_ !_ = deepErrorX "No simulation model for axiGmiiBb"
{-# NOINLINE axiGmiiTxBb #-}
{-# ANN axiGmiiTxBb hasBlackBox #-}
{-# ANN axiGmiiTxBb (
  let
    (  _dom
     :< dataWidth
     :< clk
     :< rst
     :< clkEn
     :< cfgEnable
     :< miiSel
     :< axiData
     :< axiValid
     :< axiLast
     :< axiUser
     :< gmiiData
     :< gmiiEnable
     :< gmiiErr
     :< startPacket
     :< underFlow
     :< axiReady
     :< _
     ) = ((0::Int)...)
    funcName = 'axiGmiiTxBb
  in
    InlineYamlPrimitive [Verilog] [__i|
      BlackBox:
        kind: Declaration
        name: #{funcName}
        template: |-
          wire ~GENSYM[gmiiData][#{gmiiData}];
          wire ~GENSYM[gmiiEnable][#{gmiiEnable}];
          wire ~GENSYM[gmiiErr][#{gmiiErr}];
          wire ~GENSYM[startPacket][#{startPacket}];
          wire ~GENSYM[underFlow][#{underFlow}];
          wire ~GENSYM[axiReady][#{axiReady}];
          assign ~RESULT =
            { ~SYM[#{gmiiData}]
            , ~SYM[#{gmiiEnable}]
            , ~SYM[#{gmiiErr}]
            , ~SYM[#{startPacket}]
            , ~SYM[#{underFlow}]
            , ~SYM[#{axiReady}]
            }
          axi_gmiix \#(
            .DATA_WIDTH(~ARG[#{dataWidth}])
          )
          axi_gmiix_inst (
              .clk(~ARG[#{clk}]),
              .rst(~ARG[#{rst}]),
              .clk_enable(~ARG[#{clkEn}]),
              .cfg_tx_enable(~ARG[#{cfgEnable}]),
              .mii_select(~ARG[#{miiSel}]),
              .s_axis_tdata(~ARG[#{axiData}]),
              .s_axis_tvalid(~ARG[#{axiValid}]),
              .s_axis_tlast(~ARG[#{axiLast}]),
              .s_axis_tuser(~ARG[#{axiUser}]),
              .gmii_txd(~SYM[#{gmiiData}]),
              .gmii_tx_en(~SYM[#{gmiiEnable}]),
              .gmii_tx_er(~SYM[#{gmiiErr}]),
              .start_packet(~SYM[#{startPacket}]),
              .error_underflow(~SYM[#{underFlow}]),
              .s_axis_tready(~SYM[#{axiReady}])
          );
  |]) #-}
