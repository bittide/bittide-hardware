-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE RecordWildCards #-}

module Bittide.Ethernet.Mac where

import Clash.Explicit.Prelude hiding ((:<))

import Bittide.Extra.Maybe
import Bittide.Wishbone
import Clash.Annotations.Primitive
import Clash.Class.BitPackC
import Clash.Cores.Xilinx.Ethernet.Gmii.Internal
import Data.Constraint.Nat.Extra
import Data.Constraint.Nat.Lemmas
import Data.List.Infinite (Infinite ((:<)), (...))
import Data.Maybe
import Data.String.Interpolate (__i)
import GHC.Stack (HasCallStack)
import Protocols.Axi4.Stream
import Protocols.Internal
import Protocols.MemoryMap
import Protocols.MemoryMap.TypeDescription.TH
import Protocols.Wishbone

import qualified Clash.Prelude as CP

data EthMacStatus = EthMacStatus
  { txFifoUnderflow :: "txFifoUnderflow" ::: Bool
  , txFifoOverflow :: "txFifoOverflow" ::: Bool
  , txFifoBadFrame :: "txFifoBadFrame" ::: Bool
  , txFifoGoodFrame :: "txFifoGoodFrame" ::: Bool
  , rxBadFrame :: "rxBadFrame" ::: Bool
  , rxBadFcs :: "rxBadFcs" ::: Bool
  , rxFifoOverflow :: "rxFifoOverflow" ::: Bool
  , rxFifoBadFrame :: "rxFifoBadFrame" ::: Bool
  , rxFifoGoodFrame :: "rxFifoGoodFrame" ::: Bool
  }
  deriving (Generic, NFDataX, BitPack, BitPackC)
deriveTypeDescription ''EthMacStatus

{- | Wishbone peripheral that keeps track of the status flags of the Ethernet MAC.
Every cycle that a flag is set, will be counted with a counter. The width of the counters
is configurable using the first `SNat counterWidth` argument.
-}
macStatusInterfaceWb ::
  forall dom aw nBytes counterWidth.
  ( CP.HiddenClockResetEnable dom
  , HasCallStack
  , KnownNat nBytes
  , KnownNat aw
  , 1 <= nBytes
  , counterWidth <= nBytes * 8
  ) =>
  -- | Number of bits of the counters
  SNat counterWidth ->
  Circuit
    (ToConstBwd Mm, (Wishbone dom 'Standard aw (Bytes nBytes), CSignal dom EthMacStatus))
    ()
macStatusInterfaceWb SNat = case (cancelMulDiv @nBytes @8) of
  Dict -> withMemoryMap mm $ Circuit circuitGo
   where
    circuitGo ((wbM2S, macStatus), _) = ((wbS2M, ()), ())
     where
      (_, wbS2M) = unbundle $ wbToVec <$> counts <*> wbM2S
      pulses = fmap bitCoerce macStatus :: Signal dom (Vec (BitSize EthMacStatus) Bool)
      extendF = signExtend @_ @_ @(nBytes * 8 - counterWidth)
      counts = bundle $ fmap (fmap (pack . extendF)) $ pulseCount <$> unbundle pulses
      pulseCount :: Signal dom Bool -> Signal dom (Unsigned counterWidth)
      pulseCount pulse = cnt
       where
        cnt = CP.regEn 0 (CP.isRising True pulse) (succ <$> cnt)

    mm =
      MemoryMap
        { tree = DeviceInstance locCaller "MacStatus"
        , deviceDefs = deviceSingleton deviceDef
        }
    deviceDef =
      DeviceDefinition
        { tags = []
        , registers =
            [ NamedLoc
                { name = Name "status" ""
                , loc = locHere
                , value =
                    Register
                      { fieldType = regType @EthMacStatus
                      , address = 0x0
                      , access = ReadOnly
                      , tags = []
                      , reset = Nothing
                      }
                }
            ]
        , deviceName = Name "MacStatus" ""
        , definitionLoc = locHere
        }

ethMac1GFifoC ::
  ( KnownDomain sys
  , KnownDomain tx
  , KnownDomain rx
  ) =>
  -- Configuration

  -- | TX FIFO depth
  SNat txFifoDepth ->
  -- | RX FIFO depth
  SNat rxFifoDepth ->
  -- Clocks and resets

  -- | Logic clock
  Clock sys ->
  -- | Logic reset
  Reset sys ->
  -- | TX clock
  Clock tx ->
  -- | TX reset
  Reset tx ->
  -- | RX clock
  Clock rx ->
  -- | RX reset
  Reset rx ->
  -- | Mii select
  Signal rx Bool ->
  -- | TX Clock enable
  Signal tx Bool ->
  -- | RX Clock enable
  Signal rx Bool ->
  Circuit
    (Axi4Stream sys ('Axi4StreamConfig 1 0 0) Bool, CSignal rx Gmii)
    (Axi4Stream sys ('Axi4StreamConfig 1 0 0) Bool, CSignal tx Gmii, CSignal sys EthMacStatus)
ethMac1GFifoC
  txFifoDepth
  rxFifoDepth
  sysClk
  sysRst
  txClk
  txRst
  rxClk
  rxRst
  miiSel
  txClkEna
  rxClkEna = Circuit go
   where
    go ((axiTxM2S, gmiiRx), (axiRxS2M, _, _)) = ((axiTxS2M, ()), (axiRxM2S, gmiiTx, ethStatus))
     where
      (axiTxS2M, axiRxM2S, gmiiTx, ethStatus) =
        ethMac1GFifo
          txFifoDepth
          rxFifoDepth
          sysClk
          sysRst
          txClk
          txRst
          rxClk
          rxRst
          miiSel
          txClkEna
          rxClkEna
          axiTxM2S
          gmiiRx
          axiRxS2M

ethMac1GFifo ::
  ( KnownDomain sys
  , KnownDomain tx
  , KnownDomain rx
  ) =>
  -- Configuration

  -- | TX FIFO depth
  SNat txFifoDepth ->
  -- | RX FIFO depth
  SNat rxFifoDepth ->
  -- Clocks and resets

  -- | Logic clock
  Clock sys ->
  -- | Logic reset
  Reset sys ->
  -- | TX clock
  Clock tx ->
  -- | TX reset
  Reset tx ->
  -- | RX clock
  Clock rx ->
  -- | RX reset
  Reset rx ->
  -- | Mii select
  Signal rx Bool ->
  -- | TX Clock enable
  Signal tx Bool ->
  -- | RX Clock enable
  Signal rx Bool ->
  -- TX Axi inputs
  Signal sys (Maybe (Axi4StreamM2S ('Axi4StreamConfig 1 0 0) Bool)) ->
  -- RX Gmii inputs
  Signal rx Gmii ->
  -- RX Axi inputs
  Signal sys Axi4StreamS2M ->
  ( -- TX Axi outputs
    Signal sys Axi4StreamS2M
  , -- RX Axi outputs
    Signal sys (Maybe (Axi4StreamM2S ('Axi4StreamConfig 1 0 0) Bool))
  , -- GMII outputs
    Signal tx Gmii
  , -- TX Status
    Signal sys EthMacStatus
  )
ethMac1GFifo
  txFifoDepth
  rxFifoDepth
  sysClk
  sysRst
  txClk
  txRst
  rxClk
  rxRst
  miiSel
  txClkEna
  rxClkEna
  txAxiM2S
  rxGmii
  rxAxiS2M =
    (txAxiS2M, rxAxiM2S, gmiiTx, ethStatus)
   where
    txAxiS2M = Axi4StreamS2M <$> txAxiReady

    txAxiData = _tdata . fromJust <$> txAxiM2S
    txAxiKeep = _tkeep . fromJust <$> txAxiM2S
    txAxiValid = isJust <$> txAxiM2S
    txAxiLast = _tlast . fromJust <$> txAxiM2S
    txAxiUser = _tuser . fromJust <$> txAxiM2S

    rxAxiReady = _tready <$> rxAxiS2M
    gmiiRxData' = bitCoerce . gmiiData <$> rxGmii
    gmiiRxValid' = bitCoerce . gmiiValid <$> rxGmii
    gmiiRxError' = bitCoerce . gmiiError <$> rxGmii

    -- Instantiate the blackbox
    gmiiTx = Gmii <$> gmiiTxData' <*> fmap bitCoerce gmiiTxEnable' <*> fmap bitCoerce gmiiTxError'
    ( txAxiReady
      , rxAxiData
      , rxAxiKeep
      , rxAxiValid
      , rxAxiLast
      , rxAxiUser
      , gmiiTxData'
      , gmiiTxEnable'
      , gmiiTxError'
      , txFifoUnderflow
      , txFifoOverflow
      , txFifoBadFrame
      , txFifoGoodFrame
      , rxBadFrame
      , rxBadFcs
      , rxFifoOverflow
      , rxFifoBadFrame
      , rxFifoGoodFrame
      ) =
        ethMac1GFifoBb
          txFifoDepth
          rxFifoDepth
          sysClk
          sysRst
          txClk
          txRst
          rxClk
          rxRst
          miiSel
          txClkEna
          rxClkEna
          txAxiData
          txAxiKeep
          txAxiValid
          txAxiLast
          txAxiUser
          gmiiRxData'
          gmiiRxValid'
          gmiiRxError'
          rxAxiReady

    ethStatus =
      makeEthStatus
        <$> txFifoUnderflow
        <*> txFifoOverflow
        <*> txFifoBadFrame
        <*> txFifoGoodFrame
        <*> rxBadFrame
        <*> rxBadFcs
        <*> rxFifoOverflow
        <*> rxFifoBadFrame
        <*> rxFifoGoodFrame

    rxAxiM2S = makeAxi <$> rxAxiValid <*> rxAxiData <*> rxAxiKeep <*> rxAxiUser <*> rxAxiLast

{- | Utility function to create an Axi4StreamM2S from it's components.
Exists to explicitly show the order of the arguments closely to the code that uses it.
-}
makeAxi ::
  (KnownNat n) =>
  Bool ->
  Vec n (Unsigned 8) ->
  Vec n Bool ->
  Bool ->
  Bool ->
  Maybe (Axi4StreamM2S ('Axi4StreamConfig n 0 0) Bool)
makeAxi _tvalid _tdata _tkeep _tuser _tlast = orNothing _tvalid Axi4StreamM2S{..}
 where
  _tid = 0
  _tdest = 0
  _tstrb = repeat True

{- | Utility function to create the EthMacStatus from it's components.
Exists to explicitly show the order of the arguments closely to the code that uses it.
-}
makeEthStatus ::
  Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> EthMacStatus
makeEthStatus
  txFifoUnderflow
  txFifoOverflow
  txFifoBadFrame
  txFifoGoodFrame
  rxBadFrame
  rxBadFcs
  rxFifoOverflow
  rxFifoBadFrame
  rxFifoGoodFrame = EthMacStatus{..}

-- | 1G Ethernet MAC with TX and RX FIFOs
ethMac1GFifoBb ::
  ( KnownDomain sys
  , KnownDomain tx
  , KnownDomain rx
  ) =>
  -- Configuration

  -- | TX FIFO depth
  SNat txFifoDepth ->
  -- | RX FIFO depth
  SNat rxFifoDepth ->
  -- Clocks and resets

  -- | Logic clock
  Clock sys ->
  -- | Logic reset
  Reset sys ->
  -- | TX clock
  Clock tx ->
  -- | TX reset
  Reset tx ->
  -- | RX clock
  Clock rx ->
  -- | RX reset
  Reset rx ->
  -- | Mii select
  Signal rx Bool ->
  -- | TX Clock enable
  Signal tx Bool ->
  -- | RX Clock enable
  Signal rx Bool ->
  -- TX Axi inputs

  -- | TX Axi data
  Signal sys (Vec 1 (Unsigned 8)) ->
  -- | TX Axi keep
  Signal sys (Vec 1 Bool) ->
  -- | TX Axi valid
  Signal sys Bool ->
  -- | TX Axi last
  Signal sys Bool ->
  -- | TX Axi user
  Signal sys Bool ->
  -- RX Gmii inputs

  -- | RX Gmii data
  Signal rx (BitVector 8) ->
  -- | RX Gmii valid
  Signal rx Bool ->
  -- | RX Gmii error
  Signal rx Bool ->
  -- RX Axi inputs

  -- | RX Axi ready
  Signal sys Bool ->
  ( -- TX Axi outputs
    -- \| TX Axi ready
    Signal sys Bool
  , -- RX Axi outputs
    -- \| RX Axi data
    Signal sys (Vec 1 (Unsigned 8))
  , -- \| RX Axi keep
    Signal sys (Vec 1 Bool)
  , -- \| RX Axi valid
    Signal sys Bool
  , -- \| RX Axi last
    Signal sys Bool
  , -- \| RX Axi user
    Signal sys Bool
  , -- GMII outputs
    -- \| GMII TX data
    Signal tx (BitVector 8)
  , -- \| GMII TX enable
    Signal tx Bool
  , -- \| GMII TX error
    Signal tx Bool
  , -- TX Status
    -- \| TX FIFO underflow
    Signal sys Bool
  , -- \| TX FIFO overflow
    Signal sys Bool
  , -- \| TX FIFO bad frame
    Signal sys Bool
  , -- \| TX FIFO good frame
    Signal sys Bool
  , -- RX Status
    -- \| RX error bad frame
    Signal sys Bool
  , -- \| RX error bad FCS
    Signal sys Bool
  , -- \| RX FIFO overflow
    Signal sys Bool
  , -- \| RX FIFO bad frame
    Signal sys Bool
  , -- \| RX FIFO good frame
    Signal sys Bool
  )
ethMac1GFifoBb SNat SNat !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ =
  (err, err, err, err, err, err, err, err, err, err, err, err, err, err, err, err, err, err)
 where
  err :: forall dom a. (NFDataX a) => Signal dom a
  err = pure $ deepErrorX "simulation model not implemented"
{-# OPAQUE ethMac1GFifoBb #-}
{-# ANN ethMac1GFifoBb hasBlackBox #-}
{-# ANN
  ethMac1GFifoBb
  ( let
      ( _sys
          :< _tx
          :< _rx
          :< txFifoDepth
          :< rxFifoDepth
          :< sysClk
          :< sysRst
          :< txClk
          :< txRst
          :< rxClk
          :< rxRst
          :< miiSel
          :< txClkEna
          :< rxClkEna
          :< txAxiData
          :< txAxiKeep
          :< txAxiValid
          :< txAxiLast
          :< txAxiUser
          :< gmiiRxData
          :< gmiiRxValid
          :< gmiiRxError
          :< rxAxiReady
          :< txAxiReady
          :< rxAxiData
          :< rxAxiKeep
          :< rxAxiValid
          :< rxAxiLast
          :< rxAxiUser
          :< gmiiTxData
          :< gmiiTxEn
          :< gmiiTxErr
          :< txFifoUnderflow
          :< txFifoOverflow
          :< txFifoBadFrame
          :< txFifoGoodFrame
          :< rxBadFrame
          :< rxBadFcs
          :< rxFifoOverflow
          :< rxFifoBadFrame
          :< rxFifoGoodFrame
          :< _
        ) = ((0 :: Int) ...)
      funcName = 'ethMac1GFifoBb
     in
      InlineYamlPrimitive
        [Verilog]
        [__i|
      BlackBox:
        kind: Declaration
        name: #{funcName}
        template: |-
          wire ~GENSYM[txAxiReady][#{txAxiReady}];
          wire [7:0] ~GENSYM[rxAxiData][#{rxAxiData}];
          wire ~GENSYM[rxAxiKeep][#{rxAxiKeep}];
          wire ~GENSYM[rxAxiValid][#{rxAxiValid}];
          wire ~GENSYM[rxAxiLast][#{rxAxiLast}];
          wire ~GENSYM[rxAxiUser][#{rxAxiUser}];
          wire [7:0] ~GENSYM[gmiiTxData][#{gmiiTxData}];
          wire ~GENSYM[gmiiTxEn][#{gmiiTxEn}];
          wire ~GENSYM[gmiiTxErr][#{gmiiTxErr}];
          wire ~GENSYM[txFifoUnderflow][#{txFifoUnderflow}];
          wire ~GENSYM[txFifoOverflow][#{txFifoOverflow}];
          wire ~GENSYM[txFifoBadFrame][#{txFifoBadFrame}];
          wire ~GENSYM[txFifoGoodFrame][#{txFifoGoodFrame}];
          wire ~GENSYM[rxBadFrame][#{rxBadFrame}];
          wire ~GENSYM[rxBadFcs][#{rxBadFcs}];
          wire ~GENSYM[rxFifoOverflow][#{rxFifoOverflow}];
          wire ~GENSYM[rxFifoBadFrame][#{rxFifoBadFrame}];
          wire ~GENSYM[rxFifoGoodFrame][#{rxFifoGoodFrame}];
          assign ~RESULT =
            { ~SYM[#{txAxiReady}]
            , ~SYM[#{rxAxiData}]
            , ~SYM[#{rxAxiKeep}]
            , ~SYM[#{rxAxiValid}]
            , ~SYM[#{rxAxiLast}]
            , ~SYM[#{rxAxiUser}]
            , ~SYM[#{gmiiTxData}]
            , ~SYM[#{gmiiTxEn}]
            , ~SYM[#{gmiiTxErr}]
            , ~SYM[#{txFifoUnderflow}]
            , ~SYM[#{txFifoOverflow}]
            , ~SYM[#{txFifoBadFrame}]
            , ~SYM[#{txFifoGoodFrame}]
            , ~SYM[#{rxBadFrame}]
            , ~SYM[#{rxBadFcs}]
            , ~SYM[#{rxFifoOverflow}]
            , ~SYM[#{rxFifoBadFrame}]
            , ~SYM[#{rxFifoGoodFrame}]
            };
          eth_mac_1g_fifo \#(
            .TX_FIFO_DEPTH(~ARG[#{txFifoDepth}]),
            .RX_FIFO_DEPTH(~ARG[#{rxFifoDepth}])
          )
          eth_mac_1g_fifo_inst
          (
            .rx_clk(~ARG[#{rxClk}]),
            .rx_rst(~ARG[#{rxRst}]),
            .tx_clk(~ARG[#{txClk}]),
            .tx_rst(~ARG[#{txRst}]),
            .logic_clk(~ARG[#{sysClk}]),
            .logic_rst(~ARG[#{sysRst}]),
            .tx_axis_tdata(~ARG[#{txAxiData}]),
            .tx_axis_tkeep(~ARG[#{txAxiKeep}]),
            .tx_axis_tvalid(~ARG[#{txAxiValid}]),
            .tx_axis_tlast(~ARG[#{txAxiLast}]),
            .tx_axis_tuser(~ARG[#{txAxiUser}]),
            .rx_axis_tready(~ARG[#{rxAxiReady}]),
            .tx_axis_tready(~SYM[#{txAxiReady}]),
            .rx_axis_tdata(~SYM[#{rxAxiData}]),
            .rx_axis_tkeep(~SYM[#{rxAxiKeep}]),
            .rx_axis_tvalid(~SYM[#{rxAxiValid}]),
            .rx_axis_tlast(~SYM[#{rxAxiLast}]),
            .rx_axis_tuser(~SYM[#{rxAxiUser}]),
            .gmii_rxd(~ARG[#{gmiiRxData}]),
            .gmii_rx_dv(~ARG[#{gmiiRxValid}]),
            .gmii_rx_er(~ARG[#{gmiiRxError}]),
            .gmii_txd(~SYM[#{gmiiTxData}]),
            .gmii_tx_en(~SYM[#{gmiiTxEn}]),
            .gmii_tx_er(~SYM[#{gmiiTxErr}]),
            .rx_clk_enable(~ARG[#{rxClkEna}]),
            .tx_clk_enable(~ARG[#{txClkEna}]),
            .rx_mii_select(~ARG[#{miiSel}]),
            .tx_mii_select(~ARG[#{miiSel}]),
            .tx_error_underflow(~SYM[#{txFifoUnderflow}]),
            .tx_fifo_overflow(~SYM[#{txFifoOverflow}]),
            .tx_fifo_bad_frame(~SYM[#{txFifoBadFrame}]),
            .tx_fifo_good_frame(~SYM[#{txFifoGoodFrame}]),
            .rx_error_bad_frame(~SYM[#{rxBadFrame}]),
            .rx_error_bad_fcs(~SYM[#{rxBadFcs}]),
            .rx_fifo_overflow(~SYM[#{rxFifoOverflow}]),
            .rx_fifo_bad_frame(~SYM[#{rxFifoBadFrame}]),
            .rx_fifo_good_frame(~SYM[#{rxFifoGoodFrame}]),

            .cfg_ifg(8'd12),
            .cfg_tx_enable(1'b1),
            .cfg_rx_enable(1'b1)
          );
        |]
  )
  #-}
