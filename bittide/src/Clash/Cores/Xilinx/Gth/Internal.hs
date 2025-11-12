-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Clash.Cores.Xilinx.Gth.Internal where

import Clash.Explicit.Prelude

import Clash.Cores.Xilinx.Xpm.Cdc.Internal (
  ClockPort (..),
  NamedDiffClockPort (..),
  Param (..),
  Port (..),
  ResetPort (..),
  XilinxWizard (..),
  XilinxWizardOption (BoolOpt, IntegerOpt, StrOpt),
  inst,
  instConfig,
  instWithXilinxWizard,
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

data CoreInput txUser txUser2 rxUser rxUser2 refclk0 freerun rxS = CoreInput
  { channel :: String
  -- ^ Channel name
  , refClkSpec :: String
  -- ^ Reference clock specification
  , gthrxIn :: SimWire rxUser2
  -- ^ @gthrx_in@
  , gthrxnIn :: Wire rxS
  -- ^ @gthrxn_in@
  , gthrxpIn :: Wire rxS
  -- ^ @gthrxp_in@
  , gtwizResetClkFreerunIn :: Clock freerun
  -- ^ @gtwiz_reset_clk_freerun_in@
  , gtwizResetAllIn :: Reset freerun
  -- ^ @gtwiz_reset_all_in@
  , gtwizResetTxPllAndDatapathIn :: Reset freerun
  -- ^ @gtwiz_reset_tx_pll_and_datapath_in@
  , gtwizResetTxDatapathIn :: Reset freerun
  -- ^ @gtwiz_reset_tx_datapath_in@
  , gtwizResetRxPllAndDatapathIn :: Reset freerun
  -- ^ @gtwiz_reset_rx_pll_and_datapath_in@
  , gtwizResetRxDatapathIn :: Reset freerun
  -- ^ @gtwiz_reset_rx_datapath_in@
  , gtwizUserdataTxIn :: Signal txUser2 (BitVector TX_DATA_WIDTH)
  -- ^ @gtwiz_userdata_tx_in@
  , txctrl2In :: Signal txUser2 (BitVector (DivRU TX_DATA_WIDTH 8))
  -- ^ @txctrl2_in@
  , gtrefclk0In :: Clock refclk0
  -- ^ @gtrefclk0_in@
  , txusrclkIn :: Clock txUser
  -- ^ @txusrclk_in@
  , txusrclk2In :: Clock txUser2
  -- ^ @txusrclk2_in@
  , gtwizUserclkTxActiveIn :: Signal txUser2 (BitVector 1)
  -- ^ @gtwiz_userclk_tx_active_in@
  , rxusrclkIn :: Clock rxUser
  -- ^ @rxusrclk_in@
  , rxusrclk2In :: Clock rxUser2
  -- ^ @rxusrclk2_in@
  , gtwizUserclkRxActiveIn :: Signal rxUser2 (BitVector 1)
  -- ^ @gtwiz_userclk_rx_active_in@
  }

data CoreOutput txUser txUser2 rxUser rxUser2 txS = CoreOutput
  { gthtxOut :: SimWire txUser2
  -- ^ @gthtx_out@
  , gthtxnOut :: Wire txS
  -- ^ @gthtxn_out@
  , gthtxpOut :: Wire txS
  -- ^ @gthtxp_out@
  , txoutclkOut :: Clock txUser
  -- ^ @txoutclk_out@
  , rxoutclkOut :: Clock rxUser
  -- ^ @rxoutclk_out@
  , gtwizUserdataRxOut :: Signal rxUser2 (BitVector RX_DATA_WIDTH)
  -- ^ @gtwiz_userdata_rx_out@
  , gtwizResetTxDoneOut :: Signal txUser2 (BitVector 1)
  -- ^ @gtwiz_reset_tx_done_out@
  , gtwizResetRxDoneOut :: Signal rxUser2 (BitVector 1)
  -- ^ @gtwiz_reset_rx_done_out@
  , txpmaresetdoneOut :: Signal txUser (BitVector 1)
  -- ^ @txpmaresetdone_out@
  , rxpmaresetdoneOut :: Signal rxUser (BitVector 1)
  -- ^ @rxpmaresetdone_out@
  , rxctrl0Out :: Signal rxUser2 (BitVector 16)
  -- ^ @rxctrl0_out@
  , rxctrl1Out :: Signal rxUser2 (BitVector 16)
  -- ^ @rxctrl1_out@
  , rxctrl2Out :: Signal rxUser2 (BitVector 8)
  -- ^ @rxctrl2_out@
  , rxctrl3Out :: Signal rxUser2 (BitVector 8)
  -- ^ @rxctrl3_out@
  }

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
  CoreInput txUser txUser2 rxUser rxUser2 refclk0 freerun rxS ->
  CoreOutput txUser txUser2 rxUser rxUser2 txS

gthCore ::
  forall txUser txUser2 rxUser rxUser2 refclk0 freerun txS rxS.
  GthCore txUser txUser2 rxUser rxUser2 refclk0 freerun txS rxS
gthCore input
  | clashSimulation = simCore
  | otherwise = synthCore
 where
  simCore =
    CoreOutput
      { gthtxOut = SimOnly input.gtwizUserdataTxIn
      , gthtxnOut = pure 0
      , gthtxpOut = pure 0
      , txoutclkOut = clockGen
      , rxoutclkOut = clockGen
      , gtwizUserdataRxOut = unSimOnly input.gthrxIn
      , gtwizResetTxDoneOut = pure 1
      , gtwizResetRxDoneOut = pure 1
      , txpmaresetdoneOut = pure 1
      , rxpmaresetdoneOut = pure 1
      , rxctrl0Out = pure 0
      , rxctrl1Out = pure 0
      , rxctrl2Out = pure 0
      , rxctrl3Out = pure 0
      }

  synthCore =
    CoreOutput
      { gthtxOut = SimOnly input.gtwizUserdataTxIn
      , gthtxnOut
      , gthtxpOut
      , txoutclkOut
      , rxoutclkOut
      , gtwizUserdataRxOut
      , gtwizResetTxDoneOut = pack <$> gtwizResetTxDoneOut
      , gtwizResetRxDoneOut = pack <$> gtwizResetRxDoneOut
      , txpmaresetdoneOut = pack <$> txpmaresetdoneOut
      , rxpmaresetdoneOut = pack <$> rxpmaresetdoneOut
      , rxctrl0Out
      , rxctrl1Out
      , rxctrl2Out
      , rxctrl3Out
      }
   where
    ( unPort @(Port "gthtxn_out" txS (BitVector 1)) -> gthtxnOut
      , unPort @(Port "gthtxp_out" txS (BitVector 1)) -> gthtxpOut
      , unPort @(ClockPort "txoutclk_out" txUser) -> txoutclkOut
      , unPort @(ClockPort "rxoutclk_out" rxUser) -> rxoutclkOut
      , unPort @(Port "gtwiz_userdata_rx_out" rxUser2 (BitVector 64)) -> gtwizUserdataRxOut
      , unPort @(Port "gtwiz_reset_tx_done_out" txUser2 Bit) -> gtwizResetTxDoneOut
      , unPort @(Port "gtwiz_reset_rx_done_out" rxUser2 Bit) -> gtwizResetRxDoneOut
      , unPort @(Port "txpmaresetdone_out" txUser Bit) -> txpmaresetdoneOut
      , unPort @(Port "rxpmaresetdone_out" rxUser Bit) -> rxpmaresetdoneOut
      , unPort @(Port "rxctrl0_out" rxUser2 (BitVector 16)) -> rxctrl0Out
      , unPort @(Port "rxctrl1_out" rxUser2 (BitVector 16)) -> rxctrl1Out
      , unPort @(Port "rxctrl2_out" rxUser2 (BitVector 8)) -> rxctrl2Out
      , unPort @(Port "rxctrl3_out" rxUser2 (BitVector 8)) -> rxctrl3Out
      ) = go

    go =
      instWithXilinxWizard
        (instConfig "gtwizard_ultrascale")
        XilinxWizard
          { wiz_name = "gtwizard_ultrascale"
          , wiz_vendor = "xilinx.com"
          , wiz_library = "ip"
          , wiz_version = "1.7"
          , wiz_options =
              ("CONFIG.CHANNEL_ENABLE", StrOpt input.channel)
                :> ("CONFIG.LOCATE_COMMON", StrOpt "CORE")
                :> ("CONFIG.LOCATE_IN_SYSTEM_IBERT_CORE", StrOpt "NONE")
                :> ("CONFIG.LOCATE_RESET_CONTROLLER", StrOpt "CORE")
                :> ("CONFIG.LOCATE_RX_BUFFER_BYPASS_CONTROLLER", StrOpt "CORE")
                :> ("CONFIG.LOCATE_RX_USER_CLOCKING", StrOpt "EXAMPLE_DESIGN")
                :> ("CONFIG.LOCATE_TX_BUFFER_BYPASS_CONTROLLER", StrOpt "CORE")
                :> ("CONFIG.LOCATE_TX_USER_CLOCKING", StrOpt "EXAMPLE_DESIGN")
                :> ("CONFIG.LOCATE_USER_DATA_WIDTH_SIZING", StrOpt "CORE")
                :> ("CONFIG.FREERUN_FREQUENCY", StrOpt "125.0")
                :> ("CONFIG.RX_REFCLK_FREQUENCY", StrOpt "200")
                :> ("CONFIG.RX_REFCLK_SOURCE", StrOpt input.refClkSpec)
                :> ("CONFIG.RX_DATA_DECODING", StrOpt "8B10B")
                :> ("CONFIG.RX_INT_DATA_WIDTH", IntegerOpt 40)
                :> ("CONFIG.RX_LINE_RATE", StrOpt "10")
                :> ("CONFIG.RX_OUTCLK_SOURCE", StrOpt "RXOUTCLKPMA")
                :> ("CONFIG.RX_PLL_TYPE", StrOpt "CPLL")
                :> ("CONFIG.RX_PPM_OFFSET", IntegerOpt 200)
                :> ("CONFIG.RX_USER_DATA_WIDTH", IntegerOpt 64)
                :> ("CONFIG.RX_EQ_MODE", StrOpt "LPM")
                :> ("CONFIG.RX_COMMA_PRESET", StrOpt "K28.5")
                :> ("CONFIG.RX_COMMA_P_ENABLE", BoolOpt True)
                :> ("CONFIG.RX_COMMA_M_ENABLE", BoolOpt True)
                :> ("CONFIG.RX_COMMA_SHOW_REALIGN_ENABLE", BoolOpt False)
                :> ("CONFIG.TX_REFCLK_FREQUENCY", StrOpt "200")
                :> ("CONFIG.TX_REFCLK_SOURCE", StrOpt input.refClkSpec)
                :> ("CONFIG.TXPROGDIV_FREQ_VAL", IntegerOpt 250)
                :> ("CONFIG.TX_DATA_ENCODING", StrOpt "8B10B")
                :> ("CONFIG.TX_INT_DATA_WIDTH", IntegerOpt 40)
                :> ("CONFIG.TX_LINE_RATE", StrOpt "10")
                :> ("CONFIG.TX_PLL_TYPE", StrOpt "CPLL")
                :> ("CONFIG.TXPROGDIV_FREQ_SOURCE", StrOpt "CPLL")
                :> ("CONFIG.TX_USER_DATA_WIDTH", IntegerOpt 64)
                :> Nil
          }
        -- From CoreInput
        (Port @"gthrxn_in" input.gthrxnIn)
        (Port @"gthrxp_in" input.gthrxpIn)
        (ClockPort @"gtwiz_reset_clk_freerun_in" input.gtwizResetClkFreerunIn)
        (ResetPort @"gtwiz_reset_all_in" @ActiveHigh input.gtwizResetAllIn)
        ( ResetPort @"gtwiz_reset_tx_pll_and_datapath_in" @ActiveHigh
            input.gtwizResetTxPllAndDatapathIn
        )
        (ResetPort @"gtwiz_reset_tx_datapath_in" @ActiveHigh input.gtwizResetTxDatapathIn)
        ( ResetPort @"gtwiz_reset_rx_pll_and_datapath_in" @ActiveHigh
            input.gtwizResetRxPllAndDatapathIn
        )
        (ResetPort @"gtwiz_reset_rx_datapath_in" @ActiveHigh input.gtwizResetRxDatapathIn)
        (Port @"gtwiz_userdata_tx_in" input.gtwizUserdataTxIn)
        (Port @"txctrl2_in" input.txctrl2In)
        (ClockPort @"gtrefclk0_in" input.gtrefclk0In)
        (ClockPort @"txusrclk_in" input.txusrclkIn)
        (ClockPort @"txusrclk2_in" input.txusrclk2In)
        (Port @"gtwiz_userclk_tx_active_in" input.gtwizUserclkTxActiveIn)
        (ClockPort @"rxusrclk_in" input.rxusrclkIn)
        (ClockPort @"rxusrclk2_in" input.rxusrclk2In)
        (Port @"gtwiz_userclk_rx_active_in" input.gtwizUserclkRxActiveIn)
        (ClockPort @"drpclk_in" input.gtwizResetClkFreerunIn)
        -- Tied to constants
        (Port @"txctrl0_in" (pure 0 :: Signal txUser2 (BitVector 16)))
        (Port @"txctrl1_in" (pure 0 :: Signal txUser2 (BitVector 16)))
        (Port @"tx8b10ben_in" (pure 1 :: Signal txUser2 (BitVector 1)))
        (Port @"rx8b10ben_in" (pure 1 :: Signal rxUser2 (BitVector 1)))
        (Port @"rxcommadeten_in" (pure 1 :: Signal rxUser2 (BitVector 1)))
        (Port @"rxmcommaalignen_in" (pure 1 :: Signal rxUser2 (BitVector 1)))
        (Port @"rxpcommaalignen_in" (pure 1 :: Signal rxUser2 (BitVector 1)))

xilinxGthUserClockNetworkTx ::
  forall user user2.
  (KnownDomain user, KnownDomain user2) =>
  Clock user ->
  Reset user2 ->
  (Clock user, Clock user2, Signal user2 (BitVector 1))
xilinxGthUserClockNetworkTx clkIn rstIn = (usrclk_out, usrclk2_out, pack <$> tx_active_out)
 where
  ( unPort @(ClockPort "gtwiz_userclk_tx_usrclk_out" user) -> usrclk_out
    , unPort @(ClockPort "gtwiz_userclk_tx_usrclk2_out" user2) -> usrclk2_out
    , unPort @(Port "gtwiz_userclk_tx_active_out" user2 Bit) -> tx_active_out
    ) =
      inst
        (instConfig "gtwizard_ultrascale_v1_7_13_gtwiz_userclk_tx")
        (Param @"P_FREQ_RATIO_USRCLK_TO_USRCLK2" (2 :: Integer))
        (ClockPort @"gtwiz_userclk_tx_srcclk_in" clkIn)
        (ResetPort @"gtwiz_userclk_tx_reset_in" @ActiveHigh rstIn)
{-# OPAQUE xilinxGthUserClockNetworkTx #-}

xilinxGthUserClockNetworkRx ::
  forall user user2.
  (KnownDomain user, KnownDomain user2) =>
  Clock user ->
  Reset user2 ->
  (Clock user, Clock user2, Signal user2 (BitVector 1))
xilinxGthUserClockNetworkRx clkIn rstIn = (usrclk_out, usrclk2_out, pack <$> rx_active_out)
 where
  ( unPort @(ClockPort "gtwiz_userclk_rx_usrclk_out" user) -> usrclk_out
    , unPort @(ClockPort "gtwiz_userclk_rx_usrclk2_out" user2) -> usrclk2_out
    , unPort @(Port "gtwiz_userclk_rx_active_out" user2 Bit) -> rx_active_out
    ) =
      inst
        (instConfig "gtwizard_ultrascale_v1_7_13_gtwiz_userclk_rx")
        (Param @"P_FREQ_RATIO_USRCLK_TO_USRCLK2" (2 :: Integer))
        (ClockPort @"gtwiz_userclk_rx_srcclk_in" clkIn)
        (ResetPort @"gtwiz_userclk_rx_reset_in" @ActiveHigh rstIn)
{-# OPAQUE xilinxGthUserClockNetworkRx #-}

ibufds_gte3 :: forall dom. (KnownDomain dom) => DiffClock dom -> Clock dom
ibufds_gte3 diffClk = unPort @(ClockPort "O" dom) clkOut
 where
  clkOut =
    inst
      (instConfig "IBUFDS_GTE3")
      (Param @"REFCLK_EN_TX_PATH" (0 :: Bit))
      (Param @"REFCLK_HROW_CK_SEL" (0b10 :: BitVector 2))
      (Param @"REFCLK_ICNTL_RX" (0b00 :: BitVector 2))
      (NamedDiffClockPort @"I" @"IB" diffClk)
      (Port @"CEB" @dom @Bit 0)
{-# OPAQUE ibufds_gte3 #-}

{- | Clock Buffer Driven by Gigabit Transceiver. For more information see:

    https://docs.xilinx.com/r/en-US/ug974-vivado-ultrascale-libraries/BUFG_GT

The actual divide value is the value provided in @SNat div@ plus 1.
So an @SNat 0@ gives you a division of 1
-}
bufgGt ::
  forall domIn domOut div.
  (KnownDomain domIn, KnownDomain domOut, 0 <= div, div <= 7) =>
  SNat div ->
  Clock domIn ->
  Reset domIn ->
  Clock domOut
bufgGt = unsafeBufgGt

{- | Internal implementation of 'bufgGt' without domain or division constraints.

This function should not be used directly - use 'bufgGt' instead which provides
proper type safety.
-}
unsafeBufgGt ::
  forall domIn domOut div.
  (KnownDomain domOut) =>
  SNat div ->
  Clock domIn ->
  Reset domIn ->
  Clock domOut
unsafeBufgGt SNat clkIn rstIn = unPort @(ClockPort "O" domOut) clkOut
 where
  clkOut =
    inst
      (instConfig "BUFG_GT")
      (ClockPort @"I" clkIn)
      (ResetPort @"CLR" @ActiveHigh rstIn)
      (Port @"DIV" @domIn @(BitVector 3) (pure (natToNum @div)))
      (Port @"CE" @domIn @Bit 1)
      (Port @"CEMASK" @domIn @Bit 0)
      (Port @"CLRMASK" @domIn @Bit 0)
{-# OPAQUE unsafeBufgGt #-}
