-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE OverloadedStrings #-}
module Bittide.Instances.Hitl.Ethernet where

import Clash.Explicit.Prelude
import Bittide.Instances.Pnr.Ethernet
import Bittide.Instances.Domains
import Clash.Cores.Xilinx.Ethernet.Gmii
import VexRiscv
import Clash.Xilinx.ClockGen
import Bittide.Hitl

vexRiscvTcpTest ::
  DiffClock Ext125 ->
  Reset Ext125 ->
  DiffClock Basic625 ->
    ( Signal Basic125B JtagIn
    , Signal Basic125B Bit
    , Signal Basic625 Lvds
    ) ->
    ( Signal Basic125B JtagOut
    , Signal Basic125B Bit
    , Signal Basic625 Lvds
    )
vexRiscvTcpTest diffClk cpuReset sgmiiClk inp = testStarted `hwSeqX` (j, u , s)
 where
  (sysClk, sysRst) = clockWizardDifferential diffClk cpuReset
  (j, u, s, _) = vexRiscEthernet sysClk sysRst sgmiiClk inp

  -- (testError, testSuccess) = unbundle $ bitCoerce . resize <$> gpio
  -- testDone = testError .||. testSuccess

  testStarted :: Signal Basic125B Bool
  testStarted = hitlVioBool sysClk testStarted (pure True)

{-# OPAQUE vexRiscvTcpTest #-}
{-# ANN vexRiscvTcpTest (Synthesize
  { t_name = "vexRiscEthernet"
  , t_inputs =  [ PortProduct "CLK_125MHZ"
                  [PortName "P", PortName "N"]
                , PortName "CPU_RESET"
                , PortProduct "SGMIICLK"
                  [PortName "P", PortName "N"]
                , PortProduct ""
                  [ PortProduct "JTAG"
                    [ PortName "TCK", PortName "TMS", PortName "TDI"]
                  , PortName "USB_UART_TXD"
                  , PortProduct "SGMII"
                    [ PortName "RX_P", PortName "RX_N"]
                  ]
                ]
  , t_output =
    PortProduct ""
      [ PortProduct "JTAG"
        [ PortName "TDO"
        , PortName "RST"
        ]
      , PortName "USB_UART_RXD"
      , PortProduct "SGMII"
        [PortName "TX_P", PortName "TX_N"]
      ]
  }) #-}
tests :: HitlTests ()
tests = noConfigTest "vexRiscvTcpTest" allFpgas
