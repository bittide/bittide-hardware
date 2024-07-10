-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}

module Bittide.Instances.Hitl.Ethernet where

import Bittide.Hitl
import Bittide.Instances.Domains
import Bittide.Instances.Pnr.Ethernet
import Clash.Cores.Xilinx.Ethernet.Gmii
import Clash.Explicit.Prelude
import Clash.Xilinx.ClockGen
import VexRiscv

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
vexRiscvTcpTest diffClk cpuReset sgmiiClk inp = testStarted `hwSeqX` (j, u, s)
 where
  (sysClk, sysRst) = clockWizardDifferential diffClk cpuReset
  (j, u, s, _) = vexRiscEthernet sysClk sysRst sgmiiClk inp

  testStarted :: Signal Basic125B Bool
  testStarted = hitlVioBool sysClk testStarted (pure True)
{-# OPAQUE vexRiscvTcpTest #-}
{-# ANN
  vexRiscvTcpTest
  ( Synthesize
      { t_name = "vexRiscEthernet"
      , t_inputs =
          [ PortProduct
              "CLK_125MHZ"
              [PortName "p", PortName "n"]
          , PortName "CPU_RESET"
          , PortProduct
              "SGMIICLK"
              [PortName "p", PortName "n"]
          , PortProduct
              ""
              [ PortProduct
                  "JTAG"
                  [PortName "TCK", PortName "TMS", PortName "TDI"]
              , PortName "USB_UART_TXD"
              , PortProduct
                  "SGMII"
                  [PortName "RX_p", PortName "RX_n"]
              ]
          ]
      , t_output =
          PortProduct
            ""
            [ PortProduct
                "JTAG"
                [ PortName "TDO"
                , PortName "RST"
                ]
            , PortName "USB_UART_RXD"
            , PortProduct
                "SGMII"
                [PortName "TX_p", PortName "TX_n"]
            ]
      }
  )
  #-}

tests :: HitlTestGroup
tests =
  HitlTestGroup
    { topEntity = 'vexRiscvTcpTest
    , extraXdcFiles =
        ["jtag_config.xdc", "jtag_pmod1.xdc", "sgmii.xdc"]
    , externalHdl =
        [ "$env(VERILOG_ETHERNET_SRC)/rtl/*.v"
        , "$env(VERILOG_ETHERNET_SRC)/lib/axis/rtl/*.v"
        ]
    , testCases =
        [ HitlTestCase
            { name = "VexRiscvTcp"
            , parameters = paramForSingleHwTarget (HwTargetByIndex 7) ()
            , postProcData = ()
            }
        ]
    , mPostProc = Just "post-vex-riscv-tcp-test"
    }
