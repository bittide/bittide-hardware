-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Hitl.SoftUgnDemo.TopEntity where

import Clash.Explicit.Prelude
import Protocols

import Bittide.Hitl (
  HitlTestCase (..),
  HitlTestGroup (..),
  hitlVioBool,
  paramForHwTargets,
 )
import Bittide.Instances.Domains (
  Basic125,
  Bittide,
  Ext125,
  Ext200,
  GthRx,
  GthRxS,
  GthTxS,
 )
import Bittide.Instances.Hitl.Setup (LinkCount, allHwTargets, channelNames, clockPaths)
import Bittide.Instances.Hitl.SoftUgnDemo.BringUp (bringUp)
import Clash.Annotations.TH (makeTopEntity)
import Clash.Xilinx.ClockGen (clockWizardDifferential)
import System.FilePath ((</>))
import VexRiscv (JtagIn (..), JtagOut (..))

import qualified Bittide.Instances.Hitl.SoftUgnDemo.Driver as Driver
import qualified Clash.Cores.Xilinx.Gth as Gth
import qualified Protocols.Spi as Spi

softUgnDemoTest ::
  "SMA_MGT_REFCLK_C" ::: DiffClock Ext200 ->
  "SYSCLK_125" ::: DiffClock Ext125 ->
  "GTH_RX_S" ::: Gth.SimWires GthRx LinkCount ->
  "GTH_RX_NS" ::: Gth.Wires GthRxS LinkCount ->
  "GTH_RX_PS" ::: Gth.Wires GthRxS LinkCount ->
  Signal Basic125 Spi.S2M ->
  "JTAG" ::: Signal Basic125 JtagIn ->
  "USB_UART_TXD" ::: Signal Basic125 Bit ->
  "SYNC_IN" ::: Signal Bittide Bit ->
  ( "GTH_TX_S" ::: Gth.SimWires Bittide LinkCount
  , "GTH_TX_NS" ::: Gth.Wires GthTxS LinkCount
  , "GTH_TX_PS" ::: Gth.Wires GthTxS LinkCount
  , ""
      ::: ( "FINC" ::: Signal Bittide Bool
          , "FDEC" ::: Signal Bittide Bool
          )
  , "" ::: Signal Basic125 Spi.M2S
  , "JTAG" ::: Signal Basic125 JtagOut
  , "USB_UART_RXD" ::: Signal Basic125 Bit
  , "SYNC_OUT" ::: Signal Basic125 Bit
  )
softUgnDemoTest boardClkDiff refClkDiff rxs rxns rxps spiS2M jtagIn _uartRx syncIn =
  ( txs
  , txns
  , txps
  , unbundle swFincFdecs
  , spiM2S
  , jtagOut
  , uartTx
  , syncOut
  )
 where
  boardClk :: Clock Ext200
  (boardClk, _) = Gth.ibufds_gte3 boardClkDiff

  refClk :: Clock Basic125
  refRst :: Reset Basic125
  (refClk, refRst) = clockWizardDifferential refClkDiff noReset

  testStart :: Signal Basic125 Bool
  testStart = hitlVioBool refClk testStart (pure True)

  testReset :: Reset Basic125
  testReset = unsafeFromActiveLow testStart `orReset` refRst

  ( (_bootMm, _muMm, _ccMm, _gppeMm, jtagOut, (txs, txns, txps))
    , ( spiM2S
        , syncOut
        , uartTx
        , swFincFdecs
        )
    ) =
      toSignals
        (bringUp refClk testReset)
        ( ((), (), (), (), jtagIn, (boardClk, rxs, rxns, rxps, channelNames, clockPaths))
        , (spiS2M, syncIn, (), ())
        )
{-# OPAQUE softUgnDemoTest #-}
makeTopEntity 'softUgnDemoTest

tests :: HitlTestGroup
tests =
  HitlTestGroup
    { topEntity = 'softUgnDemoTest
    , targetXdcs =
        [ "switchDemoTest.xdc"
        , "jtag" </> "config.xdc"
        , "jtag" </> "pmod1.xdc"
        , "uart" </> "pmod1.xdc"
        , "si539x" </> "fincfdec.xdc"
        , "si539x" </> "spi.xdc"
        ]
    , externalHdl = []
    , testCases =
        [ HitlTestCase
            { name = "Bittide_Demo_DUT"
            , parameters = paramForHwTargets allHwTargets ()
            , postProcData = ()
            }
        ]
    , mDriverProc = Just Driver.driver
    , mPostProc = Nothing
    }
