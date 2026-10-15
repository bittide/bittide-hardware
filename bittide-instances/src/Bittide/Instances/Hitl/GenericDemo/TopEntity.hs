-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Hitl.GenericDemo.TopEntity (
  demoTest,
  mkTests,
) where

import Clash.Explicit.Prelude
import Protocols

import Bittide.ClockControl.Si5345I2c (splitSdaPins)
import Bittide.Hitl (
  ClashTargetName,
  DeviceInfo,
  HitlTestCase (..),
  HitlTestGroup (..),
  hitlVioBool,
  paramForHwTargets,
 )
import Bittide.I2c (openDrainPins)
import Bittide.Instances.Domains (
  Basic125,
  Bittide,
  Ext125,
  Ext200,
  GthRx,
  GthRxS,
  GthTxS,
 )
import Bittide.Instances.Hitl.GenericDemo.BringUp (
  NmuRemBusWidth,
  UserCoreCircuit,
  bringUp,
 )
import Bittide.Instances.Hitl.GenericDemo.Core (NmuExternalBusses, NmuInternalBusses)
import Bittide.Instances.Hitl.Setup (LinkCount, allHwTargets, channelNames, fmcClockPaths)
import Bittide.ProcessingElement (PrefixWidth)
import Clash.Xilinx.ClockGen (clockWizardDifferential)
import System.Exit (ExitCode)
import System.FilePath ((</>))
import VexRiscv (JtagIn (..), JtagOut (..))
import Vivado.Tcl (HwTarget)
import Vivado.VivadoM (VivadoM)

import qualified Bittide.Instances.Tef0008 as Tef0008
import qualified Clash.Cores.Xilinx.Gth as Gth

{- | Shared top-entity body for the wire and soft-UGN demos. Wired up by each
demo's thin @TopEntity.hs@ wrapper, which supplies the demo-specific
@ringBufferDepth@ and @mkUserCore@.
-}
demoTest ::
  forall userCoreBusses ringBufferDepth.
  ( KnownNat userCoreBusses
  , KnownNat ringBufferDepth
  , 1 <= ringBufferDepth
  , PrefixWidth (NmuExternalBusses userCoreBusses + NmuInternalBusses) <= 30
  , 1 <= NmuRemBusWidth userCoreBusses
  , NmuRemBusWidth userCoreBusses <= 27
  ) =>
  SNat ringBufferDepth ->
  UserCoreCircuit userCoreBusses (NmuRemBusWidth userCoreBusses) ->
  -- Driven by the Si5345 on the FMC card:
  "FMC_HPC_GBTCLK1_M2C_C" ::: DiffClock Ext200 ->
  "SYSCLK_125" ::: DiffClock Ext125 ->
  "GTH_RX_S" ::: Gth.SimWires GthRx LinkCount ->
  "GTH_RX_NS" ::: Gth.Wires GthRxS LinkCount ->
  "GTH_RX_PS" ::: Gth.Wires GthRxS LinkCount ->
  -- I2C to the Si5345 on the FMC card:
  "FMC_SCL" ::: BiSignalIn 'PullUp Basic125 1 ->
  "FMC_SDA_IN" ::: Signal Basic125 Bit ->
  -- Main I2C bus of the KCU105, shared with the system controller
  "IIC_SCL_MAIN_LS" ::: BiSignalIn 'PullUp Basic125 1 ->
  "IIC_SDA_MAIN_LS" ::: BiSignalIn 'PullUp Basic125 1 ->
  "JTAG" ::: Signal Basic125 JtagIn ->
  "USB_UART_TXD" ::: Signal Basic125 Bit ->
  "SYNC_IN" ::: Signal Bittide Bit ->
  ( "GTH_TX_S" ::: Gth.SimWires Bittide LinkCount
  , "GTH_TX_NS" ::: Gth.Wires GthTxS LinkCount
  , "GTH_TX_PS" ::: Gth.Wires GthTxS LinkCount
  , "FMC_SCL_OUT" ::: BiSignalOut 'PullUp Basic125 1
  , "FMC_SDA_OUT" ::: Signal Basic125 Bit
  , "FMC_MUX_SELECT" ::: Signal Basic125 (BitVector 3)
  , "IIC_SCL_MAIN_LS_OUT" ::: BiSignalOut 'PullUp Basic125 1
  , "IIC_SDA_MAIN_LS_OUT" ::: BiSignalOut 'PullUp Basic125 1
  , "IIC_MUX_RESET_B_LS" ::: Signal Basic125 Bit
  , "JTAG" ::: Signal Basic125 JtagOut
  , "USB_UART_RXD" ::: Signal Basic125 Bit
  , "SYNC_OUT" ::: Signal Basic125 Bit
  , "" ::: Tef0008.SfpControl Basic125
  )
demoTest ringBufferDepth mkUserCore boardClkDiff refClkDiff rxs rxns rxps fmcSclIn fmcSdaIn mainSclIn mainSdaIn jtagIn _uartRx syncIn =
  testStart
    `hwSeqX` ( txs
             , txns
             , txps
             , fmcSclOut
             , fmcSdaOut
             , Tef0008.si5345MuxSelect
             , mainSclOut
             , mainSdaOut
             , Tef0008.iicMuxResetB
             , jtagOut
             , uartTx
             , syncOut
             , Tef0008.sfpControl
             )
 where
  boardClk :: Clock Ext200
  (boardClk, _) = Gth.ibufds_gte3 boardClkDiff

  refClk :: Clock Basic125
  refRst :: Reset Basic125
  (refClk, refRst) = clockWizardDifferential refClkDiff noReset

  -- We don't need the VIO for this test, but our current HITL infrastructure always
  -- tries to generate a probes file, which it can't if there is no VIO.
  testStart = hitlVioBool refClk testDone testSuccess
  testDone = register refClk testRst enableGen False (pure True)
  testSuccess = register refClk testRst enableGen False (pure True)
  testRst = unsafeFromActiveLow testStart

  ( (_memoryMaps, jtagOut, (txs, txns, txps), _, _)
    , ( mainI2cOut
        , fmcI2cOut
        , syncOut
        , uartTx
        )
    ) =
      toSignals
        (bringUp ringBufferDepth mkUserCore refClk refRst)
        ( (repeat (), jtagIn, (boardClk, rxs, rxns, rxps, channelNames, fmcClockPaths), mainI2cIn, fmcI2cIn)
        , ((), (), syncIn, ())
        )

  (mainI2cIn, mainSclOut, mainSdaOut) = openDrainPins mainSclIn mainSdaIn mainI2cOut
  (fmcI2cIn, fmcSclOut, fmcSdaOut) = splitSdaPins fmcSclIn fmcSdaIn fmcI2cOut

{- | 'HitlTestGroup' template shared by the two demos. Each demo supplies the
TH name of its own top entity and its own driver.
-}
mkTests ::
  ClashTargetName ->
  (String -> [(HwTarget, DeviceInfo)] -> VivadoM ExitCode) ->
  HitlTestGroup
mkTests topEntityName driver =
  HitlTestGroup
    { topEntity = topEntityName
    , targetXdcs =
        [ "wireDemoTest.xdc"
        , "jtag" </> "config.xdc"
        , "jtag" </> "pmod1.xdc"
        , "uart" </> "pmod1.xdc"
        , "tef0008" </> "sfpControl.xdc"
        , "tef0008" </> "si5345I2c.xdc"
        , "tef0008" </> "mainI2c.xdc"
        ]
    , externalHdl = []
    , testCases =
        [ HitlTestCase
            { name = "Bittide_Demo_DUT"
            , parameters = paramForHwTargets allHwTargets ()
            , postProcData = ()
            }
        ]
    , mDriverProc = Just driver
    , mPostProc = Nothing
    }
