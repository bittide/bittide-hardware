-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Hitl.GenericDemo.TopEntity (
  demoTest,
  mkTests,
) where

import Clash.Explicit.Prelude
import Protocols

import Bittide.Hitl (
  ClashTargetName,
  DeviceInfo,
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
import Bittide.Instances.Hitl.GenericDemo.BringUp (
  NmuRemBusWidth,
  UserCoreCircuit,
  bringUp,
 )
import Bittide.Instances.Hitl.GenericDemo.Core (NmuExternalBusses, NmuInternalBusses)
import Bittide.Instances.Hitl.Setup (LinkCount, allHwTargets, channelNames, clockPaths)
import Bittide.ProcessingElement (PrefixWidth)
import Clash.Xilinx.ClockGen (clockWizardDifferential)
import System.Exit (ExitCode)
import System.FilePath ((</>))
import VexRiscv (JtagIn (..), JtagOut (..))
import Vivado.Tcl (HwTarget)
import Vivado.VivadoM (VivadoM)

import qualified Clash.Cores.Xilinx.Gth as Gth
import qualified Protocols.Spi as Spi

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
demoTest ringBufferDepth mkUserCore boardClkDiff refClkDiff rxs rxns rxps spiS2M jtagIn _uartRx syncIn =
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

  ( (_memoryMaps, jtagOut, (txs, txns, txps))
    , ( spiM2S
        , syncOut
        , uartTx
        , swFincFdecs
        )
    ) =
      toSignals
        (bringUp ringBufferDepth mkUserCore refClk testReset)
        ( (repeat (), jtagIn, (boardClk, rxs, rxns, rxps, channelNames, clockPaths))
        , (spiS2M, syncIn, (), ())
        )

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
    , mDriverProc = Just driver
    , mPostProc = Nothing
    }
