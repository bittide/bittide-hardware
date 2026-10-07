-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-}

{- | A hardware-in-the-loop test to verify we can program the external clock
board from software using a specialized memory-mapped SPI device.
-}
module Bittide.Instances.Hitl.Si539xConfiguration where

import Clash.Explicit.Prelude
import Clash.Prelude (withClockResetEnable)
import Protocols

import GHC.Stack (HasCallStack)
import System.FilePath ((</>))

import Clash.Annotations.TH (makeTopEntity)
import Clash.Class.BitPackC (ByteOrder)
import Clash.Cores.Uart (ValidBaud)
import Clash.Cores.Xilinx (withXilinx)
import Clash.Explicit.Reset.Extra (Asserted (..), xpmResetSynchronizer)
import Clash.Xilinx.ClockGen (clockWizardDifferential)
import Protocols.MemoryMap (MemoryMap, Mm, getMMAny)
import Protocols.Spi (Spi)
import VexRiscv

import Bittide.BootPe (bootPe)
import Bittide.ClockControl.Si539xSpi (si539xSpiWb)
import Bittide.Counter (domainDiffCountersWbC)
import Bittide.Hitl (
  HitlTestCase (..),
  HitlTestGroup (..),
  HwTargetRef (HwTargetByIndex),
  hitlVioBool,
  paramForSingleHwTarget,
 )
import Bittide.ProcessingElement (PeConfig (..))
import Bittide.SharedTypes (withLittleEndian)
import Bittide.Wishbone (uartDf)
#ifdef SIM_BAUD_RATE
import Clash.Cores.Uart.Extra (MaxBaudRate)
#endif

import Bittide.Instances.Domains

import qualified Bittide.Cpus.Riscv32imc as Riscv32imc
import qualified Bittide.Instances.Hitl.Driver.Si539xConfiguration as Driver
import qualified Clash.Class.Cdc as Cdc
import qualified Clash.Cores.Xilinx.Gth as Gth
import qualified Protocols.Spi as Spi

#ifdef SIM_BAUD_RATE
type Baud = MaxBaudRate Basic125
#else
type Baud = 921_600
#endif

baud :: SNat Baud
baud = SNat

si539xConfigPe ::
  forall free sky vendor.
  ( HasCallStack
  , KnownDomain free
  , KnownDomain sky
  , HasSynchronousReset free
  , HasSynchronousReset sky
  , 1 <= DomainPeriod free
  , ValidBaud free Baud
  , ?byteOrder :: ByteOrder
  , Cdc.HiddenVendor vendor
  , Cdc.ValidGray vendor 8 sky free
  , Cdc.GrayConstraints vendor 8 sky free
  ) =>
  Clock free ->
  Reset free ->
  Clock sky ->
  Circuit
    ( ToConstBwd Mm
    , Jtag free
    )
    ( "UART_TX" ::: CSignal free Bit
    , Spi free
    )
si539xConfigPe freeClk freeRst skyClk = circuit $ \(mm, jtag) -> do
  (uartBytes, siBus, [dcBus]) <-
    withClockResetEnable freeClk freeRst enableGen
      $ bootPe peConfig
      -< (mm, jtag)

  (Fwd spiDone, spi) <-
    withClockResetEnable freeClk freeRst enableGen
      $ si539xSpiWb (SNat @(Microseconds 10))
      -< siBus

  (_uartInBytes, uartTx) <-
    withClockResetEnable freeClk freeRst enableGen
      $ uartDf baud
      -< (uartBytes, Fwd (pure low))

  -- The reset in the destination domain (free) of the `domainDiffCounterWbC` is
  -- set by the enables register. The minimum reset duration is trivially
  -- met by the source of `spiDone`.
  let skyRst = xpmResetSynchronizer Asserted freeClk skyClk $ unsafeFromActiveLow spiDone

  Fwd _domainDiff <- domainDiffCountersWbC (skyClk :> Nil) (skyRst :> Nil) freeClk freeRst -< dcBus

  idC -< (uartTx, spi)
 where
  peConfig =
    PeConfig
      { cpu = Riscv32imc.vexRiscv0
      , depthI = SNat @(Div (8 * 1024) 4)
      , depthD = SNat @(Div (8 * 1024) 4)
      , initI = Nothing
      , initD = Nothing
      , iBusTimeout = d0
      , dBusTimeout = d0
      , includeIlaWb = False
      }

si539xConfigTest ::
  "CLK_125MHZ" ::: DiffClock Ext125 ->
  "SMA_MGT_REFCLK_C" ::: DiffClock Ext200 ->
  "JTAG" ::: Signal Basic125 JtagIn ->
  "USB_UART_TXD" ::: Signal Basic125 Bit ->
  Signal Basic125 Spi.S2M ->
  ( "JTAG" ::: Signal Basic125 JtagOut
  , "USB_UART_RXD" ::: Signal Basic125 Bit
  , "" ::: Signal Basic125 Spi.M2S
  )
si539xConfigTest freeClkDiff skyClkDiff jtagIn _uartRx spiS2M =
  testStart `hwSeqX` (jtagOut, uartTx, spiM2S)
 where
  (freeClk, freeRst) = clockWizardDifferential freeClkDiff noReset
  (_, odivClk) = Gth.ibufds_gte3 skyClkDiff

  skyClk :: Clock Basic200
  skyClk = Gth.bufgGt d0 odivClk noReset

  -- We don't need the VIO for this test, but our current HITL infrastructure always
  -- tries to generate a probes file, which it can't if there is no VIO.
  testStart = hitlVioBool freeClk testDone testSuccess
  testDone = register freeClk testRst enableGen False (pure True)
  testSuccess = register freeClk testRst enableGen False (pure True)
  testRst = unsafeFromActiveLow testStart

  ((_memoryMap, jtagOut), (uartTx, spiM2S)) =
    toSignals
      (withXilinx $ withLittleEndian $ si539xConfigPe freeClk freeRst skyClk)
      (((), jtagIn), ((), spiS2M))

memoryMap :: MemoryMap
memoryMap =
  getMMAny
    $ withXilinx
    $ withLittleEndian
    $ si539xConfigPe @Basic125 @Basic200 clockGen resetGen clockGen

makeTopEntity 'si539xConfigTest

tests :: HitlTestGroup
tests =
  HitlTestGroup
    { topEntity = 'si539xConfigTest
    , targetXdcs =
        [ "si539xConfigTest.xdc"
        , "jtag" </> "config.xdc"
        , "jtag" </> "pmod1.xdc"
        , "uart" </> "pmod1.xdc"
        , "si539x" </> "spi.xdc"
        ]
    , externalHdl = []
    , testCases =
        [ HitlTestCase
            { name = "Si539xConfigTest"
            , parameters = paramForSingleHwTarget (HwTargetByIndex 7) ()
            , postProcData = ()
            }
        ]
    , mDriverProc = Just Driver.driver
    , mPostProc = Nothing
    }
