-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-}

{- | A couple of tests testing clock board programming, and subsequently frequency
increments and decrements over SPI.

The boot CPU also sets VADJ to 1.8 V over the main I2C bus of the KCU105. See
@firmware-support/bittide-cpus/src/fmc_power.rs@.
-}
module Bittide.Instances.Hitl.FincFdec where

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
  hitlVioBool,
  paramForHwTargets,
 )
import Bittide.I2c (I2cIn, I2cOut, i2cControllerWb, openDrainPins)
import Bittide.ProcessingElement (PeConfig (..))
import Bittide.SharedTypes (withLittleEndian)
import Bittide.Wishbone (uartDf)
#ifdef SIM_BAUD_RATE
import Clash.Cores.Uart.Extra (MaxBaudRate)
#endif

import Bittide.Instances.Domains
import Bittide.Instances.Hitl.Setup (allHwTargets)

import qualified Bittide.Cpus.Riscv32imc as Riscv32imc
import qualified Bittide.Instances.Hitl.Driver.FincFdec as Driver
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

fincFdecPe ::
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
    , (Jtag free, CSignal free I2cIn)
    )
    ( "UART_TX" ::: CSignal free Bit
    , Spi free
    , CSignal free I2cOut
    )
fincFdecPe freeClk freeRst skyClk = circuit $ \(mm, (jtag, i2cIn)) -> do
  (uartBytes, siBus, [dcBus, i2cBus]) <-
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

  -- The I2C switches on the KCU105 support up to 400 kHz. Run at 100 kHz to be
  -- safe.
  i2cOut <-
    withClockResetEnable freeClk freeRst enableGen
      $ i2cControllerWb (SNat @(Microseconds 10))
      -< (i2cBus, i2cIn)

  idC -< (uartTx, spi, i2cOut)
 where
  peConfig =
    PeConfig
      { cpu = Riscv32imc.vexRiscv0
      , depthI = SNat @(Div (16 * 1024) 4)
      , depthD = SNat @(Div (8 * 1024) 4)
      , initI = Nothing
      , initD = Nothing
      , iBusTimeout = d0
      , dBusTimeout = d0
      , includeIlaWb = False
      }

fincFdecTests ::
  -- Pins from internal oscillator:
  "CLK_125MHZ" ::: DiffClock Ext125 ->
  -- Pins from clock board:
  "SMA_MGT_REFCLK_C" ::: DiffClock Ext125 ->
  "JTAG" ::: Signal Basic125 JtagIn ->
  "USB_UART_TXD" ::: Signal Basic125 Bit ->
  Signal Basic125 Spi.S2M ->
  -- Main I2C bus of the KCU105, shared with the system controller
  "IIC_SCL_MAIN_LS" ::: BiSignalIn 'PullUp Basic125 1 ->
  "IIC_SDA_MAIN_LS" ::: BiSignalIn 'PullUp Basic125 1 ->
  ( "JTAG" ::: Signal Basic125 JtagOut
  , "USB_UART_RXD" ::: Signal Basic125 Bit
  , -- SPI to clock board:
    "" ::: Signal Basic125 Spi.M2S
  , "IIC_SCL_MAIN_LS_OUT" ::: BiSignalOut 'PullUp Basic125 1
  , "IIC_SDA_MAIN_LS_OUT" ::: BiSignalOut 'PullUp Basic125 1
  , "IIC_MUX_RESET_B_LS" ::: Signal Basic125 Bit
  )
fincFdecTests freeClkDiff boardClkDiff jtagIn _uartRx spiS2M sclIn sdaIn =
  testStart `hwSeqX` (jtagOut, uartTx, spiM2S, sclOut, sdaOut, muxResetB)
 where
  (freeClk, freeRst) = clockWizardDifferential freeClkDiff noReset
  (_, odivClk) = Gth.ibufds_gte3 boardClkDiff

  boardClk :: Clock Basic125A
  boardClk = Gth.bufgGt d0 odivClk noReset

  -- We don't need the VIO for this test, but our current HITL infrastructure always
  -- tries to generate a probes file, which it can't if there is no VIO.
  testStart = hitlVioBool freeClk testDone testSuccess
  testDone = register freeClk testRst enableGen False (pure True)
  testSuccess = register freeClk testRst enableGen False (pure True)
  testRst = unsafeFromActiveLow testStart

  ((_memoryMap, (jtagOut, _)), (uartTx, spiM2S, i2cOut)) =
    toSignals
      (withXilinx $ withLittleEndian $ fincFdecPe freeClk freeRst boardClk)
      (((), (jtagIn, i2cIn)), ((), spiS2M, ()))

  (i2cIn, sclOut, sdaOut) = openDrainPins sclIn sdaIn i2cOut

  -- Only resets the TCA9548 I2C switch, which we do not use. Keep it out of
  -- reset, so it does not hold up the bus.
  muxResetB = pure high

memoryMap :: MemoryMap
memoryMap =
  getMMAny
    $ withXilinx
    $ withLittleEndian
    $ fincFdecPe @Basic125 @Basic125A clockGen resetGen clockGen

{-# OPAQUE fincFdecTests #-}
makeTopEntity 'fincFdecTests

tests :: HitlTestGroup
tests =
  HitlTestGroup
    { topEntity = 'fincFdecTests
    , targetXdcs =
        [ "si539xConfigTest.xdc"
        , "jtag" </> "config.xdc"
        , "jtag" </> "pmod1.xdc"
        , "uart" </> "pmod1.xdc"
        , "si539x" </> "spi.xdc"
        , "i2c" </> "main.xdc"
        ]
    , externalHdl = []
    , testCases =
        [ HitlTestCase
            { name = "FincFdecTests"
            , parameters = paramForHwTargets allHwTargets ()
            , postProcData = ()
            }
        ]
    , mDriverProc = Just Driver.driver
    , mPostProc = Nothing
    }
