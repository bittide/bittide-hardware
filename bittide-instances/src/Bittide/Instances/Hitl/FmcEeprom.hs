-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Programs the IPMI FRU EEPROM of the Trenz TEF0008 FMC card (4x SFP+, with a
Si5345 clock generator) on the FMC HPC connector. The contents written describe
this particular card, so do not run this on boards with any other FMC card
attached.

The KCU105 system controller reads this EEPROM at power-on to decide which
voltage to put on VADJ. The TEF0008 ships with an unprogrammed EEPROM, which
makes the system controller set VADJ to 0 V. That leaves the card's clock chip
unpowered. See @firmware-binaries/hitl-tests/program-fmc-eeprom@ for the
contents written.

The EEPROM is on the FMC card, so this only needs to run once per card. It does
not run as part of regular CI. Add it to @.github/synthesis/debug.json@ to run
it. After programming, the boards need a power cycle for VADJ to be set.

The FPGA reaches the EEPROM through the board's main I2C bus, which is
shared with the system controller. The FMC HPC I2C bus is behind channel 1 of
the PCA9544 I2C switch on that bus. See the KCU105 board user guide (UG917),
section "I2C Bus, Topology, and Switches".
-}
module Bittide.Instances.Hitl.FmcEeprom where

import Clash.Explicit.Prelude
import Clash.Prelude (withClockResetEnable)
import Protocols

import Clash.Annotations.TH (makeTopEntity)
import Clash.Class.BitPackC (ByteOrder)
import Clash.Cores.Uart (ValidBaud)
import Clash.Xilinx.ClockGen (clockWizardDifferential)
import Protocols.MemoryMap (MemoryMap, Mm, getMMAny)
import VexRiscv

import Bittide.Hitl (
  HitlTestCase (..),
  HitlTestGroup (..),
  HwTargetRef (HwTargetByIndex),
  hitlVioBool,
  paramForSingleHwTarget,
 )
import Bittide.I2c (I2cIn, I2cOut, i2cControllerWb, openDrainPins)
import Bittide.Instances.Domains
import Bittide.ProcessingElement (PeConfig (..), processingElement)
import Bittide.SharedTypes (withLittleEndian)
import Bittide.Wishbone (timeWb, uartDf, uartInterfaceWb)
#ifdef SIM_BAUD_RATE
import Clash.Cores.Uart.Extra (MaxBaudRate)
#endif

import GHC.Stack (HasCallStack)
import System.FilePath ((</>))

import qualified Bittide.Cpus.Riscv32imc as Riscv32imc
import qualified Bittide.Instances.Hitl.Driver.FmcEeprom as Driver

#ifdef SIM_BAUD_RATE
type Baud = MaxBaudRate Basic125
#else
type Baud = 921_600
#endif

baud :: SNat Baud
baud = SNat

fmcEepromPe ::
  forall dom.
  ( HasCallStack
  , KnownDomain dom
  , HasSynchronousReset dom
  , 1 <= DomainPeriod dom
  , ValidBaud dom Baud
  , ?byteOrder :: ByteOrder
  ) =>
  Clock dom ->
  Reset dom ->
  Circuit
    (ToConstBwd Mm, (Jtag dom, CSignal dom I2cIn))
    ("UART_TX" ::: CSignal dom Bit, CSignal dom I2cOut)
fmcEepromPe clk rst = circuit $ \(mm, (jtag, i2cIn)) -> do
  [timeBus, uartBus, i2cBus] <-
    withClockResetEnable clk rst enableGen
      $ processingElement NoDumpVcd peConfig
      -< (mm, jtag)

  Fwd _localCounter <- withClockResetEnable clk rst enableGen $ timeWb Nothing -< timeBus
  (uartTx, _uartStatus) <-
    withClockResetEnable clk rst enableGen
      $ uartInterfaceWb d16 d16
      $ uartDf baud
      -< (uartBus, Fwd (pure low))

  -- The I2C switches on the KCU105 support up to 400 kHz, the EEPROM up to
  -- 400 kHz or more. Run at 100 kHz to be safe.
  i2cOut <-
    withClockResetEnable clk rst enableGen
      $ i2cControllerWb (SNat @(Microseconds 10))
      -< (i2cBus, i2cIn)

  idC -< (uartTx, i2cOut)
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

programFmcEeprom ::
  "CLK_125MHZ" ::: DiffClock Ext125 ->
  "JTAG" ::: Signal Basic125 JtagIn ->
  "USB_UART_TXD" ::: Signal Basic125 Bit ->
  -- Main I2C bus of the KCU105, shared with the system controller
  "IIC_SCL_MAIN_LS" ::: BiSignalIn 'PullUp Basic125 1 ->
  "IIC_SDA_MAIN_LS" ::: BiSignalIn 'PullUp Basic125 1 ->
  ""
    ::: ( "JTAG" ::: Signal Basic125 JtagOut
        , "USB_UART_RXD" ::: Signal Basic125 Bit
        , "IIC_SCL_MAIN_LS_OUT" ::: BiSignalOut 'PullUp Basic125 1
        , "IIC_SDA_MAIN_LS_OUT" ::: BiSignalOut 'PullUp Basic125 1
        , "IIC_MUX_RESET_B_LS" ::: Signal Basic125 Bit
        )
programFmcEeprom clkDiff jtagIn _uartRx sclIn sdaIn =
  testStart `hwSeqX` (jtagOut, uartTx, sclOut, sdaOut, muxResetB)
 where
  clk :: Clock Basic125
  rst :: Reset Basic125
  (clk, rst) = clockWizardDifferential clkDiff noReset

  -- We don't need the VIO for this test, but our current HITL infrastructure always
  -- tries to generate a probes file, which it can't if there is no VIO.
  testStart = hitlVioBool clk testDone testSuccess
  testDone = register clk testRst enableGen False (pure True)
  testSuccess = register clk testRst enableGen False (pure True)
  testRst = unsafeFromActiveLow testStart

  ((_memoryMap, (jtagOut, _)), (uartTx, i2cOut)) =
    toSignals
      (withLittleEndian $ fmcEepromPe clk rst)
      (((), (jtagIn, i2cIn)), ((), ()))

  (i2cIn, sclOut, sdaOut) = openDrainPins sclIn sdaIn i2cOut

  -- Only resets the TCA9548 I2C switch, which we do not use. Keep it out of
  -- reset, so it does not hold up the bus.
  muxResetB = pure high

memoryMap :: MemoryMap
memoryMap =
  getMMAny
    $ withLittleEndian
    $ fmcEepromPe @Basic125 clockGen resetGen

{-# OPAQUE programFmcEeprom #-}
makeTopEntity 'programFmcEeprom

tests :: HitlTestGroup
tests =
  HitlTestGroup
    { topEntity = 'programFmcEeprom
    , targetXdcs =
        [ "programFmcEeprom.xdc"
        , "jtag" </> "config.xdc"
        , "jtag" </> "pmod1.xdc"
        , "uart" </> "pmod1.xdc"
        ]
    , externalHdl = []
    , testCases =
        [ HitlTestCase
            { name = "ProgramFmcEeprom"
            , parameters = paramForSingleHwTarget (HwTargetByIndex 7) ()
            , postProcData = ()
            }
        ]
    , mDriverProc = Just Driver.driver
    , mPostProc = Nothing
    }
