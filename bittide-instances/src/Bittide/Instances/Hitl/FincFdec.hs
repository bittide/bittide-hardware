-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-}

{- | A couple of tests testing programming of the Si5345 on the FMC card, and
subsequently frequency increments and decrements over I2C.

The boot CPU first sets VADJ to 1.8 V over the main I2C bus of the KCU105, as
VADJ powers the Si5345 and the I/O through which we reach it. See
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
import VexRiscv

import Bittide.BootPe (DefSi5345SclPeriod, defFmcBootPeConfig, fmcBootPe)
import Bittide.ClockControl.Si5345I2c (splitSdaPins)
import Bittide.Counter (domainDiffCountersWbC)
import Bittide.Hitl (
  HitlTestCase (..),
  HitlTestGroup (..),
  hitlVioBool,
  paramForHwTargets,
 )
import Bittide.I2c (I2cIn, I2cOut, openDrainPins)
import Bittide.SharedTypes (withLittleEndian)
import Bittide.Wishbone (uartDf)
#ifdef SIM_BAUD_RATE
import Clash.Cores.Uart.Extra (MaxBaudRate)
#endif

import Bittide.Instances.Domains
import Bittide.Instances.Hitl.Setup (allHwTargets)

import qualified Bittide.Instances.Hitl.Driver.FincFdec as Driver
import qualified Bittide.Instances.Tef0008 as Tef0008
import qualified Clash.Class.Cdc as Cdc
import qualified Clash.Cores.Xilinx.Gth as Gth

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
    , ( Jtag free
      , "MAIN" ::: CSignal free I2cIn
      , "FMC" ::: CSignal free I2cIn
      )
    )
    ( "UART_TX" ::: CSignal free Bit
    , "MAIN" ::: CSignal free I2cOut
    , "FMC" ::: CSignal free I2cOut
    )
fincFdecPe freeClk freeRst skyClk = circuit $ \(mm, (jtag, mainI2cIn, fmcI2cIn)) -> do
  (uartBytes, Fwd clockChipDone, mainI2cOut, fmcI2cOut, [dcBus]) <-
    withClockResetEnable freeClk freeRst enableGen
      $ fmcBootPe (SNat @DefSi5345SclPeriod) peConfig
      -< (mm, jtag, mainI2cIn, fmcI2cIn)

  (_uartInBytes, uartTx) <-
    withClockResetEnable freeClk freeRst enableGen
      $ uartDf baud
      -< (uartBytes, Fwd (pure low))

  -- The reset in the destination domain (free) of the `domainDiffCounterWbC` is
  -- set by the enables register. The minimum reset duration is trivially
  -- met by the source of `clockChipDone`.
  let skyRst = xpmResetSynchronizer Asserted freeClk skyClk $ unsafeFromActiveLow clockChipDone

  Fwd _domainDiff <- domainDiffCountersWbC (skyClk :> Nil) (skyRst :> Nil) freeClk freeRst -< dcBus

  idC -< (uartTx, mainI2cOut, fmcI2cOut)
 where
  peConfig = defFmcBootPeConfig

fincFdecTests ::
  -- Pins from internal oscillator:
  "CLK_125MHZ" ::: DiffClock Ext125 ->
  -- Pins from the Si5345 on the FMC card:
  "FMC_HPC_GBTCLK1_M2C_C" ::: DiffClock Ext125 ->
  "JTAG" ::: Signal Basic125 JtagIn ->
  "USB_UART_TXD" ::: Signal Basic125 Bit ->
  -- I2C to the Si5345 on the FMC card:
  "FMC_SCL" ::: BiSignalIn 'PullUp Basic125 1 ->
  "FMC_SDA_IN" ::: Signal Basic125 Bit ->
  -- Main I2C bus of the KCU105, shared with the system controller
  "IIC_SCL_MAIN_LS" ::: BiSignalIn 'PullUp Basic125 1 ->
  "IIC_SDA_MAIN_LS" ::: BiSignalIn 'PullUp Basic125 1 ->
  ( "JTAG" ::: Signal Basic125 JtagOut
  , "USB_UART_RXD" ::: Signal Basic125 Bit
  , "FMC_SCL_OUT" ::: BiSignalOut 'PullUp Basic125 1
  , "FMC_SDA_OUT" ::: Signal Basic125 Bit
  , "FMC_MUX_SELECT" ::: Signal Basic125 (BitVector 3)
  , "IIC_SCL_MAIN_LS_OUT" ::: BiSignalOut 'PullUp Basic125 1
  , "IIC_SDA_MAIN_LS_OUT" ::: BiSignalOut 'PullUp Basic125 1
  , "IIC_MUX_RESET_B_LS" ::: Signal Basic125 Bit
  )
fincFdecTests freeClkDiff boardClkDiff jtagIn _uartRx fmcSclIn fmcSdaIn mainSclIn mainSdaIn =
  testStart
    `hwSeqX` ( jtagOut
             , uartTx
             , fmcSclOut
             , fmcSdaOut
             , Tef0008.si5345MuxSelect
             , mainSclOut
             , mainSdaOut
             , Tef0008.iicMuxResetB
             )
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

  ((_memoryMap, (jtagOut, _, _)), (uartTx, mainI2cOut, fmcI2cOut)) =
    toSignals
      (withXilinx $ withLittleEndian $ fincFdecPe freeClk freeRst boardClk)
      (((), (jtagIn, mainI2cIn, fmcI2cIn)), ((), (), ()))

  (mainI2cIn, mainSclOut, mainSdaOut) = openDrainPins mainSclIn mainSdaIn mainI2cOut
  (fmcI2cIn, fmcSclOut, fmcSdaOut) = splitSdaPins fmcSclIn fmcSdaIn fmcI2cOut

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
        [ "fincFdecTests.xdc"
        , "jtag" </> "config.xdc"
        , "jtag" </> "pmod1.xdc"
        , "uart" </> "pmod1.xdc"
        , "tef0008" </> "si5345I2c.xdc"
        , "tef0008" </> "mainI2c.xdc"
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
