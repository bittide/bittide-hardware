-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}

{- | A couple of tests testing programming of the Si5345 on the FMC card, and
subsequently frequency increments and decrements over I2C.
-}
module Bittide.Instances.Hitl.FincFdec where

import Clash.Explicit.Prelude
import Clash.Prelude (withClockResetEnable)
import Protocols

import Clash.Annotations.TH (makeTopEntity)
import Clash.Class.BitPackC (ByteOrder)
import Clash.Cores.Uart (ValidBaud)
import Clash.Cores.Xilinx (withXilinx)
import Clash.Xilinx.ClockGen (clockWizardDifferential)
import Protocols.MemoryMap (MemoryMap, Mm, getMMAny)
import VexRiscv

import Bittide.ClockControl.Si5345I2c (I2cIn, I2cOut, si5345I2cWb, splitSdaPins)
import Bittide.Counter (domainDiffCountersWbC)
import Bittide.Hitl (
  HitlTestCase (..),
  HitlTestGroup (..),
  hitlVioBool,
  paramForHwTargets,
 )
import Bittide.Instances.Domains
import Bittide.Instances.Hitl.Setup (allHwTargets)
import Bittide.ProcessingElement (PeConfig (..), processingElement)
import Bittide.SharedTypes (withLittleEndian)
import Bittide.Wishbone (timeWb, uartDf, uartInterfaceWb)

import GHC.Stack (HasCallStack)
import System.FilePath ((</>))

import qualified Bittide.Cpus.Riscv32imc as Riscv32imc
import qualified Bittide.Instances.Hitl.Driver.FincFdec as Driver
import qualified Clash.Class.Cdc as Cdc
import qualified Clash.Cores.Xilinx.Gth as Gth

type Baud = 921_600

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
    , CSignal free I2cOut
    )
fincFdecPe freeClk freeRst skyClk = circuit $ \(mm, (jtag, i2cIn)) -> do
  [timeBus, uartBus, siBus, dcBus] <-
    withClockResetEnable freeClk freeRst enableGen
      $ processingElement NoDumpVcd peConfig
      -< (mm, jtag)

  Fwd _localCounter <- withClockResetEnable freeClk freeRst enableGen $ timeWb Nothing -< timeBus
  (uartTx, _uartStatus) <-
    withClockResetEnable freeClk freeRst enableGen
      $ uartInterfaceWb d16 d16
      $ uartDf baud
      -< (uartBus, Fwd (pure low))
  (Fwd clockChipDone, i2cOut) <-
    withClockResetEnable freeClk freeRst enableGen
      $ si5345I2cWb (SNat @(Nanoseconds 2500)) 0x69
      -< (siBus, i2cIn)

  let skyRst = convertReset freeClk skyClk (unsafeFromActiveLow clockChipDone)
  Fwd _domainDiff <- domainDiffCountersWbC (skyClk :> Nil) (skyRst :> Nil) freeClk freeRst -< dcBus

  idC -< (uartTx, i2cOut)
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

fincFdecTests ::
  -- Pins from internal oscillator:
  "CLK_125MHZ" ::: DiffClock Ext125 ->
  -- Pins from the Si5345 on the FMC card:
  "FMC_HPC_GBTCLK1_M2C_C" ::: DiffClock Ext200 ->
  "JTAG" ::: Signal Basic125 JtagIn ->
  "USB_UART_TXD" ::: Signal Basic125 Bit ->
  -- I2C to the Si5345 on the FMC card:
  "FMC_SCL" ::: BiSignalIn 'PullUp Basic125 1 ->
  "FMC_SDA_IN" ::: Signal Basic125 Bit ->
  ""
    ::: ( "JTAG" ::: Signal Basic125 JtagOut
        , "USB_UART_RXD" ::: Signal Basic125 Bit
        , "FMC_SCL_OUT" ::: BiSignalOut 'PullUp Basic125 1
        , "FMC_SDA_OUT" ::: Signal Basic125 Bit
        , "FMC_MUX_SELECT" ::: Signal Basic125 (BitVector 3)
        )
fincFdecTests freeClkDiff boardClkDiff jtagIn _uartRx sclIn sdaIn =
  (jtagOut, uartTx, sclOut, sdaOut, muxSelect)
 where
  freeClk :: Clock Basic125
  freeRst :: Reset Basic125
  (freeClk, freeRst) = clockWizardDifferential freeClkDiff noReset

  (_, odivClk) = Gth.ibufds_gte3 boardClkDiff

  boardClk :: Clock Basic200
  boardClk = Gth.bufgGt d0 odivClk noReset

  testStart :: Signal Basic125 Bool
  testStart = hitlVioBool freeClk testStart (pure True)

  testRst = unsafeFromActiveLow testStart `orReset` freeRst

  ((_memoryMap, (jtagOut, _)), (uartTx, i2cOut)) =
    toSignals
      (withXilinx $ withLittleEndian $ fincFdecPe freeClk testRst boardClk)
      (((), (jtagIn, i2cIn)), ((), ()))

  (i2cIn, sclOut, sdaOut) = splitSdaPins sclIn sdaIn i2cOut

  -- Value used in earlier experiments with this FMC card. It is not known what
  -- these pins select.
  muxSelect = pure 0b100

memoryMap :: MemoryMap
memoryMap =
  getMMAny
    $ withXilinx
    $ withLittleEndian
    $ fincFdecPe @Basic125 @Basic200 clockGen resetGen clockGen

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
        , "si5345" </> "i2c.xdc"
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
