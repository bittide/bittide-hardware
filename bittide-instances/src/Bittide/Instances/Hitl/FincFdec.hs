-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}

{- | A couple of tests testing clock board programming, and subsequently the
FINC and FDEC pins.
-}
module Bittide.Instances.Hitl.FincFdec where

import Clash.Explicit.Prelude
import Clash.Prelude (HiddenClock, HiddenReset, withClock, withClockResetEnable, withReset)
import Protocols

import Clash.Annotations.TH (makeTopEntity)
import Clash.Class.BitPackC (ByteOrder)
import Clash.Cores.Uart (ValidBaud)
import Clash.Cores.Xilinx (withXilinx)
import Clash.Xilinx.ClockGen (clockWizardDifferential)
import Protocols.MemoryMap (Access (WriteOnly), MemoryMap, Mm, getMMAny)
import Protocols.MemoryMap.Registers.WishboneStandard (
  RegisterConfig (..),
  deviceConfig,
  deviceWbI,
  registerConfig,
  registerWbI,
 )
import Protocols.Spi
import VexRiscv

import Bittide.ClockControl (FDEC, FINC, SpeedChange (..), speedChangeToFincFdec)
import Bittide.ClockControl.Si539xSpi (si539xSpiWb)
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
import Bittide.SharedTypes (BitboneMm, withLittleEndian)
import Bittide.Wishbone (timeWb, uartDf, uartInterfaceWb)

import GHC.Stack (HasCallStack)
import System.FilePath ((</>))

import qualified Bittide.Cpus.Riscv32imc as Riscv32imc
import qualified Bittide.Instances.Hitl.Driver.FincFdec as Driver
import qualified Clash.Class.Cdc as Cdc
import qualified Clash.Cores.Xilinx.Gth as Gth
import qualified Protocols.Spi as Spi

type Baud = 921_600

baud :: SNat Baud
baud = SNat

{- | Memory mapped component to control the FINC and FDEC pins. Note that you still need to
convert from 'SpeedChange' to the FINC/FDEC pins using 'speedChangeToFincFdec'.
-}
hardwareSpeedChange ::
  forall aw dom.
  ( HasCallStack
  , HiddenClock dom
  , HiddenReset dom
  , KnownNat aw
  , 1 <= aw
  , ?byteOrder :: ByteOrder
  ) =>
  Circuit (BitboneMm dom aw) (CSignal dom SpeedChange)
hardwareSpeedChange = circuit $ \(mm, wb) -> do
  [speedChangeBus] <- deviceWbI (deviceConfig "HardwareSpeedChange") -< (mm, wb)

  (speedChange, _speedChangeActivity) <-
    registerWbI speedChangeConfig NoChange -< (speedChangeBus, Fwd (pure Nothing))

  idC -< speedChange
 where
  speedChangeConfig = (registerConfig "speed_change" ""){access = WriteOnly}

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
    , Jtag free
    )
    ( "UART_TX" ::: CSignal free Bit
    , Spi free
    , CSignal free SpeedChange
    )
fincFdecPe freeClk freeRst skyClk = circuit $ \(mm, jtag) -> do
  [timeBus, uartBus, siBus, dcBus, speedChangeBus] <-
    withClockResetEnable freeClk freeRst enableGen
      $ processingElement NoDumpVcd peConfig
      -< (mm, jtag)

  Fwd _localCounter <- withClockResetEnable freeClk freeRst enableGen $ timeWb Nothing -< timeBus
  (uartTx, _uartStatus) <-
    withClockResetEnable freeClk freeRst enableGen
      $ uartInterfaceWb d16 d16
      $ uartDf baud
      -< (uartBus, Fwd (pure low))
  (Fwd spiDone, spiOut) <-
    withClockResetEnable freeClk freeRst enableGen
      $ si539xSpiWb (SNat @(Microseconds 10))
      -< siBus

  let skyRst = convertReset freeClk skyClk (unsafeFromActiveLow spiDone)
  Fwd _domainDiff <- domainDiffCountersWbC (skyClk :> Nil) (skyRst :> Nil) freeClk freeRst -< dcBus

  speedChange <- withClock freeClk $ withReset freeRst $ hardwareSpeedChange -< speedChangeBus

  idC -< (uartTx, spiOut, speedChange)
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
  -- Pins from clock board:
  "SMA_MGT_REFCLK_C" ::: DiffClock Ext125 ->
  "JTAG" ::: Signal Basic125 JtagIn ->
  "USB_UART_TXD" ::: Signal Basic125 Bit ->
  Signal Basic125 Spi.S2M ->
  ""
    ::: ( "JTAG" ::: Signal Basic125 JtagOut
        , "USB_UART_RXD" ::: Signal Basic125 Bit
        , -- SPI to clock board:
          "" ::: Signal Basic125 Spi.M2S
        , -- Freq increase / freq decrease request to clock board
          ""
            ::: ( "FINC" ::: Signal Basic125 FINC
                , "FDEC" ::: Signal Basic125 FDEC
                )
        )
fincFdecTests freeClkDiff boardClkDiff jtagIn _uartRx spiS2M =
  (jtagOut, uartTx, spiM2S, fincFdec)
 where
  freeClk :: Clock Basic125
  freeRst :: Reset Basic125
  (freeClk, freeRst) = clockWizardDifferential freeClkDiff noReset

  (_, odivClk) = Gth.ibufds_gte3 boardClkDiff

  boardClk :: Clock Basic125A
  boardClk = Gth.bufgGt d0 odivClk noReset

  testStart :: Signal Basic125 Bool
  testStart = hitlVioBool freeClk testStart (pure True)

  testRst = unsafeFromActiveLow testStart `orReset` freeRst

  ((_memoryMap, jtagOut), (uartTx, spiM2S, speedChange)) =
    toSignals
      (withXilinx $ withLittleEndian $ fincFdecPe freeClk testRst boardClk)
      (((), jtagIn), ((), spiS2M, ()))

  fincFdec = unbundle $ speedChangeToFincFdec freeClk testRst speedChange

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
        , "si539x" </> "fincfdec.xdc"
        , "si539x" </> "spi.xdc"
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
