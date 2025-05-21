-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

-- {-# OPTIONS -fplugin-opt=Protocols.Plugin:debug #-}

module Bittide.Instances.Hitl.VexRiscv where

import Clash.Annotations.TH (makeTopEntity)

import Clash.Explicit.Prelude (noReset, orReset)
import Clash.Prelude

import BitPackC
import Clash.Cores.UART (ValidBaud)
import Clash.Xilinx.ClockGen (clockWizardDifferential)
import Data.Maybe (fromMaybe)
import Protocols
import Protocols.MemoryMap (Access (WriteOnly), ConstBwd, MM, constBwd, getMMAny)
import Protocols.MemoryMap.FieldType
import Protocols.MemoryMap.Registers.WishboneStandard (
  RegisterConfig (description),
  access,
  deviceWbC,
  registerConfig,
  registerWbC,
 )
import Protocols.Wishbone
import System.Environment (lookupEnv)
import VexRiscv

import Bittide.DoubleBufferedRam (
  ContentType (Vec),
  InitialContent (Reloadable, Undefined),
 )
import Bittide.Hitl
import Bittide.Instances.Domains (Basic125, Ext125)
import Bittide.Instances.Hitl.Driver.VexRiscv
import Bittide.ProcessingElement (PeConfig (..), processingElement)
import Bittide.ProcessingElement.Util (vecFromElfData, vecFromElfInstr)
import Bittide.SharedTypes
import Bittide.Wishbone
import Clash.Cores.UART.Extra

import GHC.Stack (HasCallStack)
import Project.FilePath (
  CargoBuildType (Release),
  findParentContaining,
  firmwareBinariesDir,
 )
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)

import qualified Protocols.MemoryMap as MM

data TestStatus = Running | Success | Fail
  deriving (Enum, Eq, Generic, NFDataX, BitPack, BitPackC, ToFieldType, Show)

type TestDone = Bool
type TestSuccess = Bool
type UartRx = Bit
type UartTx = Bit

#ifdef SIM_BAUD_RATE
type Baud = MaxBaudRate Basic125
#else
type Baud = 921_600
#endif

baud :: SNat Baud
baud = SNat

-- | To use this function, change the initial contents of the iMem and dMem
sim :: IO ()
sim =
  uartIO stdin stdout baud $ withClockResetEnable clockGen resetGen enableGen $ Circuit go
 where
  go (uartRx, _) = (pure (), uartTx)
   where
    (_, (_, uartTx)) =
      toSignals (vexRiscvTestC @Basic125) (((), (pure $ unpack 0, uartRx)), (pure (), pure ()))

{- | Wishbone accessible status register. Used to communicate the test status
from the CPU to the outside world through VIOs.
-}
statusRegister ::
  forall aw dom.
  (HasCallStack, HiddenClock dom, HiddenReset dom, KnownNat aw, 1 <= aw) =>
  Circuit
    (ConstBwd MM, Wishbone dom 'Standard aw (Bytes 4))
    (CSignal dom TestStatus)
statusRegister = circuit $ \(mm, wb) -> do
  [statusWb] <- deviceWbC "StatusRegister" -< (mm, wb)
  (statusOut, _a) <-
    registerWbC hasClock hasReset statusConf Running -< (statusWb, Fwd (pure Nothing))
  idC -< statusOut
 where
  statusConf =
    (registerConfig "status")
      { access = WriteOnly
      , description = "Set test status"
      }

vexRiscvTestMM :: MM.MemoryMap
vexRiscvTestMM =
  getMMAny
    $ withClockResetEnable clockGen resetGen enableGen
    $ vexRiscvTestC @Basic125

vexRiscvTestC ::
  forall dom.
  ( HiddenClockResetEnable dom
  , HasCallStack
  , 1 <= DomainPeriod dom
  , ValidBaud dom Baud
  ) =>
  Circuit
    (ConstBwd MM, (Jtag dom, CSignal dom UartRx))
    (CSignal dom TestStatus, CSignal dom UartTx)
vexRiscvTestC = circuit $ \(mm, (jtag, uartRx)) -> do
  [ (preTime, (mmTime, timeBus))
    , (preUart, (mmUart, uartBus))
    , (preStatus, (mmStatus, statusRegisterBus))
    ] <-
    processingElement NoDumpVcd peConfig -< (mm, jtag)

  --       0b010 Data memory
  --       0b100 Instruction memory
  constBwd 0b101 -< preTime
  constBwd 0b110 -< preUart
  constBwd 0b111 -< preStatus

  (uartTx, _uartStatus) <-
    uartInterfaceWb @dom d16 d16 (uartDf baud) -< (mmUart, (uartBus, uartRx))
  _localCounter <- timeWb -< (mmTime, timeBus)

  testResult <- statusRegister -< (mmStatus, statusRegisterBus)
  idC -< (testResult, uartTx)
 where
  peConfig
    | clashSimulation = peConfigSim
    | otherwise = peConfigRtl

  peConfigSim = unsafePerformIO $ do
    root <- findParentContaining "cabal.project"
    maybeBinaryName <- lookupEnv "TEST_BINARY_NAME"
    let
      elfDir = root </> firmwareBinariesDir "riscv32imc" Release
      elfPath = elfDir </> fromMaybe "hello" maybeBinaryName
    pure
      peConfigRtl
        { initI =
            Reloadable
              $ Vec
              $ unsafePerformIO
              $ vecFromElfInstr @IMemWords BigEndian elfPath
        , initD =
            Reloadable
              $ Vec
              $ unsafePerformIO
              $ vecFromElfData @DMemWords BigEndian elfPath
        , includeIlaWb = False
        }
  peConfigRtl =
    PeConfig
      { initI = Undefined @DMemWords
      , prefixI = 0b100
      , initD = Undefined @IMemWords
      , prefixD = 0b010
      , iBusTimeout = d0
      , dBusTimeout = d0
      , includeIlaWb = True
      }

type IMemWords = DivRU (64 * 1024) 4
type DMemWords = DivRU (64 * 1024) 4

vexRiscvTest ::
  "CLK_125MHZ" ::: DiffClock Ext125 ->
  "JTAG" ::: Signal Basic125 JtagIn ->
  "USB_UART_TXD" ::: Signal Basic125 UartRx ->
  ""
    ::: ( "done" ::: Signal Basic125 TestDone
        , "success" ::: Signal Basic125 TestSuccess
        , "JTAG" ::: Signal Basic125 JtagOut
        , "USB_UART_RXD" ::: Signal Basic125 UartTx
        )
vexRiscvTest diffClk jtagIn uartRx = (testStatusDone, testStatusSuccess, jtagOut, uartTx)
 where
  (unbundle -> (testStatusDone, testStatusSuccess)) = stateToDoneSuccess <$> testStatus

  stateToDoneSuccess Running = (False, False)
  stateToDoneSuccess Success = (True, True)
  stateToDoneSuccess Fail = (True, False)

  (clk, clkStableRst) = clockWizardDifferential diffClk noReset

  ((_mm, (jtagOut, _)), (testStatus, uartTx)) =
    withClockResetEnable clk reset enableGen
      $ toSignals
        (vexRiscvTestC @Basic125)
        (((), (jtagIn, uartRx)), (pure (), pure ()))

  reset = orReset clkStableRst (unsafeFromActiveLow testStarted)

  testStarted :: Signal Basic125 Bool
  testStarted = hitlVioBool clk testDone testSuccess

  -- TODO: We used to perform a HITL test where the CPU would write to a success
  --       register (or a failure register when it would get trapped). We
  --       currently load programs over JTAG instead of preloading them in the
  --       bitstream, making this impossible to do. We should add a _pre_
  --       processing step to the HITL infrastructure, restoring the ability to
  --       do this once more.
  testDone = testStarted
  testSuccess = testStarted
{-# NOINLINE vexRiscvTest #-}
makeTopEntity 'vexRiscvTest

tests :: HitlTestGroup
tests =
  HitlTestGroup
    { topEntity = 'vexRiscvTest
    , extraXdcFiles = ["jtag" </> "config.xdc", "jtag" </> "pmod1.xdc", "uart" </> "pmod1.xdc"]
    , externalHdl = []
    , testCases =
        [ HitlTestCase
            { name = "VexRiscV"
            , parameters =
                paramForHwTargets [HwTargetByIndex 1, HwTargetByIndex 2] ()
            , postProcData = ()
            }
        ]
    , mDriverProc = Just driverFunc
    , mPostProc = Nothing
    }
