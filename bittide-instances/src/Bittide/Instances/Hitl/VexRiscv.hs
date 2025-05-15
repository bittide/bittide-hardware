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
import Protocols
import Protocols.MemoryMap as MM
import Protocols.MemoryMap.FieldType
import Protocols.Wishbone
import VexRiscv

import Bittide.DoubleBufferedRam
import Bittide.Hitl
import Bittide.Instances.Domains (Basic125, Ext125)
import Bittide.Instances.Hitl.Driver.VexRiscv
import Bittide.ProcessingElement (PeConfig (..), processingElement)
import Bittide.ProcessingElement.Util (unsafeVecFromElfData, unsafeVecFromElfInstr)
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

data TestStatus = Running | Success | Fail
  deriving (Enum, Eq, Generic, NFDataX, BitPack, BitPackC, ToFieldType)

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

  constBwd 0b110 -< preUart
  (uartTx, _uartStatus) <-
    uartInterfaceWb @dom d16 d16 (uartDf baud) -< (mmUart, (uartBus, uartRx))
  constBwd 0b101 -< preTime
  _localCounter <- timeWb -< (mmTime, timeBus)

  constBwd 0b111 -< preStatus
  testResult <- statusRegister -< (mmStatus, statusRegisterBus)
  idC -< (testResult, uartTx)
 where
  statusRegister ::
    Circuit (ConstBwd MM, Wishbone dom 'Standard 27 (Bytes 4)) (CSignal dom TestStatus)
  statusRegister = withMemoryMap mm $ Circuit $ \(fwd, _) ->
    let (unbundle -> (m2s, st)) = mealy go Running fwd
     in (m2s, st)
   where
    mm =
      MemoryMap
        { tree = DeviceInstance locCaller "StatusRegister"
        , deviceDefs = deviceSingleton deviceDef
        }
    deviceDef =
      DeviceDefinition
        { tags = []
        , registers =
            [ NamedLoc
                { name = Name "status" ""
                , loc = locHere
                , value =
                    Register
                      { fieldType = regType @TestStatus
                      , address = 0x00
                      , access = WriteOnly
                      , tags = []
                      , reset = Nothing
                      }
                }
            ]
        , deviceName = Name "StatusRegister" ""
        , definitionLoc = locHere
        }
    go st WishboneM2S{..}
      -- out of cycle, no response, same state
      | not (busCycle && strobe) = (st, (emptyWishboneS2M, st))
      -- already done, ACK and same state
      | st /= Running = (st, (emptyWishboneS2M{acknowledge = True}, st))
      -- read, this is write-only, so error, same state
      | not writeEnable =
          ( st
          ,
            ( (emptyWishboneS2M @(Bytes 4))
                { err = True
                , readData = errorX "status register is write-only"
                }
            , st
            )
          )
      -- write! change state, ACK
      | otherwise =
          let state = case writeData of
                1 -> Success
                _ -> Fail
           in (state, (emptyWishboneS2M{acknowledge = True}, state))

  -- ╭────────┬───────┬───────┬────────────────────────────────────╮
  -- │ bin    │ hex   │ bus   │ description                        │
  -- ├────────┼───────┼───────┼────────────────────────────────────┤
  -- │ 0b000. │ 0x0   │       │                                    │
  -- │ 0b001. │ 0x2   │       │                                    │
  -- │ 0b010. │ 0x4   │ 1     │ Data memory                        │
  -- │ 0b011. │ 0x6   │       │                                    │
  -- │ 0b100. │ 0x8   │ 0     │ Instruction memory                 │
  -- │ 0b101. │ 0xA   │ 2     │ Time                               │
  -- │ 0b110. │ 0xC   │ 3     │ UART                               │
  -- │ 0b111. │ 0xE   │ 4     │ Test status register               │
  -- ╰────────┴───────┴───────┴────────────────────────────────────╯

  peConfig
    | clashSimulation = peConfigSim
    | otherwise = peConfigRtl

  peConfigSim = unsafePerformIO $ do
    root <- findParentContaining "cabal.project"
    let elfPath = root </> firmwareBinariesDir "riscv32imc" Release </> "hello"
    pure
      PeConfig
        { initI = Reloadable (Vec $ unsafeVecFromElfInstr @IMemWords BigEndian elfPath)
        , prefixI = 0b100
        , initD = Reloadable (Vec $ unsafeVecFromElfData @DMemWords BigEndian elfPath)
        , prefixD = 0b010
        , iBusTimeout = d0 -- No timeouts on the instruction bus
        , dBusTimeout = d0 -- No timeouts on the data bus
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
