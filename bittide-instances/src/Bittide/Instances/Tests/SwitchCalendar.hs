-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Bittide.Instances.Tests.SwitchCalendar where

import Bittide.DoubleBufferedRam (
  ContentType (Vec),
  InitialContent (Reloadable),
 )
import Bittide.Instances.Domains (Basic200)
import Bittide.Instances.Pnr.Switch
import Bittide.ProcessingElement
import Bittide.ProcessingElement.Util (
  vecFromElfData,
  vecFromElfInstr,
 )
import Bittide.SharedTypes
import Bittide.Wishbone hiding (MemoryMap)
import Clash.Class.BitPackC
import Clash.Explicit.Prelude
import Clash.Prelude (withClockResetEnable)
import Data.Char
import Data.Maybe
import GHC.Stack (HasCallStack)
import Project.FilePath (
  CargoBuildType (..),
  findParentContaining,
  firmwareBinariesDir,
 )
import Protocols
import Protocols.Idle
import Protocols.MemoryMap
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import VexRiscv (DumpVcd (DumpVcd, NoDumpVcd))

memoryMap :: (HasCallStack) => MemoryMap
memoryMap = (\(SimOnly mm, _) -> mm) $ toSignals dut ((), pure $ deepErrorX "memoryMap")

main :: IO ()
main = sim

sim :: IO ()
sim = putStrLn simResult

simResult :: String
simResult = chr . fromIntegral <$> catMaybes uartStream
 where
  uartStream = sampleC def dut0

  dut0 :: Circuit () (Df Basic200 (BitVector 8))
  dut0 = Circuit $ ((),) . snd . toSignals dut . ((),) . snd

dut :: Circuit (ConstBwd MM) (Df Basic200 (BitVector 8))
dut =
  withBittideByteOrder
    $ withClockResetEnable clockGen (resetGenN d2) enableGen
    $ circuit
    $ \mm -> do
      (uartRx, jtag) <- idleSource
      [uartBus, (switchMm, switchCalWb)] <-
        processingElement dumpVcd peConfig -< (mm, jtag)
      (uartTx, _uartStatus) <- uartInterfaceWb d2 d2 uartBytes -< (uartBus, uartRx)
      _switchResult <-
        switchExample clockGen (resetGenN d2) -< (switchMm, (Fwd (repeat $ pure 0), switchCalWb))
      idC -< uartTx
 where
  dumpVcd =
    unsafePerformIO $ do
      mVal <- lookupEnv "SWITCHCALENDAR_DUMP_VCD"
      case mVal of
        Just s -> pure (DumpVcd s)
        _ -> pure NoDumpVcd

  peConfig = unsafePerformIO $ do
    root <- findParentContaining "cabal.project"
    let elfPath = root </> firmwareBinariesDir "riscv32imc" Release </> "switch_calendar_test"
    pure
      PeConfig
        { initI =
            Reloadable @IMemWords
              $ Vec
              $ unsafePerformIO
              $ vecFromElfInstr BigEndian elfPath
        , initD =
            Reloadable @DMemWords
              $ Vec
              $ unsafePerformIO
              $ vecFromElfData BigEndian elfPath
        , iBusTimeout = d0 -- No timeouts on the instruction bus
        , dBusTimeout = d0 -- No timeouts on the data bus
        , includeIlaWb = False
        }
{-# OPAQUE dut #-}

type IMemWords = DivRU (128 * 1024) 4
type DMemWords = DivRU (128 * 1024) 4
