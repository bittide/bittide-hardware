-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

module Bittide.Instances.Tests.RegisterWbC (
  dut,
  memoryMap,
  sim,
  simResult,
) where

import Clash.Prelude

-- Local
import Bittide.DoubleBufferedRam (
  ContentType (Vec),
  InitialContent (Reloadable),
 )
import Bittide.Instances.Domains (Basic50)
import Bittide.ProcessingElement (
  PeConfig (
    PeConfig,
    dBusTimeout,
    iBusTimeout,
    includeIlaWb,
    initD,
    initI,
    prefixD,
    prefixI
  ),
  processingElement,
 )
import Bittide.ProcessingElement.Util (
  vecFromElfData,
  vecFromElfInstr,
 )
import Bittide.SharedTypes (ByteOrder (BigEndian), Bytes)
import Bittide.Wishbone (uartInterfaceWb, uartSim)
import Project.FilePath (
  CargoBuildType (Release),
  findParentContaining,
  firmwareBinariesDir,
 )

-- Other

import Data.Char (chr)
import Data.Maybe (catMaybes)
import Protocols (Circuit (Circuit), Df, Drivable (sampleC), idC, toSignals)
import Protocols.Idle (idleSource)
import Protocols.MemoryMap (ConstBwd, MM, MemoryMap, constBwd, getMMAny)
import Protocols.MemoryMap.Registers.WishboneStandard (
  deviceWbC,
  registerConfig,
  registerWbC_,
 )
import Protocols.Wishbone (Wishbone, WishboneMode (Standard))
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty.HUnit (HasCallStack)
import VexRiscv (DumpVcd (NoDumpVcd))

-- | Example that exports a bunch of different types.
manyTypesWb ::
  forall wordSize aw dom.
  ( HasCallStack
  , HiddenClock dom
  , HiddenReset dom
  , KnownNat wordSize
  , KnownNat aw
  , 1 <= wordSize
  , 4 <= aw
  ) =>
  Circuit
    (ConstBwd MM, Wishbone dom 'Standard aw (Bytes wordSize))
    ()
manyTypesWb = circuit $ \(mm, wb) -> do
  [wbS0, wbS1, wbS2, wbS3, wbU0, wbU1, wbU2] <-
    deviceWbC "ManyTypes" -< (mm, wb)

  registerWbC_ hasClock hasReset (registerConfig "s0") initWbS0 -< (wbS0, Fwd noWrite)
  registerWbC_ hasClock hasReset (registerConfig "s1") initWbS1 -< (wbS1, Fwd noWrite)
  registerWbC_ hasClock hasReset (registerConfig "s2") initWbS2 -< (wbS2, Fwd noWrite)
  registerWbC_ hasClock hasReset (registerConfig "s3") initWbS3 -< (wbS3, Fwd noWrite)

  registerWbC_ hasClock hasReset (registerConfig "u0") initWbU0 -< (wbU0, Fwd noWrite)
  registerWbC_ hasClock hasReset (registerConfig "u1") initWbU1 -< (wbU1, Fwd noWrite)
  registerWbC_ hasClock hasReset (registerConfig "u2") initWbU2 -< (wbU2, Fwd noWrite)

  idC
 where
  initWbS0 :: Signed 8
  initWbS0 = -8

  initWbS1 :: Signed 8
  initWbS1 = 8

  initWbS2 :: Signed 16
  initWbS2 = 16

  initWbS3 :: Signed 64
  initWbS3 = 3721049880298531338

  initWbU0 :: Unsigned 8
  initWbU0 = 8

  initWbU1 :: Unsigned 16
  initWbU1 = 16

  initWbU2 :: Unsigned 64
  initWbU2 = 3721049880298531338

  noWrite :: forall a. Signal dom (Maybe a)
  noWrite = pure Nothing

sim :: IO ()
sim = putStrLn simResult

simResult :: String
simResult = chr . fromIntegral <$> catMaybes uartStream
 where
  uartStream = sampleC def dut0

  dut0 :: Circuit () (Df Basic50 (BitVector 8))
  dut0 = Circuit $ ((),) . snd . toSignals dut . ((),) . snd

{- | An instance connecting a vexriscv to a UART and a memory mapped devices that
has many (read/write) registers.
-}
dut :: Circuit (ConstBwd MM) (Df Basic50 (BitVector 8))
dut = withClockResetEnable clockGen resetGen enableGen
  $ circuit
  $ \mm -> do
    (uartRx, jtag) <- idleSource -< ()
    [(prefixUart, (mmUart, uartBus)), (prefixManyTypes, manyTypes)] <-
      processingElement NoDumpVcd peConfig -< (mm, jtag)
    (uartTx, _uartStatus) <- uartInterfaceWb d2 d2 uartSim -< (mmUart, (uartBus, uartRx))
    constBwd 0b00 -< prefixUart
    manyTypesWb -< manyTypes
    constBwd 0b11 -< prefixManyTypes
    idC -< uartTx
 where
  peConfig = unsafePerformIO $ do
    root <- findParentContaining "cabal.project"
    let elfPath = root </> firmwareBinariesDir "riscv32imc" Release </> "registerwbc_test"
    pure
      PeConfig
        { initI =
            Reloadable @IMemWords
              $ Vec
              $ unsafePerformIO
              $ vecFromElfInstr BigEndian elfPath
        , prefixI = 0b10
        , initD =
            Reloadable @DMemWords
              $ Vec
              $ unsafePerformIO
              $ vecFromElfData BigEndian elfPath
        , prefixD = 0b01
        , iBusTimeout = d0 -- No timeouts on the instruction bus
        , dBusTimeout = d0 -- No timeouts on the data bus
        , includeIlaWb = False
        }
{-# NOINLINE dut #-}

memoryMap :: MemoryMap
memoryMap = getMMAny dut0
 where
  dut0 :: Circuit (ConstBwd MM, Df System ()) ()
  dut0 = circuit $ \(mm, _df) -> do
    _uart <- dut -< mm
    idC

type IMemWords = DivRU (128 * 1024) 4
type DMemWords = DivRU (128 * 1024) 4
