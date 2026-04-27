-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Tests.TimeWb where

import Clash.Prelude

import Bittide.Cpus.Riscv32imc (vexRiscv0)
import Bittide.Instances.Common (PeConfigElfSource (NameOnly), emptyPeConfig, peConfigFromElf)
import Bittide.ProcessingElement
import Bittide.SharedTypes (withLittleEndian)
import Bittide.Wishbone
import Project.FilePath

import Clash.Class.BitPackC (BitPackC)
import Protocols
import Protocols.Idle
import Protocols.MemoryMap (Mm, getMMAny, unMemmap)
import Protocols.MemoryMap.TypeDescription
import VexRiscv (DumpVcd (NoDumpVcd))

import qualified Protocols.MemoryMap as Mm

data TestStatus = Running | Success | Fail
  deriving (Show, Eq, Generic, NFDataX, BitPack, BitPackC)
deriveTypeDescription ''TestStatus

-- | Memory map for the C timer test
timeWbMm :: Mm.MemoryMap
timeWbMm =
  getMMAny
    $ withClockResetEnable @System clockGen resetGen enableGen
    $ dutCpu
    $ emptyPeConfig iMemWords dMemWords d0 d0 False vexRiscv0

timeWbMmC :: Mm.MemoryMap
timeWbMmC =
  getMMAny
    $ withClockResetEnable @System clockGen resetGen enableGen
    $ dutCpu
    $ emptyPeConfig iMemWordsC dMemWordsC d0 d0 False vexRiscv0

-- | DUT for C timer test - VexRiscv with UART and Timer peripherals
dutCpu ::
  (HiddenClockResetEnable dom, 1 <= DomainPeriod dom) =>
  PeConfig 4 ->
  Circuit (ToConstBwd Mm, ()) (Df dom (BitVector 8))
dutCpu peConfig = withLittleEndian $ circuit $ \(mm, _unit) -> do
  (uartRx, jtag) <- idleSource
  [uartBus, (mmTime, timeBus)] <-
    processingElement NoDumpVcd peConfig -< (mm, jtag)
  (uartTx, _uartStatus) <- uartInterfaceWb d2 d2 uartBytes -< (uartBus, uartRx)
  _localCounter <- timeWb Nothing -< (mmTime, timeBus)
  idC -< uartTx
{-# OPAQUE dutCpu #-}

dutNoMm ::
  (HiddenClockResetEnable dom, 1 <= DomainPeriod dom) =>
  PeConfig 4 ->
  Circuit () (Df dom (BitVector 8))
dutNoMm = unMemmap . dutCpu

type IMemWords = DivRU (8 * 1024) 4
type DMemWords = DivRU (4 * 1024) 4

iMemWords :: SNat IMemWords
iMemWords = SNat

dMemWords :: SNat DMemWords
dMemWords = SNat

type IMemWordsC = DivRU (4 * 1024) 4
type DMemWordsC = DivRU (4 * 1024) 4

iMemWordsC :: SNat IMemWordsC
iMemWordsC = SNat

dMemWordsC :: SNat DMemWordsC
dMemWordsC = SNat

peConfigSim ::
  (KnownNat iMem, 1 <= iMem, KnownNat dMem, 1 <= dMem) =>
  SNat iMem ->
  SNat dMem ->
  String ->
  IO (PeConfig 4)
peConfigSim iMem dMem binName =
  peConfigFromElf
    iMem
    dMem
    (NameOnly binName)
    Release
    d0
    d0
    False
    vexRiscv0
