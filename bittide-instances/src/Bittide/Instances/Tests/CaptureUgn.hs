-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Tests.CaptureUgn where

import Clash.Prelude
import Protocols

import Protocols.Idle (idleSource)
import Protocols.MemoryMap (MemoryMap, Mm, getMMAny, unMemmap)
import VexRiscv (DumpVcd (NoDumpVcd))

import Bittide.CaptureUgn (captureUgn)
import Bittide.Cpus.Riscv32imc (vexRiscv0)
import Bittide.ElasticBuffer (ElasticBufferData (Data))
import Bittide.Instances.Common (PeConfigElfSource (NameOnly), emptyPeConfig, peConfigFromElf)
import Bittide.ProcessingElement (PeConfig, processingElement)
import Bittide.SharedTypes (withLittleEndian)
import Bittide.Wishbone (uartBytes, uartInterfaceWb)
import Project.FilePath (CargoBuildType (Release))

import qualified Bittide.Cpus.Riscv32imc as Riscv32imc

{- | A simulation-only instance containing just VexRisc with UART and the captureUgn
peripheral which runs the `capture_ugn_test` binary from `firmware-binaries`.
-}
dutWithMm ::
  forall dom.
  ( HiddenClockResetEnable dom
  , 1 <= DomainPeriod dom
  ) =>
  -- | Procesing element configuration
  PeConfig 4 ->
  -- | Elastic buffer
  Signal dom (Maybe (BitVector 64)) ->
  -- | Local sequence counter
  Signal dom (Unsigned 64) ->
  Circuit (ToConstBwd Mm, ()) (Df dom (BitVector 8))
dutWithMm peConfig eb localCounter = withLittleEndian $ circuit $ \(mm, _unit) -> do
  (uartRx, jtagIdle) <- idleSource
  [uartBus, ugnBus] <- processingElement @dom NoDumpVcd peConfig -< (mm, jtagIdle)
  (uartTx, _uartStatus) <- uartInterfaceWb d2 d2 uartBytes -< (uartBus, uartRx)
  _bittideData <- captureUgn localCounter (Data <$> eb) -< ugnBus
  idC -< uartTx

type IMemWords = DivRU (4 * 1024) 4
type DMemWords = DivRU (4 * 1024) 4

peConfigSim :: IO (PeConfig 4)
peConfigSim =
  peConfigFromElf
    (SNat @IMemWords)
    (SNat @DMemWords)
    (NameOnly "capture_ugn_test")
    Release
    d0
    d0
    False
    Riscv32imc.vexRiscv0

memoryMap :: MemoryMap
memoryMap =
  getMMAny
    $ withClockResetEnable @System clockGen (resetGenN d2) enableGen
    $ dutWithMm emptyPeCfg (pure Nothing) (pure 0)
 where
  emptyPeCfg = emptyPeConfig (SNat @IMemWords) (SNat @DMemWords) d0 d0 False vexRiscv0

dut ::
  forall dom.
  ( HiddenClockResetEnable dom
  , 1 <= DomainPeriod dom
  ) =>
  -- | Procesing element configuration
  PeConfig 4 ->
  -- | Elastic buffer
  Signal dom (Maybe (BitVector 64)) ->
  -- | Local sequence counter
  Signal dom (Unsigned 64) ->
  Circuit () (Df dom (BitVector 8))
dut peConfig eb localCounter = unMemmap $ dutWithMm peConfig eb localCounter
