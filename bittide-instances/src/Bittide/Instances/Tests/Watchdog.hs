-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Bittide.Instances.Tests.Watchdog where

-- Preludes
import Clash.Prelude

-- Local
import Bittide.Instances.Common (PeConfigElfSource (NameOnly), emptyPeConfig, peConfigFromElf)
import Bittide.Instances.Domains
import Bittide.ProcessingElement
import Bittide.SharedTypes (withLittleEndian)
import Bittide.Wishbone hiding (MemoryMap)
import Project.FilePath

-- Other
import Bittide.Cpus.Riscv32imc (vexRiscv0)
import GHC.Stack (HasCallStack)
import Protocols
import Protocols.Idle
import Protocols.MemoryMap
import VexRiscv (DumpVcd (NoDumpVcd))

-- Qualified
import qualified Protocols.ToConst as ToConst

{- | A simple instance containing just VexRisc and UART as peripheral.
Runs the `hello` binary from `firmware-binaries`.
-}
dutWithMm :: (HasCallStack) => PeConfig 6 -> Circuit (ToConstBwd Mm, ()) (Df Basic200 (BitVector 8))
dutWithMm peConfig = withLittleEndian
  $ withClockResetEnable clockGen (resetGenN d2) enableGen
  $ circuit
  $ \(mm, _unit) -> do
    (uartRx, jtag) <- idleSource
    [ uartBus
      , (mmTime, timeBus0)
      , (mmIdleA, idleBusA)
      , (mmIdleB, idleBusB)
      ] <-
      processingElement NoDumpVcd peConfig -< (mm, jtag)

    idleSink <| (watchDogWb @_ @_ @4 "1 cycle" d1) -< idleBusA
    idleSink
      <| (watchDogWb @_ @_ @4 "50 us" (SNat @(PeriodToCycles Basic200 (Microseconds 50))))
      -< idleBusB
    ToConst.toBwd (SimOnly $ idleMm "IdleA") -< mmIdleA
    ToConst.toBwd (SimOnly $ idleMm "IdleB") -< mmIdleB

    timeBus1 <- watchDogWb @_ @_ @4 "" d0 -< timeBus0
    _localCounter <- timeWb Nothing -< (mmTime, timeBus1)
    (uartTx, _uartStatus) <-
      (uartInterfaceWb @_ @_ @4) d2 d2 uartBytes -< (uartBus, uartRx)
    idC -< uartTx
 where
  -- This defines a "fake" device. It's a writable location in memory, but we just use it to check
  -- that the watchdog works as intended. It doesn't actually do anything on hardware.
  idleMm devName =
    MemoryMap
      { tree = DeviceInstance locCaller devName
      , deviceDefs = deviceSingleton $ idleMmDevDef devName
      }
  idleMmDevDef devName =
    DeviceDefinition
      { deviceName = Name devName "Fake device, only writable for purposes of the test"
      , registers =
          [ NamedLoc
              { name = Name "idle" "Fake register, only writable for purposes of the test"
              , loc = locHere
              , value =
                  Register
                    { access = WriteOnly
                    , address = 0
                    , fieldType = regType @(())
                    , reset = Nothing
                    , tags = ["zero-width"]
                    }
              }
          ]
      , definitionLoc = locHere
      , tags = []
      }
{-# OPAQUE dutWithMm #-}

type IMemWords = DivRU (4 * 1024) 4
type DMemWords = DivRU (4 * 1024) 4

peConfigSim :: IO (PeConfig 6)
peConfigSim =
  peConfigFromElf
    (SNat @IMemWords)
    (SNat @DMemWords)
    (NameOnly "watchdog_test")
    Release
    d0
    d0
    False
    vexRiscv0

dutMm :: MemoryMap
dutMm =
  getMMAny
    $ dutWithMm
    $ emptyPeConfig (SNat @IMemWords) (SNat @DMemWords) d0 d0 False vexRiscv0

dut :: PeConfig 6 -> Circuit () (Df Basic200 (BitVector 8))
dut = unMemmap . dutWithMm
