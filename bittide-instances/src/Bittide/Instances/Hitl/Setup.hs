-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Hitl.Setup (
  -- * Constants
  DeviceInfo (..),
  FpgaCount,
  LinkCount,
  FpgaId,

  -- * Topology
  allHwTargets,
  channelNames,
  clockPaths,
  fpgaSetup,
  knownFpgaIds,
  knownFpgaIdsVec,
  linkMask,
  linkMasks,
  demoRigInfo,
  debugDeviceInfo,
) where

import Clash.Prelude

import Bittide.Hitl (DeviceInfo (..), FpgaId, HwTargetRef (..))
import Bittide.Topology

-- | The number of FPGAs in the current setup
type FpgaCount = 8 :: Nat

type LinkCount = FpgaCount - 1

channelNames :: Vec LinkCount String
channelNames =
  "X0Y10" :> "X0Y9" :> "X0Y16" :> "X0Y17" :> "X0Y18" :> "X0Y19" :> "X0Y11" :> Nil

clockPaths :: Vec LinkCount String
clockPaths =
  "clk0" :> "clk0" :> "clk0-2" :> "clk0-2" :> "clk0-2" :> "clk0-2" :> "clk0" :> Nil

{- | Some order of the FPGA ids and a mapping to their connected
neighbors (via the index position in the vector) according to the
different hardware interfaces on the boards.
-}
fpgaSetup :: Vec FpgaCount (FpgaId, Vec LinkCount (Index FpgaCount))
{- FOURMOLU_DISABLE -}
fpgaSetup =
    -- FPGA Id          SFP0 SFP1 J4   J5   J6   J7   SMA
       ("210308B3B272", 3 :> 2 :> 4 :> 5 :> 6 :> 7 :> 1 :> Nil)
    :> ("210308B0992E", 2 :> 3 :> 5 :> 6 :> 7 :> 4 :> 0 :> Nil)
    :> ("210308B0AE73", 1 :> 0 :> 6 :> 7 :> 4 :> 5 :> 3 :> Nil)
    :> ("210308B0AE6D", 0 :> 1 :> 7 :> 4 :> 5 :> 6 :> 2 :> Nil)
    :> ("210308B0AFD4", 7 :> 6 :> 0 :> 3 :> 2 :> 1 :> 5 :> Nil)
    :> ("210308B0AE65", 6 :> 7 :> 1 :> 0 :> 3 :> 2 :> 4 :> Nil)
    :> ("210308B3A22D", 5 :> 4 :> 2 :> 1 :> 0 :> 3 :> 7 :> Nil)
    :> ("210308B0B0C2", 4 :> 5 :> 3 :> 2 :> 1 :> 0 :> 6 :> Nil)
    :> Nil
{- FOURMOLU_ENABLE -}

{- | The IDs of the Digilent chips on each of the FPGA boards of the test
setup. The indices match the position of each FPGA in the mining rig.
-}
knownFpgaIdsVec :: Vec FpgaCount FpgaId
knownFpgaIdsVec = fst <$> fpgaSetup

{- | The IDs of the Digilent chips on each of the FPGA boards of the test
setup. The indices match the position of each FPGA in the mining rig.
-}
knownFpgaIds :: [FpgaId]
knownFpgaIds = toList knownFpgaIdsVec

allHwTargets :: [HwTargetRef]
allHwTargets = (\d -> HwTargetById d.deviceId d) <$> demoRigInfo

-- | Determines whether a link is active for a given FPGA and topology.
isActiveLink ::
  forall n.
  (KnownNat n, n <= FpgaCount) =>
  Topology n ->
  Index n ->
  Index LinkCount ->
  Bool
isActiveLink topology fpgaNr linkNr
  | sourceFpgaNr >= natToNum @n = False
  | otherwise = hasEdge topology (fromIntegral sourceFpgaNr) fpgaNr
 where
  (_, links) = fpgaSetup !! fpgaNr
  sourceFpgaNr = links !! linkNr

{- | Determines the link mask of a particular node.

>>> import Data.Graph
>>> import Clash.Prelude
>>> import Bittide.Topology
>>> let graph = complete d3
>>> pack (linkMask graph 0)
0b010_0001
>>> pack (linkMask graph 1)
0b100_0001
>>> pack (linkMask graph 2)
0b110_0000
-}
linkMask ::
  forall n.
  (KnownNat n, n <= FpgaCount) =>
  Topology n ->
  -- | FPGA number
  Index n ->
  Vec LinkCount Bool
linkMask topology fpgaNr = isActiveLink topology fpgaNr <$> indicesI

linkMasks ::
  forall n.
  (KnownNat n, n <= FpgaCount) =>
  Topology n ->
  Vec n (BitVector LinkCount)
linkMasks topology = pack . linkMask topology <$> indicesI

-- | List of device information of all FPGAs connected to the demo rig
demoRigInfo :: [DeviceInfo]
demoRigInfo =
  [ DeviceInfo
      { deviceId = "210308B3B272"
      , dna = 0x400200010169c040044164c5
      , serial = "/dev/serial/by-path/pci-0000:00:14.0-usb-0:5.4.4.2:1.1-port0"
      , usbAdapterLocation = "1-5.4.4.2:1"
      }
  , DeviceInfo
      { deviceId = "210308B0992E"
      , dna = 0x40020001815160e805108285
      , serial = "/dev/serial/by-path/pci-0000:00:14.0-usb-0:5.4.4.1:1.1-port0"
      , usbAdapterLocation = "1-5.4.4.1:1"
      }
  , DeviceInfo
      { deviceId = "210308B0AE73"
      , dna = 0x4002000101695ce72c808445
      , serial = "/dev/serial/by-path/pci-0000:00:14.0-usb-0:5.4.3:1.1-port0"
      , usbAdapterLocation = "1-5.4.3:1"
      }
  , DeviceInfo
      { deviceId = "210308B0AE6D"
      , dna = 0x4002000101695ce72c702305
      , serial = "/dev/serial/by-path/pci-0000:00:14.0-usb-0:5.4.2:1.1-port0"
      , usbAdapterLocation = "1-5.4.2:1"
      }
  , DeviceInfo
      { deviceId = "210308B0AFD4"
      , dna = 0x40020001016ba8e52581a285
      , serial = "/dev/serial/by-path/pci-0000:00:14.0-usb-0:5.4.1:1.1-port0"
      , usbAdapterLocation = "1-5.4.1:1"
      }
  , DeviceInfo
      { deviceId = "210308B0AE65"
      , dna = 0x400200010157f4862d01c345
      , serial = "/dev/serial/by-path/pci-0000:00:14.0-usb-0:5.3:1.1-port0"
      , usbAdapterLocation = "1-5.3:1"
      }
  , DeviceInfo
      { deviceId = "210308B3A22D"
      , dna = 0x400200010169c04004308185
      , serial = "/dev/serial/by-path/pci-0000:00:14.0-usb-0:5.2:1.1-port0"
      , usbAdapterLocation = "1-5.2:1"
      }
  , DeviceInfo
      { deviceId = "210308B0B0C2"
      , dna = 0x40020001015664862d20e405
      , serial = "/dev/serial/by-path/pci-0000:00:14.0-usb-0:5.1:1.1-port0"
      , usbAdapterLocation = "1-5.1:1"
      }
  ]

-- | Device information of the FPGA connected to our debug setup. Do not use this on CI.
debugDeviceInfo :: DeviceInfo
debugDeviceInfo =
  DeviceInfo
    { deviceId = "210308B3B018"
    , dna = 0x4002000101604ee70cc0e085
    , serial = "/dev/serial/by-path/pci-0000:02:00.0-usb-0:2:1.1-port0"
    , usbAdapterLocation = "1-2:1"
    }
