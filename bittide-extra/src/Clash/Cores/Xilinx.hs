-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | This module contains instances of the various CDC primitive typeclasses for Xilinx. For help
with choosing primitives, please consult Xilinx's documentation:
<https://docs.amd.com/r/en-US/ug949-vivado-design-methodology/Single-Bit-CDC>
-}
module Clash.Cores.Xilinx where

import Clash.Prelude

import Clash.Cores.Xilinx.Xpm.Cdc.ArraySingle (XpmCdcArraySingleConfig (..), xpmCdcArraySingleWith)
import Clash.Cores.Xilinx.Xpm.Cdc.Gray (XpmCdcGrayConfig (..), xpmCdcGrayWith)
import Clash.Cores.Xilinx.Xpm.Cdc.Handshake (XpmCdcHandshakeConfig (..), xpmCdcHandshakeWith)
import Clash.Cores.Xilinx.Xpm.Cdc.Pulse (XpmCdcPulseConfig (..), xpmCdcPulseWith)
import Clash.Cores.Xilinx.Xpm.Cdc.Single (XpmCdcSingleConfig (..), xpmCdcSingleWith)
import Clash.Cores.Xilinx.Xpm.Cdc.SyncRst (
  Asserted (..),
  XpmCdcSyncRstConfig (..),
  xpmCdcSyncRstWith,
 )
import Data.Data (Proxy (Proxy))

import qualified Clash.Class.Cdc as Cdc

type Xilinx = "Xilinx"

withXilinx :: forall r. ((Cdc.HiddenVendor Xilinx) => r) -> r
withXilinx = Cdc.withVendorI @Xilinx

genInitialValues ::
  forall src dst. (KnownDomain src, KnownDomain dst) => Proxy src -> Proxy dst -> String -> Bool
genInitialValues (_ :: Proxy src) (_ :: Proxy dst) compName =
  case (initBehavior @src, initBehavior @dst) of
    (SDefined, SDefined) -> True
    (SUnknown, SUnknown) -> False
    _ ->
      clashCompileError
        $ compName
        <> ": domains need to agree on initial value "
        <> "behavior. To set initial value usage explicitly, "
        <> "consider using '"
        <> compName
        <> "With'."

instance Cdc.IndependentBits Xilinx where
  type
    IndependentBitsWithConstraints Xilinx stages a src dst =
      ( 2 <= stages
      , stages <= 10
      , 1 <= BitSize a
      , BitSize a <= 1024
      )
  type IndependentBitsConfig Xilinx stages a src dst = XpmCdcArraySingleConfig stages
  independentBitsWith = xpmCdcArraySingleWith
  type IndependentBitsConstraints Xilinx a src dst = (1 <= BitSize a, BitSize a <= 1024)
  type IndependentBitsDefaultStages Xilinx a src dst = 4
  defaultIndependentBitsConfig (_ :: Proxy src) (_ :: Proxy dst) (_ :: Proxy a) =
    XpmCdcArraySingleConfig
      { stages = SNat
      , initialValues = genInitialValues @src @dst Proxy Proxy "xpmCdcArraySingle"
      , registerInput = True
      }

instance Cdc.Gray Xilinx where
  type GrayWithConstraints Xilinx stages n src dst = (2 <= stages, stages <= 10, 2 <= n, n <= 32)
  type GrayConfig Xilinx stages n src dst = XpmCdcGrayConfig stages
  grayWith = xpmCdcGrayWith
  type GrayConstraints Xilinx n src dst = (2 <= n, n <= 32)
  type GrayDefaultStages Xilinx n src dst = 4
  defaultGrayConfig (_ :: Proxy src) (_ :: Proxy dst) (_ :: Proxy n) =
    XpmCdcGrayConfig
      { stages = SNat
      , initialValues = genInitialValues @src @dst Proxy Proxy "xpmCdcGray"
      }

instance Cdc.Handshake Xilinx where
  type
    HandshakeWithConstraints Xilinx srcStages dstStages a src dst =
      ( 2 <= srcStages
      , srcStages <= 10
      , 2 <= dstStages
      , dstStages <= 10
      , 1 <= BitSize a
      , BitSize a <= 1024
      )
  type
    HandshakeConfig Xilinx srcStages dstStages a src dst =
      XpmCdcHandshakeConfig srcStages dstStages
  handshakeWith = xpmCdcHandshakeWith
  type HandshakeConstraints Xilinx a src dst = (1 <= BitSize a, BitSize a <= 1024)
  type HandshakeDefaultSrcStages Xilinx a src dst = 4
  type HandshakeDefaultDstStages Xilinx a src dst = 4
  defaultHandshakeConfig (_ :: Proxy src) (_ :: Proxy dst) (_ :: Proxy a) =
    XpmCdcHandshakeConfig
      { srcStages = SNat
      , dstStages = SNat
      , initialValues = genInitialValues @src @dst Proxy Proxy "xpmCdcHandshake"
      }

instance Cdc.Pulse Xilinx where
  type PulseWithConstraints Xilinx stages a src dst = (2 <= stages, stages <= 10, BitSize a ~ 1)
  type PulseConfig Xilinx stages a src dst = XpmCdcPulseConfig stages
  pulseWith = xpmCdcPulseWith
  type PulseConstraints Xilinx a src dst = (BitSize a ~ 1)
  type PulseDefaultStages Xilinx a src dst = 4
  defaultPulseConfig (_ :: Proxy src) (_ :: Proxy dst) (_ :: Proxy a) =
    XpmCdcPulseConfig
      { stages = SNat
      , initialValues = genInitialValues @src @dst Proxy Proxy "xpmCdcPulse"
      , registerOutput = True
      , resetUsed = False
      }

instance Cdc.Bit Xilinx where
  type BitWithConstraints Xilinx stages a src dst = (2 <= stages, stages <= 10, BitSize a ~ 1)
  type BitConfig Xilinx stages a src dst = XpmCdcSingleConfig stages
  bitWith = xpmCdcSingleWith
  type BitConstraints Xilinx a src dst = (BitSize a ~ 1)
  type BitDefaultStages Xilinx a src dst = 4
  defaultBitConfig (_ :: Proxy src) (_ :: Proxy dst) (_ :: Proxy a) =
    XpmCdcSingleConfig
      { stages = SNat
      , initialValues = genInitialValues @src @dst Proxy Proxy "xpmCdcSingle"
      , registerInput = True
      }

instance Cdc.SyncRst Xilinx where
  type SyncRstWithConstraints Xilinx stages src dst = (2 <= stages, stages <= 10)
  type SyncRstConfig Xilinx stages src dst = XpmCdcSyncRstConfig stages
  syncRstWith = xpmCdcSyncRstWith
  type SyncRstConstraints Xilinx src dst = ()
  type ResetAssert Xilinx = Asserted
  defaultAssert = Cdc.asserted @Xilinx
  asserted = Asserted
  deasserted = Deasserted
  type SyncRstDefaultStages Xilinx src dst = 4
  defaultSyncRstConfig resetAsserted (_ :: Proxy src) (_ :: Proxy dst) =
    XpmCdcSyncRstConfig
      { stages = SNat
      , initialValues = case initBehavior @dst of
          SDefined -> Just resetAsserted
          SUnknown -> Nothing
      }
