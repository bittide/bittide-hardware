-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

-- XXX: Should upstream shockwave instances to @clash-protocols@?
{-# OPTIONS -Wno-orphans #-}

module Bittide.Wishbone.Orphans where

import Clash.Prelude

import Clash.Shockwaves.Waveform (Waveform)
import Protocols.Wishbone (
  BurstTypeExtension,
  CycleTypeIdentifier,
  WishboneM2S,
  WishboneS2M,
 )

instance (KnownNat aw, KnownNat wordSize) => Waveform (WishboneM2S aw wordSize)
instance (KnownNat wordSize) => Waveform (WishboneS2M wordSize)
instance Waveform CycleTypeIdentifier
instance Waveform BurstTypeExtension
