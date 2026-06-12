-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
-- Defined in GenericDemo
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Bittide.Instances.Hitl.SoftUgnDemo.TopEntity where

import Bittide.Hitl (HitlTestGroup)
import Bittide.Instances.Hitl.GenericDemo.Core (muConfig)
import Bittide.Instances.Hitl.GenericDemo.TopEntity (demoTest, mkTests)
import Bittide.Instances.Hitl.SoftUgnDemo.UserCore (mkUserCore, ringBufferDepth)
import Clash.Annotations.TH (makeTopEntity)

import qualified Bittide.Instances.Hitl.SoftUgnDemo.Driver as Driver

softUgnDemoTest = demoTest ringBufferDepth muConfig mkUserCore
{-# OPAQUE softUgnDemoTest #-}
makeTopEntity 'softUgnDemoTest

tests :: HitlTestGroup
tests = mkTests 'softUgnDemoTest Driver.driver
