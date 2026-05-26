-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
-- Defined in GenericDemo
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Bittide.Instances.Hitl.WireDemo.TopEntity where

import Bittide.Hitl (HitlTestGroup)
import Bittide.Instances.Hitl.GenericDemo.TopEntity (demoTest, mkTests)
import Bittide.Instances.Hitl.WireDemo.UserCore (mkUserCore, ringBufferDepth)
import Clash.Annotations.TH (makeTopEntity)

import qualified Bittide.Instances.Hitl.WireDemo.Driver as Driver

wireDemoTest = demoTest ringBufferDepth mkUserCore
{-# OPAQUE wireDemoTest #-}
makeTopEntity 'wireDemoTest

tests :: HitlTestGroup
tests = mkTests 'wireDemoTest Driver.driver
