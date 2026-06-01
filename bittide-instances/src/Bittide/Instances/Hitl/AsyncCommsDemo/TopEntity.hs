-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Bittide.Instances.Hitl.AsyncCommsDemo.TopEntity where

import Clash.Prelude

import Bittide.Hitl (HitlTestGroup)
import Bittide.Instances.Hitl.GenericDemo.TopEntity (demoTest, mkTests)
import Bittide.Instances.Hitl.SoftUgnDemo.UserCore (mkUserCore)
import Clash.Annotations.TH (makeTopEntity)

import qualified Bittide.Instances.Hitl.AsyncCommsDemo.Driver as Driver

smoltcpDemoTest = demoTest ringBufferDepth mkUserCore
 where
  ringBufferDepth = d128
{-# OPAQUE smoltcpDemoTest #-}
makeTopEntity 'smoltcpDemoTest

tests :: HitlTestGroup
tests = mkTests 'smoltcpDemoTest Driver.driver
