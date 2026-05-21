-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
-- Defined in GenericDemo
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Bittide.Instances.Hitl.AsyncCommsDemo.TopEntity where

import Bittide.Hitl (HitlTestGroup)
import Bittide.Instances.Hitl.AsyncCommsDemo.UserCore (mkUserCore, ringBufferDepth)
import Bittide.Instances.Hitl.GenericDemo.TopEntity (demoTest, mkTests)
import Clash.Annotations.TH (makeTopEntity)

import qualified Bittide.Instances.Hitl.AsyncCommsDemo.Driver as Driver

asyncCommsDemoTest = demoTest ringBufferDepth mkUserCore
{-# OPAQUE asyncCommsDemoTest #-}
makeTopEntity 'asyncCommsDemoTest

tests :: HitlTestGroup
tests = mkTests 'asyncCommsDemoTest Driver.driver
