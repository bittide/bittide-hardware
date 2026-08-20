-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
-- Defined in GenericDemo
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Bittide.Instances.Hitl.DecodeDemo.TopEntity where

import Prelude (Maybe (..))

import Bittide.Hitl (HitlTestGroup)
import Bittide.Instances.Hitl.DecodeDemo.UserCore (mkUserCore, ringBufferDepth)
import Bittide.Instances.Hitl.GenericDemo.TopEntity (demoTest, mkTests)
import Clash.Annotations.TH (makeTopEntity)

import qualified Bittide.Instances.Hitl.DecodeDemo.Driver as Driver
import qualified Bittide.Instances.Hitl.DecodeDemo.PostProc as PostProc

decodeDemoTest = demoTest ringBufferDepth mkUserCore
{-# OPAQUE decodeDemoTest #-}
makeTopEntity 'decodeDemoTest

tests :: HitlTestGroup
tests = mkTests "Decode_Demo_DUT" 'decodeDemoTest Driver.driver (Just PostProc.postProc)
