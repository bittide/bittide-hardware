-- SPDX-FileCopyrightText: 2026 QBayLogic
--
-- SPDX-License-Identifier: Apache-2.0
-- Defined in GenericDemo
{-# OPTIONS_GHC -Wno-missing-signatures #-}

{- | Top entity for the Manticore demo: the shared 'demoTest' skeleton wired up
with the Manticore user core (which holds the 'manticoreBittideChip' blackbox).

The foreign Manticore Verilog (emitted by manticore-hw @-t bittide@) is pulled
into synthesis through 'externalHdl'; point @MANTICORE_HDL_SRC@ at the
generator's output directory. The Clash-simulation build does not need it (the
blackbox has an idle simulation stub).
-}
module Bittide.Instances.Hitl.ManticoreDemo.TopEntity where

import Bittide.Hitl (HitlTestGroup (..))
import Bittide.Instances.Hitl.GenericDemo.TopEntity (demoTest, mkTests)
import Bittide.Instances.Hitl.ManticoreDemo.UserCore (mkUserCore, ringBufferDepth)
import Clash.Annotations.TH (makeTopEntity)

import qualified Bittide.Instances.Hitl.ManticoreDemo.Driver as Driver

manticoreDemoTest = demoTest ringBufferDepth mkUserCore
{-# OPAQUE manticoreDemoTest #-}
makeTopEntity 'manticoreDemoTest

tests :: HitlTestGroup
tests =
  baseTests
    { externalHdl =
        [ "$env(MANTICORE_HDL_SRC)/*.v"
        ]
    }
 where
  baseTests = mkTests 'manticoreDemoTest Driver.driver
