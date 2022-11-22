-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Synchronizer where

import Clash.Explicit.Prelude

import Bittide.Instances.Domains

import qualified Clash.Cores.Extra as Cores
import Clash.Annotations.TH (makeTopEntity)


tripleFlipFlopSynchronizer ::
  "clk1"   ::: Clock Basic200 ->
  "clk2"   ::: Clock Basic199 ->
  "source" ::: Signal Basic200 Bit ->
  "target" ::: Signal Basic199 Bit
tripleFlipFlopSynchronizer clk1 clk2 =
  Cores.tripleFlipFlopSynchronizer @Basic200 @Basic199 @Bit clk1 clk2 0
makeTopEntity 'tripleFlipFlopSynchronizer
