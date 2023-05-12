-- SPDX-FileCopyrightText: 2022-2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Simulate.ElasticBuffer where

import Clash.Prelude
import GHC.Stack

import Bittide.Counter
import Bittide.ClockControl

-- | Simple model of a FIFO that only models the interesting part for conversion:
-- data counts.
elasticBuffer ::
  forall n readDom writeDom.
  (HasCallStack, KnownDomain readDom, KnownDomain writeDom, KnownNat n, n <= 65) =>
  Clock readDom ->
  Clock writeDom ->
  Signal readDom (DataCount n)
elasticBuffer clkRead clkWrite = resize . fst
  <$> domainDiffCounter @n @writeDom @readDom
        clkWrite resetGen enableGen
        clkRead resetGen enableGen
