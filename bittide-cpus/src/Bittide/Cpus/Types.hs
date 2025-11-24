-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Bittide.Cpus.Types where

import Clash.Prelude

import VexRiscv (CpuIn, CpuOut, DumpVcd, JtagIn, JtagOut)
import VexRiscv.Reset (MinCyclesReset)

type BittideCpu dom =
  (KnownDomain dom) =>
  DumpVcd ->
  Clock dom ->
  MinCyclesReset dom 2 ->
  Signal dom CpuIn ->
  Signal dom JtagIn ->
  ( Signal dom CpuOut
  , Signal dom JtagOut
  )
