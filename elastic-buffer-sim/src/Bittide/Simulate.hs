{-|

Provides a rudimentary simulation of elastic buffers.

-}

-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE OverloadedStrings #-}

module Bittide.Simulate where

import Clash.Prelude
import Clash.Signal.Internal
import GHC.Stack
import Numeric.Natural

import Bittide.ClockControl

-- Number of type aliases for documentation purposes in various functions defined
-- down below.
type StepSize = Natural
type InitialPeriod = Natural
type Offset = Signed 64
type DynamicRange = Natural


-- | Determines how 'elasticBuffer' should respond to underflow/overflow.
data OverflowMode
  -- | Saturate at empty/full boundaries. Useful for testing purposes.
  = Saturate
  -- | Error on write-when-full, or read-when-empty. This mode shuold be used in
  -- practise. Overflowing elastic buffer cannot happen in a real Bittide system.
  | Error

-- | Simple model of a FIFO that only models the interesting part for conversion:
-- data counts.
elasticBuffer ::
  forall readDom writeDom.
  (HasCallStack, KnownDomain readDom, KnownDomain writeDom) =>
  -- | What behavior to pick on underflow/overflow
  OverflowMode ->
  -- | Size of FIFO. To reflect our target platforms, this should be a power of two
  -- where typical sizes would probably be: 16, 32, 64, 128.
  ElasticBufferSize ->
  Clock readDom ->
  Clock writeDom ->
  Signal readDom DataCount
elasticBuffer mode size clkRead clkWrite
  | Clock _ (Just readPeriods) <- clkRead
  , Clock _ (Just writePeriods) <- clkWrite
  = go 0 (targetDataCount size) readPeriods writePeriods
 where
  go !relativeTime !fillLevel rps wps@(writePeriod :- _) =
    if relativeTime < toInteger writePeriod
    then goRead relativeTime fillLevel rps wps
    else goWrite relativeTime fillLevel rps wps

  goWrite relativeTime fillLevel rps (writePeriod :- wps) =
    go (relativeTime - toInteger writePeriod) newFillLevel rps wps
   where
    newFillLevel
      | fillLevel >= size = case mode of
          Saturate -> fillLevel
          Error -> error "elasticBuffer: overflow"
      | otherwise = fillLevel + 1

  goRead relativeTime fillLevel (readPeriod :- rps) wps =
    newFillLevel :- go (relativeTime + toInteger readPeriod) newFillLevel rps wps
   where
    newFillLevel
      | fillLevel <= 0 = case mode of
          Saturate -> 0
          Error -> error "elasticBuffer: underflow"
      | otherwise = fillLevel - 1

elasticBuffer mode size (Clock ss Nothing) clock1 =
  -- Convert read clock to a "dynamic" clock if it isn't one
  let
    period = snatToNum (clockPeriod @readDom)
  in
    elasticBuffer mode size (Clock ss (Just (pure period))) clock1

elasticBuffer mode size clock0 (Clock ss Nothing) =
  -- Convert write clock to a "dynamic" clock if it isn't one
  let
    period = snatToNum (clockPeriod @writeDom)
  in
    elasticBuffer mode size clock0 (Clock ss (Just (pure period)))
