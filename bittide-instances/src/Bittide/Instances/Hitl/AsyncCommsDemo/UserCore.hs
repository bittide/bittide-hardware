-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{- | User core for the async-comms demo. It is identical to the soft-UGN demo's user core
(reused verbatim); the only difference between the two demos is that the async-comms demo
uses shallow @d128@-deep ring buffers.
-}
module Bittide.Instances.Hitl.AsyncCommsDemo.UserCore (
  UserCoreBusses,
  RingBufferDepth,
  ringBufferDepth,
  mkUserCore,
) where

import Clash.Explicit.Prelude

import Bittide.Instances.Hitl.SoftUgnDemo.UserCore (UserCoreBusses, mkUserCore)

-- | Shallow ring buffers: the one intended difference from the soft-UGN demo.
type RingBufferDepth = 128

ringBufferDepth :: SNat RingBufferDepth
ringBufferDepth = SNat
