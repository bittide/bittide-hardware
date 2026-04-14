-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Protocols.Internal.Types.Extra where

import Clash.Prelude hiding (traceSignal)
import Data.Data (Proxy (..))
import Protocols

-- | Typeclass used for tracing circuits either with 'clash-shockwaves', 'Clash.Prelude.traceSignal', or 'Debug.Trace.trace'.
class TraceC p where
  -- | Tracing function that uses 'traceSignal' from the 'clash-shockwaves' package to
  -- create waveforms with ADTs.
  shock :: String -> Circuit p p

  -- | Tracing function that uses 'Clash.Prelude.traceSignal' to add signals to normal waveforms.
  signal :: String -> Circuit p p

  -- | Returns a list of signal names that will be added to the HDL when using 'shock'.
  -- useful in combination with 'dumpVCD' and friends.
  names :: Proxy p -> String -> [String]

  -- | Tracing function that uses 'Debug.Trace.trace' to print values to the console.
  trace :: String -> Circuit p p
