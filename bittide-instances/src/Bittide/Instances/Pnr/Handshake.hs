-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Pnr.Handshake where

import Clash.Prelude
import Protocols

import Bittide.ElasticBuffer (ElasticBufferData (Data))
import Bittide.Handshake (Inputs (..), Outputs (..), handshakesWb)
import Bittide.Instances.Domains (Basic300)
import Bittide.Instances.Hacks (reducePins)
import Bittide.SharedTypes (withLittleEndian)

handshakesWbFast ::
  Clock Basic300 -> Reset Basic300 -> Signal Basic300 Bit -> Signal Basic300 Bit
handshakesWbFast clk rst = withClock clk $ reducePins dut
 where
  dut (unbundle -> (wbIn, fromNeighbors, fromCores)) = bundle (wbOut, fromOutputs outputs)
   where
    (((_mm, wbOut), _inputs), outputs) =
      toSignals
        ( withLittleEndian
            $ withClock clk
            $ withReset rst
            $ handshakesWb @6 @_ @32 @4
        )
        ((((), wbIn), inputs), units)

    inputs = Inputs (unbundle (fmap (fmap Data) fromNeighbors)) (unbundle fromCores)
    fromOutputs outs =
      bundle
        ( bundle outs.toNeighbors
        , bundle outs.toCores
        , bundle outs.toNeighborDones
        , bundle outs.toCoreDones
        , bundle outs.fromNeighborDones
        , bundle outs.fromCoreDones
        )
