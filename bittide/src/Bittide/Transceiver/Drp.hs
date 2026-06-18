-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{- | A small master for the Xilinx GTH transceiver Dynamic Reconfiguration Port
(DRP). It serializes single read/write transactions issued by software (over
Wishbone, see "Bittide.Transceiver.Wishbone") onto the per-channel DRP buses of
the GTH channels.

The DRP is a simple synchronous interface (UG576, /Dynamic Reconfiguration
Port/): drive 'drpAddrs'/'drpDis', pulse 'drpEns' (and 'drpWes' for a write) for
one cycle, then wait for the channel's @drprdy@. On a read, @drpdo@ is valid the
cycle @drprdy@ is asserted.

This master keeps things deliberately simple: one transaction at a time, no
pipelining. Read-modify-write is done in software (read, modify, write); the
master only provides read and write primitives. A watchdog ('DrpTimeout') guards
against a missing @drprdy@ so a stuck channel cannot hang the engine.
-}
module Bittide.Transceiver.Drp where

import Clash.Explicit.Prelude

import Clash.Class.BitPackC (BitPackC)
import Protocols.MemoryMap.TypeDescription (deriveTypeDescription)

-- | DRP address width for UltraScale GTHE3 channels (@DRPADDR[8:0]@).
type DrpAddrWidth = 9

-- | DRP data width (@DRPDI[15:0]@ / @DRPDO[15:0]@).
type DrpDataWidth = 16

{- | Number of @drpclk@ cycles to wait for @drprdy@ before declaring a timeout.
UG576 specifies @drprdy@ within a bounded number of cycles (≤ 500); this is a
generous over-estimate.
-}
type DrpTimeout = 1024

{- | A single DRP transaction requested by software. Writing this register (see
'Bittide.Transceiver.Wishbone') triggers the transaction.
-}
data DrpRequest = DrpRequest
  { channel :: Unsigned 8
  -- ^ Which GTH channel to address. Must be @< n@ (the number of channels).
  , address :: BitVector DrpAddrWidth
  -- ^ DRP register address.
  , writeData :: BitVector DrpDataWidth
  -- ^ Data to write (ignored for reads).
  , isWrite :: Bool
  -- ^ 'True' for a write, 'False' for a read.
  }
  deriving (Generic, NFDataX, BitPackC, Eq, Show)

deriveTypeDescription ''DrpRequest

-- | Result of a completed DRP transaction, latched internally by the master.
data Response = Response
  { readData :: BitVector DrpDataWidth
  -- ^ Data read back (valid after a read; zero after a write).
  , timedOut :: Bool
  -- ^ 'True' if @drprdy@ was not seen within 'DrpTimeout' cycles.
  }
  deriving (Generic, NFDataX, Eq, Show)

{- | Software-readable status of the DRP master. Software polls 'busy'; once it
is 'False' the last transaction has completed and 'readData' / 'timedOut' are
valid.
-}
data DrpStatus = DrpStatus
  { busy :: Bool
  -- ^ 'True' while a transaction is in flight.
  , timedOut :: Bool
  -- ^ Result of the last completed transaction (see 'Response').
  , readData :: BitVector DrpDataWidth
  -- ^ Result of the last completed read (see 'Response').
  }
  deriving (Generic, NFDataX, BitPackC, Eq, Show)

deriveTypeDescription ''DrpStatus

-- | Internal FSM state of 'drpMaster'.
data DrpState
  = -- | No transaction in flight.
    Idle
  | -- | Enable pulse issued; waiting for @drprdy@ with a watchdog counter.
    Waiting DrpRequest (Index DrpTimeout)
  | -- | Emit the response for one cycle, then return to 'Idle'.
    Finishing Response
  deriving (Generic, NFDataX)

{- | DRP master. Accepts one request at a time (a 'Just' on the request input
starts a transaction; it is ignored unless the master is idle) and drives the
per-channel DRP buses of the GTH channels.

The request input is intended to be driven by the write-activity of a Wishbone
register, so it is asserted for exactly one cycle per software write.
-}
drpMaster ::
  forall n dom.
  (KnownDomain dom, KnownNat n, 1 <= n) =>
  Clock dom ->
  Reset dom ->
  Enable dom ->
  -- | Start a transaction ('Just' for one cycle). Ignored unless idle.
  Signal dom (Maybe DrpRequest) ->
  -- | Per-channel @drpdo_out@ from the GTH channels.
  Vec n (Signal dom (BitVector DrpDataWidth)) ->
  -- | Per-channel @drprdy_out@ from the GTH channels.
  Vec n (Signal dom (BitVector 1)) ->
  {- | @(drpaddr, drpdi, drpen, drpwe, response, busy)@. The first four fan out
  to the GTH channels' DRP inputs; @response@ strobes 'Just' for one cycle
  when a transaction completes; @busy@ is high while one is in flight.
  -}
  ( Vec n (Signal dom (BitVector DrpAddrWidth))
  , Vec n (Signal dom (BitVector DrpDataWidth))
  , Vec n (Signal dom (BitVector 1))
  , Vec n (Signal dom (BitVector 1))
  , Signal dom (Maybe Response)
  , Signal dom Bool
  )
drpMaster clk rst ena reqM drpDos drpRdys =
  ( unbundle addrs
  , unbundle dis
  , unbundle ens
  , unbundle wes
  , respM
  , busy
  )
 where
  (addrs, dis, ens, wes, respM, busy) =
    mealyB clk rst ena go Idle (reqM, bundle drpDos, bundle drpRdys)

  -- All-deasserted drive (no channel selected).
  idleDrive = (repeat 0, repeat 0, repeat 0, repeat 0)

  -- Drive @address@/@writeData@ on the requested channel. When @enable@ is set,
  -- also pulse @drpen@ (and @drpwe@ for a write) on that channel.
  activeDrive r enable =
    ( replace r.channel r.address (repeat 0)
    , replace r.channel r.writeData (repeat 0)
    , replace r.channel (if enable then 1 else 0) (repeat 0)
    , replace r.channel (if enable && r.isWrite then 1 else 0) (repeat 0)
    )

  go ::
    DrpState ->
    (Maybe DrpRequest, Vec n (BitVector DrpDataWidth), Vec n (BitVector 1)) ->
    ( DrpState
    , ( Vec n (BitVector DrpAddrWidth)
      , Vec n (BitVector DrpDataWidth)
      , Vec n (BitVector 1)
      , Vec n (BitVector 1)
      , Maybe Response
      , Bool
      )
    )
  go Idle (reqIn, _, _) = case reqIn of
    Just r -> (Waiting r 0, out (activeDrive r True) Nothing True)
    Nothing -> (Idle, out idleDrive Nothing False)
  go (Waiting r timer) (_, doV, rdyV)
    | (rdyV !! r.channel) == 1 =
        (Finishing (Response{readData = doV !! r.channel, timedOut = False}), waitOut)
    | timer == maxBound =
        (Finishing (Response{readData = 0, timedOut = True}), waitOut)
    | otherwise =
        (Waiting r (timer + 1), waitOut)
   where
    -- Hold address/data, deassert enable (the pulse was a single cycle).
    waitOut = out (activeDrive r False) Nothing True
  go (Finishing resp) _ = (Idle, out idleDrive (Just resp) True)

  out (a, d, e, w) resp busyOut = (a, d, e, w, resp, busyOut)
