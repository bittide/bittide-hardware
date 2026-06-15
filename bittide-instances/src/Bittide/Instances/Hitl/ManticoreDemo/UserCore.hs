-- SPDX-FileCopyrightText: 2026 QBayLogic
--
-- SPDX-License-Identifier: Apache-2.0

{- | User core for the Manticore demo (milestone 1: a single Manticore chip on
one FPGA, seams tied off). It instantiates the 'manticoreBittideChip' blackbox
and exposes the chip's host interface — host registers, @start@, the device
registers, and a memory-mapped gmem window (program-image load + trace
readback) — as Wishbone devices on the management unit's bus, so the MU CPU
drives the whole boot/run/trace flow over GDB (replacing the KCU105 JTAG-to-AXI
host).

The gmem window is an 'addressableBytesWb' region whose read/write requests are
bridged ('gmemReqRespBridge') onto the chip's raw gmem host BRAM port. Because
it is an addressable memory region (not a register), GDB can bulk-transfer the
whole program image in one block write — and bulk-read the trace — instead of
one round-trip per 16-bit word.

The chip does not touch the Bittide links here: the handshake TX is forwarded
verbatim to the GTH (as in the soft-UGN demo). Seam wiring is a later
milestone.

Wishbone layout (two MU busses):

  * @ManticoreControl@: schedule_config, gmem_base, trace_base (RW),
    start (RW, write-strobe), and the device registers + done/idle/clock_active
    (RO).
  * @ManticoreGmem@: the gmem memory region (RW), 32-bit words.
-}
module Bittide.Instances.Hitl.ManticoreDemo.UserCore (
  UserCoreBusses,
  RingBufferDepth,
  ringBufferDepth,
  mkUserCore,
) where

import Clash.Explicit.Prelude
import Clash.Prelude (withClockResetEnable)
import Protocols

import qualified Clash.Prelude as CP

import Bittide.Instances.Domains (Bittide)
import Bittide.Instances.Hitl.GenericDemo.BringUp (NmuRemBusWidth, UserCoreCircuit)
import Bittide.Instances.Hitl.ManticoreDemo.Chip (
  GmemHostWords,
  ManticoreChipIn (..),
  ManticoreChipOut (..),
  ManticoreDeviceRegisters (..),
  ManticoreHostRegisters (..),
  manticoreBittideChip,
 )

import Bittide.Instances.Hitl.Setup (LinkCount)
import Bittide.SharedTypes (Bitbone)
import Clash.Class.BitPackC (ByteOrder)
import Clash.Prelude (HiddenClockResetEnable)
import Data.Maybe (isJust)
import GHC.Stack (HasCallStack)
import Protocols.MemoryMap (Access (ReadOnly, ReadWrite), Mm)
import Protocols.MemoryMap.Registers.WishboneStandard (
  RegisterConfig (access),
  addressableBytesWb,
  busActivityWrite,
  deviceConfig,
  deviceWbI,
  registerConfig,
  registerWbI,
  registerWbI_,
 )
import Protocols.ReqResp (ReqResp)

-- | Two MU busses: control + device registers, and the gmem memory region.
type UserCoreBusses = 2

{- | Ring-buffer depth of the (unused, milestone-1) link handshake path. Matches
the soft-UGN demo's value; the Manticore chip itself does not use it yet.
-}
type RingBufferDepth = 4000

ringBufferDepth :: SNat RingBufferDepth
ringBufferDepth = SNat

boolToBv32 :: Bool -> BitVector 32
boolToBv32 b = if b then 1 else 0

{- | Phase of the command-complete handshake (see 'cmdComplete' in the user
core). @PhDone@ means the last-issued command has genuinely completed.
-}
data CmdPhase = PhDone | PhWaitBusy | PhWaitDone
  deriving (Generic, NFDataX, Eq)

{- | Transition for the command-complete FSM, driven by
@(startPulse, chipStopped)@ where @chipStopped = done || idle@:

  * @PhDone@      — idle/complete; a @start@ pulse arms the FSM.
  * @PhWaitBusy@  — wait for the chip to actually go busy (@chipStopped@ low),
                    so the stale completion of the previous command is ignored.
  * @PhWaitDone@  — wait for the chip to report stopped again (this command's
                    completion), then return to @PhDone@.

The output @(== PhDone)@ therefore only reads True once the command issued by
the latest @start@ has run to completion.
-}
cmdPhaseStep :: CmdPhase -> (Bool, Bool) -> CmdPhase
cmdPhaseStep ph (startPulse, chipStopped) = case ph of
  PhDone
    | startPulse -> PhWaitBusy
    | otherwise -> PhDone
  PhWaitBusy
    | startPulse -> PhWaitBusy
    | not chipStopped -> PhWaitDone
    | otherwise -> PhWaitBusy
  PhWaitDone
    | chipStopped -> PhDone
    | otherwise -> PhWaitDone

mkUserCore :: UserCoreCircuit UserCoreBusses (NmuRemBusWidth UserCoreBusses)
mkUserCore bitClk bitRst bitEna _localCounter _maybeDna _appReset =
  manticoreUserCoreC bitClk bitRst bitEna

{- | The user-core circuit. Carries 'HasCallStack' so the Wishbone register
helpers ('registerWbI') can record source locations for the memory map (they
error without a call-stack frame); 'mkUserCore' calls it, supplying that frame.
-}
manticoreUserCoreC ::
  (HasCallStack, ?byteOrder :: ByteOrder) =>
  Clock Bittide ->
  Reset Bittide ->
  Enable Bittide ->
  Circuit
    ( Vec UserCoreBusses (ToConstBwd Mm, Bitbone Bittide (NmuRemBusWidth UserCoreBusses))
    , "RXS2_RAW" ::: CSignal Bittide (Vec LinkCount (BitVector 64))
    , "HANDSHAKE_OUT" ::: CSignal Bittide (Vec LinkCount (BitVector 64))
    )
    ("GTH_TX" ::: CSignal Bittide (Vec LinkCount (BitVector 64)))
manticoreUserCoreC bitClk bitRst bitEna =
  circuit $ \(userCoreBusses, _rxs2Raw, handshakeOut) -> do
    [controlBus, gmemBus] <- idC -< userCoreBusses

    -- ---- control + device registers ----
    [ wbSched
      , wbGmem
      , wbTrace
      , wbStart
      , wbVc
      , wbEid
      , wbEc
      , wbTh
      , wbBc
      , wbDi
      , wbCs
      , wbDone
      , wbIdle
      , wbCa
      ] <-
      withCRE (deviceWbI (deviceConfig "ManticoreControl")) -< controlBus

    (Fwd (schedV, _)) <-
      withCRE (registerWbI (rw "schedule_config") (0 :: BitVector 64)) -< (wbSched, Fwd (pure Nothing))
    (Fwd (gmemV, _)) <-
      withCRE (registerWbI (rw "gmem_base") (0 :: BitVector 64)) -< (wbGmem, Fwd (pure Nothing))
    (Fwd (traceV, _)) <-
      withCRE (registerWbI (rw "trace_base") (0 :: BitVector 64)) -< (wbTrace, Fwd (pure Nothing))
    (Fwd (_, startAct)) <-
      withCRE (registerWbI (rw "start") (0 :: BitVector 32)) -< (wbStart, Fwd (pure Nothing))

    -- ---- memory-mapped gmem window ----
    -- 'addressableBytesWb' presents the chip-local gmem as an addressable
    -- 32-bit-word region on the MU bus; 'gmemReqRespBridge' turns its
    -- read/write requests into accesses on the chip's raw gmem host BRAM port
    -- (feeding back the chip's read data). GDB block-writes the program image
    -- here and block-reads the trace.
    [wbGmemWin] <- withCRE (deviceWbI (deviceConfig "ManticoreGmem")) -< gmemBus
    reqResp <- withCRE (addressableBytesWb @GmemHostWords (rw "data")) -< wbGmemWin
    Fwd gmemDrive <- withCRE (gmemReqRespBridge (chipOut.gmemDout)) -< reqResp

    let
      (gmemEnS, gmemWeS, gmemAddrS, gmemDinS) = unbundle gmemDrive
      hostRegs = ManticoreHostRegisters <$> schedV <*> gmemV <*> traceV
      startPulse = isJust . busActivityWrite <$> startAct

      chipOut =
        manticoreBittideChip
          bitClk
          bitRst
          ManticoreChipIn
            { hostRegs
            , start = startPulse
            , gmemEn = gmemEnS
            , gmemWe = gmemWeS
            , gmemAddr = gmemAddrS
            , gmemDin = gmemDinS
            }

      devRegs = chipOut.deviceRegs

      -- Clean command-complete handshake. The chip's raw @done@/@idle@ are
      -- level signals that stay asserted from the PREVIOUS command, and the
      -- host polls over GDB at ~ms granularity — far slower than the chip's
      -- busy window (the program runs between @$display@s in tens of µs). So a
      -- host polling raw @done@/@idle@ right after poking @start@ samples the
      -- stale completion and "advances" without the command running (observed
      -- as the first trace record captured many times). Track the lifecycle in
      -- hardware instead: 'cmdPhaseStep' arms on the start pulse, waits for the
      -- chip to go busy (@done@/@idle@ both deassert), and only then reports
      -- complete — so a True read always reflects THIS command.
      chipStopped = (||) <$> chipOut.done <*> chipOut.idle
      cmdComplete =
        withCRE (CP.moore cmdPhaseStep (== PhDone) PhDone) (bundle (startPulse, chipStopped))

    -- ---- device registers read back by the MU (read-only) ----
    withCRE (registerWbI_ (ro "virtual_cycles") (0 :: BitVector 64))
      -< (wbVc, Fwd (Just . (.virtualCycles) <$> devRegs))
    withCRE (registerWbI_ (ro "exception_id") (0 :: BitVector 32))
      -< (wbEid, Fwd (Just . (.exceptionId) <$> devRegs))
    withCRE (registerWbI_ (ro "execution_cycles") (0 :: BitVector 64))
      -< (wbEc, Fwd (Just . (.executionCycles) <$> devRegs))
    withCRE (registerWbI_ (ro "trace_dump_head") (0 :: BitVector 64))
      -< (wbTh, Fwd (Just . (.traceDumpHead) <$> devRegs))
    withCRE (registerWbI_ (ro "bootloader_cycles") (0 :: BitVector 32))
      -< (wbBc, Fwd (Just . (.bootloaderCycles) <$> devRegs))
    withCRE (registerWbI_ (ro "device_info") (0 :: BitVector 32))
      -< (wbDi, Fwd (Just . (.deviceInfo) <$> devRegs))
    withCRE (registerWbI_ (ro "clock_stalls") (0 :: BitVector 64))
      -< (wbCs, Fwd (Just . (.clockStalls) <$> devRegs))
    withCRE (registerWbI_ (ro "done") (0 :: BitVector 32))
      -< (wbDone, Fwd (Just . boolToBv32 <$> cmdComplete))
    withCRE (registerWbI_ (ro "idle") (0 :: BitVector 32))
      -< (wbIdle, Fwd (Just . boolToBv32 <$> chipOut.idle))
    withCRE (registerWbI_ (ro "clock_active") (0 :: BitVector 32))
      -< (wbCa, Fwd (Just . boolToBv32 <$> chipOut.clockActive))

    -- Milestone 1: forward the handshake TX verbatim; the chip does not drive
    -- the links yet.
    idC -< handshakeOut
 where
  withCRE :: forall r. ((HiddenClockResetEnable Bittide) => r) -> r
  withCRE = withClockResetEnable bitClk bitRst bitEna

  rw :: String -> RegisterConfig
  rw name = (registerConfig name){access = ReadWrite}

  ro :: String -> RegisterConfig
  ro name = (registerConfig name){access = ReadOnly}

{- | Bridge between an 'addressableBytesWb' region and the chip's raw gmem host
BRAM port.

'addressableBytesWb' converts each Wishbone access into a held request — read
(@Left idx@) or write (@Right (idx, byteMask, data)@) — and only acknowledges
once we return read data ('Just'). This drives the request onto the chip's gmem
host port and, after the BRAM read settles, returns the read word.

To break the long combinational path across the chip boundary (Wishbone ->
bridge -> BRAM -> bridge -> Wishbone), both the drive signals (address / data /
write-enables) and the read data are registered on the Clash side. The request
is held stable by the Wishbone master until acknowledged, so the access is
idempotent while in flight. Total latency request -> response is 3 cycles
(input register, 1-cycle BRAM read, output register), tracked by a small
saturating counter; the response is asserted for the single cycle the counter
is saturated, which acknowledges the Wishbone transfer and drops the request.
-}
gmemReqRespBridge ::
  forall dom nWords aw.
  ( HiddenClockResetEnable dom
  , KnownNat nWords
  , 1 <= nWords
  , aw ~ CLog 2 nWords
  , BitSize (Index nWords) ~ aw
  ) =>
  -- | Read data from the chip's gmem host port (combinational BRAM output).
  Signal dom (BitVector 32) ->
  Circuit
    ( ReqResp
        dom
        (Either (Index nWords) (Index nWords, BitVector 4, BitVector 32))
        (BitVector 32)
    )
    (CSignal dom (Bool, BitVector 4, BitVector aw, BitVector 32))
gmemReqRespBridge dout = Circuit go
 where
  go (req, _) = (resp, drive)
   where
    drive = CP.register (False, 0, 0, 0) (mkDrive <$> req)
    doutReg = CP.register 0 dout
    cnt = CP.register (0 :: Index 4) cntNext
    cntNext =
      mux (isJust <$> resp) (pure 0)
        $ mux (isJust <$> req) (satSucc SatBound <$> cnt) (pure 0)
    resp = mux ((== maxBound) <$> cnt) (Just <$> doutReg) (pure Nothing)

  mkDrive ::
    Maybe (Either (Index nWords) (Index nWords, BitVector 4, BitVector 32)) ->
    (Bool, BitVector 4, BitVector aw, BitVector 32)
  mkDrive Nothing = (False, 0, 0, 0)
  mkDrive (Just (Left idx)) = (True, 0, pack idx, 0)
  mkDrive (Just (Right (idx, mask, dat))) = (True, mask, pack idx, dat)
