-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{- |
A small Wishbone scratch peripheral holding the UGN grooming corrections that the
host computes and the management-unit CPU applies.

It exposes two read/write registers:

  * @corrections@ — a vector of @n@ signed correction values (initialised to all
    zeros).
  * @valid@ — a flag indicating the corrections are ready to be applied
    (initialised to 'False').

The intended flow (see "Bittide.ClockControl.Ugn.Grooming"):

  1. The CPU finishes its normal work and spins, polling @valid@ over MMIO.
  2. The host computes the corrections, halts the CPU via GDB, writes
     @corrections@, sets @valid@ to 'True', and resumes the CPU.
  3. On its next poll the CPU sees @valid == True@, reads @corrections@, and
     applies them.

Both registers are plain bus-accessible storage (no fabric write port): the CPU
and the host both reach them as Wishbone masters. Nothing reads them from the
fabric, so the peripheral has no outputs.
-}
module Bittide.ClockControl.Ugn.Corrections (correctionsWb) where

import Clash.Explicit.Prelude

import Bittide.SharedTypes (BitboneMm)
import GHC.Stack (HasCallStack)
import Protocols
import Protocols.MemoryMap (Access (ReadWrite))
import Protocols.MemoryMap.Registers.WishboneStandard (
  RegisterConfig (access),
  deviceConfig,
  deviceWbI,
  registerConfig,
  registerWbI_,
 )

import Clash.Class.BitPackC (ByteOrder)

import qualified Clash.Prelude as C

{- | UGN grooming corrections peripheral. Holds @n@ signed corrections and a
validity flag, all writable and readable over Wishbone. See the module header
for the intended host/CPU handshake.
-}
correctionsWb ::
  forall n dom addrW.
  ( HasCallStack
  , C.HiddenClockResetEnable dom
  , KnownNat addrW
  , ?byteOrder :: ByteOrder
  ) =>
  SNat n ->
  Circuit (BitboneMm dom addrW) ()
correctionsWb SNat = circuit $ \bus -> do
  [wbCorrections, wbValid] <- deviceWbI (deviceConfig "UgnCorrections") -< bus
  registerWbI_ correctionsConfig initCorrections -< (wbCorrections, Fwd noWrite)
  registerWbI_ validConfig False -< (wbValid, Fwd noWrite)
 where
  noWrite = pure Nothing
  initCorrections = C.repeat 0 :: Vec n (Signed 64)
  correctionsConfig = (registerConfig "corrections"){access = ReadWrite}
  validConfig = (registerConfig "valid"){access = ReadWrite}
