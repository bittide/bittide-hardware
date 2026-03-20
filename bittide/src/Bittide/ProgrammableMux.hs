-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.ProgrammableMux (programmableMux) where

import Clash.Prelude
import Protocols

import Bittide.ElasticBuffer (stickyE)
import Clash.Class.BitPackC (ByteOrder)
import GHC.Stack (HasCallStack)
import Protocols.MemoryMap (Access (ReadWrite, WriteOnly), Mm)
import Protocols.MemoryMap.Registers.WishboneStandard (
  RegisterConfig (access, description),
  deviceConfig,
  deviceWb,
  registerConfig,
  registerWbI,
 )
import Protocols.Wishbone (Wishbone, WishboneMode (Standard))

{- | A mux which switches its output from input 'A' to input 'B' when the local counter is
'first_b_cycle' and the device is armed. Once switched the mux cannot switched back
using the registers, but requires a reset.

An intended use for this component is in the 'WireDemo'. In this demo, the management unit
(input A) starts with control over the link, while the processing element (input B) is
held in reset. When the management unit has finished its setup, it sets the
'first_b_cycle' and arms the mux. At the specified cycle, the mux switches to the
processing element's links and deasserts its reset, allowing it to start writing to the
link.
-}
programmableMux ::
  forall dom addrW nBytes a.
  ( HasCallStack
  , HiddenClockResetEnable dom
  , KnownNat addrW
  , KnownNat nBytes
  , 1 <= nBytes
  , ?regByteOrder :: ByteOrder
  ) =>
  -- | Local counter
  Signal dom (Unsigned 64) ->
  Circuit
    ( (ToConstBwd Mm, Wishbone dom 'Standard addrW nBytes)
    , "A" ::: CSignal dom a
    , "B" ::: CSignal dom a
    )
    ( "B_RESET" ::: Reset dom
    , "OUT" ::: CSignal dom a
    )
programmableMux localCounter = circuit $ \(bus, a, b) -> do
  [wbFirstBCycle, wbArm] <- deviceWb (deviceConfig "ProgrammableMux") -< bus

  let
    -- Trigger one cycle earlier to account for the delay from 'sticky'
    trigger = stickyE hasClock hasReset $ arm .&&. (localCounter .==. (firstBCycle - 1))
    bReset = unsafeFromActiveLow trigger
  Fwd linkOut <- muxC trigger -< (b, a)

  (Fwd firstBCycle, _firstBCycleActivity) <-
    registerWbI
      (registerConfig "first_b_cycle")
        { access = ReadWrite
        , description = "Clock cycle to switch from input 'A' to 'B'."
        }
      maxBound
      -< (wbFirstBCycle, Fwd (pure Nothing))

  (Fwd arm, _armActivity) <-
    registerWbI
      (registerConfig "arm")
        { access = WriteOnly
        , description =
            "Arm the mux to switch on 'first_b_cycle'. Prevents atomicity issues when writing to 'first_b_cycle'."
        }
      False
      -< (wbArm, Fwd (pure Nothing))

  idC -< Fwd (bReset, linkOut)
 where
  muxC :: Signal dom Bool -> Circuit (CSignal dom a, CSignal dom a) (CSignal dom a)
  muxC bool = Circuit go
   where
    go ((t, f), _) = (units, mux bool t f)
