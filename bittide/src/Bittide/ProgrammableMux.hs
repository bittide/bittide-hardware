-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.ProgrammableMux (programmableMux) where

import Clash.Prelude

import Bittide.ElasticBuffer (sticky)
import Bittide.SharedTypes (BitboneMm)
import Clash.Class.BitPackC (ByteOrder)
import GHC.Stack (HasCallStack)
import Protocols
import Protocols.MemoryMap (Access (ReadWrite, WriteOnly))
import Protocols.MemoryMap.Registers.WishboneStandard (
  RegisterConfig (access, description),
  deviceWb,
  registerConfig,
  registerWbI,
 )

import qualified Protocols.Vec as Vec

{- | The programmable mux allows the management unit to start with asynchronous communication,
but switch to synchronous communication at a specific clock cycle. On that cycle the business
logic will come out of reset and the data it writes is routed to to the elastic buffers.
Switching to business logic cannot be undone.
-}
programmableMux ::
  forall dom addrW linkCount.
  ( HasCallStack
  , HiddenClockResetEnable dom
  , KnownNat addrW
  , KnownNat linkCount
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
  -- | Local counter
  Signal dom (Unsigned 64) ->
  Circuit
    ( BitboneMm dom addrW
    , "MU_LINKS" ::: Vec linkCount (CSignal dom (BitVector 64))
    , "PE_LINKS" ::: Vec linkCount (CSignal dom (BitVector 64))
    )
    ( "PE_RESET" ::: CSignal dom Bool
    , "OUT_LINKS" ::: Vec linkCount (CSignal dom (BitVector 64))
    )
programmableMux localCounter = circuit $ \(bus, muLinks, peLinks) -> do
  [wbCycleToSwitch, wbArm] <- deviceWb "ProgrammableMux" -< bus

  let
    trigger = sticky hasClock hasReset $ arm .&&. (localCounter .==. cycleToSwitch)
    peReset = not <$> trigger
  Fwd linkOut <- repeatC (muxC trigger) <| Vec.zip -< (peLinks, muLinks)

  (Fwd cycleToSwitch, _cycleToSwitchActivity) <-
    registerWbI
      (registerConfig "cycle_to_switch")
        { access = ReadWrite
        , description = "Clock cycle to switch from MU to PE"
        }
      (maxBound :: Unsigned 64)
      -< (wbCycleToSwitch, Fwd (pure Nothing))

  (Fwd arm, _armActivity) <-
    registerWbI
      (registerConfig "arm")
        { access = WriteOnly
        , description = "Arm the mux to switch on cycle_to_switch. Prevents atomicity issues."
        }
      False
      -< (wbArm, Fwd (pure Nothing))

  idC -< Fwd (peReset, linkOut)
 where
  muxC :: Signal dom Bool -> Circuit (CSignal dom a, CSignal dom a) (CSignal dom a)
  muxC p = applyC (uncurry (mux p)) (const $ ((), ()))
