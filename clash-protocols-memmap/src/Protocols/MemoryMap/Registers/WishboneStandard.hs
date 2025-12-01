-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{- | Utilities for creating Wishbone devices and registers, while also creating
a memory map.
-}
module Protocols.MemoryMap.Registers.WishboneStandard (
  -- * Device creation
  deviceWb,
  deviceWithOffsetsWb,

  -- * Register creation (explicit clocks and resets)
  registerWb,
  registerWb_,
  registerWbDf,
  registerWithOffsetWb,
  registerWithOffsetWbDf,

  -- * Register creation (implicit clocks and resets)
  registerWbI,
  registerWbI_,
  registerWbDfI,
  registerWithOffsetWbI,
  registerWithOffsetWbDfI,

  -- * Supporting types and functions
  registerConfig,
  busActivityWrite,
  BusReadBehavior (..),
  BusActivity (..),
  DeviceConfig (..),
  RegisterConfig (..),
) where

import Clash.Explicit.Prelude
import Protocols

import Clash.Class.BitPackC (Bytes)
import Clash.Prelude (HiddenClock, HiddenReset, hasClock, hasReset)
import GHC.Stack (withFrozenCallStack)
import GHC.Stack.Types (HasCallStack)
import Protocols.MemoryMap (Mm)
import Protocols.MemoryMap.Registers.WishboneStandard.Internal
import Protocols.Wishbone (Wishbone, WishboneMode (Standard))

import qualified Protocols.Df as Df
import qualified Protocols.Vec as V

{- | Tie a bunch of registers together to form a device. Note that @aw@ must be
chosen such that it can hold the addresses of all registers, including their
maximum byte address.

Example usage:

> deviceExample clk rst = circuit $ \(mm, wb) -> do
>   [reg1, reg2, reg3] <- deviceWb "example" -< (mm, wb)
>
>   registerWb_ clk rst (registerConfig "float") (0.0 :: Float)     -< (reg1, Fwd noWrite)
>   registerWb_ clk rst (registerConfig "u16")   (0 :: Unsigned 16) -< (reg2, Fwd noWrite)
>   registerWb_ clk rst (registerConfig "bool")  (False :: Bool)    -< (reg3, Fwd noWrite)
>
>   idC
>  where
>   noWrite = pure Nothing
-}
deviceWb ::
  forall n wordSize aw dom.
  ( HasCallStack
  , KnownNat n
  , KnownNat wordSize
  , KnownNat aw
  ) =>
  -- | Device name
  String ->
  Circuit
    ( ToConstBwd Mm
    , Wishbone dom 'Standard aw (Bytes wordSize)
    )
    ( Vec n (RegisterWb dom aw wordSize)
    )
deviceWb deviceName = circuit $ \(mm, wb) -> do
  (offsets0, metas0, wbs) <-
    V.unzip3 <| withFrozenCallStack deviceWithOffsetsWb deviceName -< (mm, wb)
  (offsets1, metas1) <- genOffsets -< (offsets0, metas0)
  V.zip3 -< (offsets1, metas1, wbs)
 where
  -- Generate offsets based on the sizes of the registers. The first address will be
  -- at offset 0, the next at the size of the first register, etc.
  --
  -- XXX: Handle alignment requirements. For example, a u64 should be aligned to a
  --      8-byte boundary. Because every register starts at a multiple of the word
  --      size, it auto-aligns at least at a `wordSize`-byte boundary.
  genOffsets ::
    Circuit
      (Vec n (ToConstBwd (Offset aw)), Vec n (ToConstBwd (RegisterMeta aw)))
      (Vec n (ToConst (Offset aw)), Vec n (ToConstBwd (RegisterMeta aw)))
  genOffsets = Circuit go
   where
    go ::
      ((Vec n (), Vec n ()), (Vec n (), Vec n (RegisterMeta aw))) ->
      ((Vec n (BitVector aw), Vec n (RegisterMeta aw)), (Vec n (BitVector aw), Vec n ()))
    go (_, (_, metas)) = ((offsets, metas), (offsets, repeat ()))
     where
      sizes = fmap (.nWords) metas
      offsets = snd $ mapAccumL (\acc size -> (acc + size, resize acc)) 0 sizes

-- | Like 'registerWbDf', but returns a 'CSignal' of 'Maybe' instead of a 'Df' stream.
registerWb ::
  forall a dom wordSize aw.
  (RegisterWbConstraints a dom wordSize aw) =>
  Clock dom ->
  Reset dom ->
  -- | Configuration values
  RegisterConfig ->
  -- | Reset value
  a ->
  Circuit
    ( RegisterWb dom aw wordSize
    , CSignal dom (Maybe a)
    )
    ( CSignal dom a
    , CSignal dom (Maybe (BusActivity a))
    )
registerWb clk rst regConfig resetValue = circuit $ \i -> do
  (regValue, busActivityDf) <- registerWbDf clk rst regConfig resetValue -< i
  busActivity <- Df.toMaybe -< busActivityDf
  idC -< (regValue, busActivity)

-- | Like 'registerWbDf', but does not return the register value.
registerWb_ ::
  forall a dom wordSize aw.
  (RegisterWbConstraints a dom wordSize aw) =>
  Clock dom ->
  Reset dom ->
  -- | Configuration values
  RegisterConfig ->
  -- | Reset value
  a ->
  Circuit
    ( RegisterWb dom aw wordSize
    , CSignal dom (Maybe a)
    )
    ()
registerWb_ clk rst regConfig resetValue = circuit $ \i -> do
  _ignored <- registerWbDf clk rst regConfig resetValue -< i
  idC

{- | Same as 'registerWb', but also takes an offset. You can tie registers
created using this function together with 'deviceWithOffsetsWb'.
-}
registerWithOffsetWb ::
  forall a dom wordSize aw.
  (RegisterWbConstraints a dom wordSize aw) =>
  Clock dom ->
  Reset dom ->
  -- | Configuration values
  RegisterConfig ->
  -- | Offset
  BitVector aw ->
  -- | Reset value
  a ->
  Circuit
    ( RegisterWithOffsetWb dom aw wordSize
    , CSignal dom (Maybe a)
    )
    ( CSignal dom a
    , CSignal dom (Maybe (BusActivity a))
    )
registerWithOffsetWb clk rst regConfig offset resetValue = circuit $ \i -> do
  (regValue, busActivityDf) <-
    registerWithOffsetWbDf clk rst regConfig offset resetValue -< i
  busActivity <- Df.toMaybe -< busActivityDf
  idC -< (regValue, busActivity)

{- | Same as 'registerWbDf', but also takes an offset. You can tie registers
created using this function together with 'deviceWithOffsetsWb'.
-}
registerWithOffsetWbDf ::
  forall a dom wordSize aw.
  (RegisterWbConstraints a dom wordSize aw) =>
  Clock dom ->
  Reset dom ->
  -- | Configuration values
  RegisterConfig ->
  -- | Offset
  BitVector aw ->
  -- | Reset value
  a ->
  Circuit
    ( RegisterWithOffsetWb dom aw wordSize
    , CSignal dom (Maybe a)
    )
    ( CSignal dom a
    , Df dom (BusActivity a)
    )
registerWithOffsetWbDf clk rst regConfig offset resetValue =
  circuit $ \((offsetBwd, meta, wb), maybeA) -> do
    genOffset -< offsetBwd
    registerWbDf clk rst regConfig resetValue -< ((Fwd offset, meta, wb), maybeA)
 where
  genOffset :: Circuit (ToConstBwd (BitVector aw)) ()
  genOffset = Circuit $ \_ -> (offset, ())

-- | 'registerWb' with a hidden clock and reset
registerWbI ::
  forall a dom wordSize aw.
  ( HiddenClock dom
  , HiddenReset dom
  , RegisterWbConstraints a dom wordSize aw
  ) =>
  -- | Configuration values
  RegisterConfig ->
  -- | Reset value
  a ->
  Circuit
    ( RegisterWb dom aw wordSize
    , CSignal dom (Maybe a)
    )
    ( CSignal dom a
    , CSignal dom (Maybe (BusActivity a))
    )
registerWbI = withFrozenCallStack $ registerWb hasClock hasReset

-- | 'registerWbDf' with a hidden clock and reset
registerWbDfI ::
  forall a dom wordSize aw.
  ( HiddenClock dom
  , HiddenReset dom
  , RegisterWbConstraints a dom wordSize aw
  ) =>
  -- | Configuration values
  RegisterConfig ->
  -- | Reset value
  a ->
  Circuit
    ( RegisterWb dom aw wordSize
    , CSignal dom (Maybe a)
    )
    ( CSignal dom a
    , Df dom (BusActivity a)
    )
registerWbDfI = withFrozenCallStack $ registerWbDf hasClock hasReset

-- | 'registerWbDf_' with a hidden clock and reset
registerWbI_ ::
  forall a dom wordSize aw.
  ( HiddenClock dom
  , HiddenReset dom
  , RegisterWbConstraints a dom wordSize aw
  ) =>
  -- | Configuration values
  RegisterConfig ->
  -- | Reset value
  a ->
  Circuit
    ( RegisterWb dom aw wordSize
    , CSignal dom (Maybe a)
    )
    ()
registerWbI_ = withFrozenCallStack $ registerWb_ hasClock hasReset

-- | 'registerWithOffsetWb' with a hidden clock and reset
registerWithOffsetWbI ::
  forall a dom wordSize aw.
  ( HiddenClock dom
  , HiddenReset dom
  , RegisterWbConstraints a dom wordSize aw
  ) =>
  -- | Configuration values
  RegisterConfig ->
  -- | Offset
  BitVector aw ->
  -- | Reset value
  a ->
  Circuit
    ( RegisterWithOffsetWb dom aw wordSize
    , CSignal dom (Maybe a)
    )
    ( CSignal dom a
    , CSignal dom (Maybe (BusActivity a))
    )
registerWithOffsetWbI = withFrozenCallStack $ registerWithOffsetWb hasClock hasReset

-- | 'registerWithOffsetWbDf' with a hidden clock and reset
registerWithOffsetWbDfI ::
  forall a dom wordSize aw.
  ( HiddenClock dom
  , HiddenReset dom
  , RegisterWbConstraints a dom wordSize aw
  ) =>
  -- | Configuration values
  RegisterConfig ->
  -- | Offset
  BitVector aw ->
  -- | Reset value
  a ->
  Circuit
    ( RegisterWithOffsetWb dom aw wordSize
    , CSignal dom (Maybe a)
    )
    ( CSignal dom a
    , Df dom (BusActivity a)
    )
registerWithOffsetWbDfI = withFrozenCallStack $ registerWithOffsetWbDf hasClock hasReset
