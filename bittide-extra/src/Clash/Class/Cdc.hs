-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{- | This module introduces typeclasses, instances, and some helper functions + type aliases for
writing components that require CDC crossings, but it may not be desirable to use a specific CDC
crossing. This enables the designer to write functions that are general over the many parameters
of CDC crossings as well, including what type the input signal contains, the particular clock
domains, and the number of stages in the CDC primitive. The goal of this is then that the
designer is able to write another file with orphaned instances containing the primitives for a
given vendor, which may then be used to specialize more general components.

To be more concrete, a designer may want to be able to write a component that requires a gray CDC
primitive and synthesize for both Xilinx and Intel FPGAs. In this case, the designer can write that
component with the constraint @Gray vendor InnerType SrcDomain DstDomain@ and provide the vendor
later through the 'withVendor' or 'withVendorI' functions.

N.B.: this module is written with qualified use in mind. Generally it should only be imported as
@import qualified Clash.Class.Cdc as Cdc@ or similar.
-}
module Clash.Class.Cdc where

import Clash.Prelude hiding (Bit, isRising, regEn)

import Bittide.Extra.Maybe (toMaybe)
import Clash.Explicit.Prelude (isRising, noReset, regEn)
import Data.Data (Proxy (Proxy))
import Data.Maybe (fromMaybe, isJust, isNothing)
import GHC.Stack (HasCallStack)

{- | Guarantees the existence of a CDC primitive for a fixed-width array of independent bits for a
given vendor
-}
class IndependentBits (vendor :: Symbol) where
  -- | Additional constraints on the independent bits CDC primitive with a custom configuration
  type
    IndependentBitsWithConstraints vendor (stages :: Nat) (a :: Type) (src :: Domain) (dst :: Domain) ::
      Constraint

  -- | Configuration type for an independent bits CDC primitive
  type IndependentBitsConfig vendor (stages :: Nat) (a :: Type) (src :: Domain) (dst :: Domain) :: Type

  -- | Independent bits CDC primitive with a custom configuration
  independentBitsWith ::
    forall stages a src dst.
    ( KnownDomain src
    , KnownDomain dst
    , HasCallStack
    , NFDataX a
    , BitPack a
    , IndependentBitsWithConstraints vendor stages a src dst
    ) =>
    IndependentBitsConfig vendor stages a src dst ->
    Clock src ->
    Clock dst ->
    Signal src a ->
    Signal dst a

  -- | Additional constraints on the independent bits CDC primitive with the default configuration
  type IndependentBitsConstraints vendor (a :: Type) (src :: Domain) (dst :: Domain) :: Constraint

  -- | Default number of stages in an independent bits CDC primitive
  type IndependentBitsDefaultStages vendor (a :: Type) (src :: Domain) (dst :: Domain) :: Nat

  -- | Default configuration for an independent bits CDC primitive
  defaultIndependentBitsConfig ::
    forall a src dst.
    (KnownDomain src, KnownDomain dst) =>
    Proxy src ->
    Proxy dst ->
    Proxy a ->
    IndependentBitsConfig vendor (IndependentBitsDefaultStages vendor a src dst) a src dst

  -- | Independent bits CDC primitive with the default configuration
  independentBits ::
    forall a src dst.
    ( KnownDomain src
    , KnownDomain dst
    , HasCallStack
    , NFDataX a
    , BitPack a
    , IndependentBitsConstraints vendor a src dst
    ) =>
    Clock src ->
    Clock dst ->
    Signal src a ->
    Signal dst a
  default independentBits ::
    ( KnownDomain src
    , KnownDomain dst
    , NFDataX a
    , BitPack a
    , IndependentBitsWithConstraints vendor (IndependentBitsDefaultStages vendor a src dst) a src dst
    ) =>
    Clock src ->
    Clock dst ->
    Signal src a ->
    Signal dst a
  independentBits (srcClk :: Clock src) (dstClk :: Clock dst) (input :: Signal src a) =
    independentBitsWith @vendor @(IndependentBitsDefaultStages vendor a src dst) @a @src @dst
      (defaultIndependentBitsConfig @vendor @a @src @dst Proxy Proxy Proxy)
      srcClk
      dstClk
      input

-- | Helper typeclass for bringing in the primitive typeclass and additional constraints
class
  (IndependentBits vendor, IndependentBitsConstraints vendor a src dst) =>
  ValidIndependentBits (vendor :: Symbol) (a :: Type) (src :: Domain) (dst :: Domain)

instance
  (IndependentBits vendor, IndependentBitsConstraints vendor a src dst) =>
  ValidIndependentBits (vendor :: Symbol) (a :: Type) (src :: Domain) (dst :: Domain)

-- | Helper typeclass for bringing in the primitive typeclass and additional constraints
class
  (IndependentBits vendor, IndependentBitsWithConstraints vendor stages a src dst) =>
  ValidIndependentBitsWith
    (vendor :: Symbol)
    (stages :: Nat)
    (a :: Type)
    (src :: Domain)
    (dst :: Domain)

instance
  (IndependentBits vendor, IndependentBitsWithConstraints vendor stages a src dst) =>
  ValidIndependentBitsWith
    (vendor :: Symbol)
    (stages :: Nat)
    (a :: Type)
    (src :: Domain)
    (dst :: Domain)

-- | Guarantees the existence of a gray CDC primitive for a given vendor
class Gray (vendor :: Symbol) where
  -- | Additional constraints on the gray CDC primtive with a custom configuration
  type GrayWithConstraints vendor (stages :: Nat) (n :: Nat) (src :: Domain) (dst :: Domain) :: Constraint

  -- | Configuration type for a gray CDC primitive
  type GrayConfig vendor (stages :: Nat) (n :: Nat) (src :: Domain) (dst :: Domain) :: Type

  -- | Gray CDC primitive with a custom configuration
  grayWith ::
    forall stages n src dst.
    ( KnownDomain src
    , KnownDomain dst
    , HasCallStack
    , KnownNat n
    , GrayWithConstraints vendor stages n src dst
    ) =>
    GrayConfig vendor stages n src dst ->
    Clock src ->
    Clock dst ->
    Signal src (Unsigned n) ->
    Signal dst (Unsigned n)

  -- | Additional constraints on the gray CDC primtive with the default configuration
  type GrayConstraints vendor (n :: Nat) (src :: Domain) (dst :: Domain) :: Constraint

  -- | Default number of stages in a gray CDC
  type GrayDefaultStages vendor (n :: Nat) (src :: Domain) (dst :: Domain) :: Nat

  -- | Default configuration for a gray CDC primitive
  defaultGrayConfig ::
    forall n src dst.
    (KnownDomain src, KnownDomain dst) =>
    Proxy src ->
    Proxy dst ->
    Proxy n ->
    GrayConfig vendor (GrayDefaultStages vendor n src dst) n src dst

  -- | Gray CDC primitive with the default configuration
  gray ::
    forall n src dst.
    ( KnownDomain src
    , KnownDomain dst
    , KnownNat n
    , HasCallStack
    , GrayConstraints vendor n src dst
    ) =>
    Clock src ->
    Clock dst ->
    Signal src (Unsigned n) ->
    Signal dst (Unsigned n)
  default gray ::
    ( KnownDomain src
    , KnownDomain dst
    , KnownNat n
    , HasCallStack
    , GrayWithConstraints vendor (GrayDefaultStages vendor n src dst) n src dst
    ) =>
    Clock src ->
    Clock dst ->
    Signal src (Unsigned n) ->
    Signal dst (Unsigned n)
  gray (srcClk :: Clock src) (dstClk :: Clock dst) (input :: Signal src (Unsigned n)) =
    grayWith @vendor @(GrayDefaultStages vendor n src dst) @n @src @dst
      (defaultGrayConfig @vendor @n @src @dst Proxy Proxy Proxy)
      srcClk
      dstClk
      input

-- | Helper typeclass for bringing in the primitive typeclass and additional constraints
class
  (Gray vendor, GrayConstraints vendor n src dst) =>
  ValidGray (vendor :: Symbol) (n :: Nat) (src :: Domain) (dst :: Symbol)

instance
  (Gray vendor, GrayConstraints vendor n src dst) =>
  ValidGray (vendor :: Symbol) (n :: Nat) (src :: Domain) (dst :: Symbol)

-- | Helper typeclass for bringing in the primitive typeclass and additional constraints
class
  (Gray vendor, GrayWithConstraints vendor stages n src dst) =>
  ValidGrayWith (vendor :: Symbol) (stages :: Nat) (n :: Nat) (src :: Domain) (dst :: Symbol)

instance
  (Gray vendor, GrayWithConstraints vendor stages n src dst) =>
  ValidGrayWith (vendor :: Symbol) (stages :: Nat) (n :: Nat) (src :: Domain) (dst :: Symbol)

-- | Guarantees the existence of a handshake CDC primitive for a given vendor
class Handshake (vendor :: Symbol) where
  -- | Additional constraints on the handshake CDC primitive with a custom configuration
  type
    HandshakeWithConstraints
      vendor
      (srcStages :: Nat)
      (dstStages :: Nat)
      (a :: Type)
      (src :: Domain)
      (dst :: Domain) ::
      Constraint

  -- | Configuration type for a handshake CDC primitive
  type
    HandshakeConfig
      vendor
      (srcStages :: Nat)
      (dstStages :: Nat)
      (a :: Type)
      (src :: Domain)
      (dst :: Domain) ::
      Type

  -- | Handshake CDC primitive with a custom configuration
  handshakeWith ::
    forall srcStages dstStages a src dst.
    ( KnownDomain src
    , KnownDomain dst
    , HasCallStack
    , BitPack a
    , NFDataX a
    , HandshakeWithConstraints vendor srcStages dstStages a src dst
    ) =>
    HandshakeConfig vendor srcStages dstStages a src dst ->
    Clock src ->
    Clock dst ->
    {- | Word to synchronize to destination domain. This value should not change when @src_send@ is
    asserted.
    -}
    "src_in" ::: Signal src a ->
    {- | Assertion of this signal allows the @src_in@ bus to be synchronized to the destination
    clock domain. This signal should only be asserted when @src_rcv@ is deasserted, indicating
    that the previous data transfer is complete. This signal should only be deasserted once
    @src_rcv@ is asserted, acknowledging that the @src_in@ has been received by the destination
    logic.
    -}
    "src_send" ::: Signal src Bool ->
    -- Destination ack receive signal. Asserting this signal indicates that data on the output
    -- signal has been captured by the destination logic. This signal should be deasserted once the
    -- destination request signal is deasserted, completing the handshake on the destination clock
    -- domain and indicating that the destination logic is ready for a new transfer.
    "dst_ack" ::: Signal dst Bool ->
    {- | @dest_req@ indicates that @dest_out@ contains valid data. It can be acknowledged by
    asserting @dst_ack@. @src_rcv@ indicates that the destination domain has acknowledged a data
    transfer.
    -}
    ( "dest_out" ::: Signal dst a
    , "dest_req" ::: Signal dst Bool
    , "src_rcv" ::: Signal src Bool
    )

  -- | Additional constraints on the handshake CDC primitive with the default configuration
  type HandshakeConstraints vendor (a :: Type) (src :: Domain) (dst :: Domain) :: Constraint

  -- | Default number of stages in the source domain in a handshake CDC primitive
  type HandshakeDefaultSrcStages vendor (a :: Type) (src :: Domain) (dst :: Domain) :: Nat

  -- | Default number of stages in the destination domain in a handshake CDC primitive
  type HandshakeDefaultDstStages vendor (a :: Type) (src :: Domain) (dst :: Domain) :: Nat

  -- | Default configuration for a handshake CDC primitive
  defaultHandshakeConfig ::
    forall a src dst.
    (KnownDomain src, KnownDomain dst) =>
    Proxy src ->
    Proxy dst ->
    Proxy a ->
    HandshakeConfig
      vendor
      (HandshakeDefaultSrcStages vendor a src dst)
      (HandshakeDefaultDstStages vendor a src dst)
      a
      src
      dst

  -- | Handshake CDC primitive with the default configuration
  handshake ::
    forall a src dst.
    ( KnownDomain src
    , KnownDomain dst
    , BitPack a
    , NFDataX a
    , HasCallStack
    , HandshakeConstraints vendor a src dst
    ) =>
    Clock src ->
    Clock dst ->
    Signal src a ->
    Signal src Bool ->
    Signal dst Bool ->
    (Signal dst a, Signal dst Bool, Signal src Bool)
  default handshake ::
    ( KnownDomain src
    , KnownDomain dst
    , BitPack a
    , NFDataX a
    , HasCallStack
    , HandshakeWithConstraints
        vendor
        (HandshakeDefaultSrcStages vendor a src dst)
        (HandshakeDefaultDstStages vendor a src dst)
        a
        src
        dst
    ) =>
    Clock src ->
    Clock dst ->
    Signal src a ->
    Signal src Bool ->
    Signal dst Bool ->
    ( Signal dst a
    , Signal dst Bool
    , Signal src Bool
    )
  handshake
    (srcClk :: Clock src)
    (dstClk :: Clock dst)
    (input :: Signal src a)
    (srcSend :: Signal src Bool)
    (dstAck :: Signal dst Bool) =
      handshakeWith
        @vendor
        @(HandshakeDefaultSrcStages vendor a src dst)
        @(HandshakeDefaultDstStages vendor a src dst)
        @a
        @src
        @dst
        (defaultHandshakeConfig @vendor @a @src @dst Proxy Proxy Proxy)
        srcClk
        dstClk
        input
        srcSend
        dstAck

-- | Helper typeclass for bringing in the primitive typeclass and additional constraints
class
  (Handshake vendor, HandshakeConstraints vendor a src dst) =>
  ValidHandshake (vendor :: Symbol) (a :: Type) (src :: Domain) (dst :: Domain)

instance
  (Handshake vendor, HandshakeConstraints vendor a src dst) =>
  ValidHandshake (vendor :: Symbol) (a :: Type) (src :: Domain) (dst :: Domain)

-- | Helper typeclass for bringing in the primitive typeclass and additional constraints
class
  (Handshake vendor, HandshakeWithConstraints vendor srcStages dstStages a src dst) =>
  ValidHandshakeWith
    (vendor :: Symbol)
    (srcStages :: Nat)
    (dstStages :: Nat)
    (a :: Type)
    (src :: Domain)
    (dst :: Domain)

instance
  (Handshake vendor, HandshakeWithConstraints vendor srcStages dstStages a src dst) =>
  ValidHandshakeWith
    (vendor :: Symbol)
    (srcStages :: Nat)
    (dstStages :: Nat)
    (a :: Type)
    (src :: Domain)
    (dst :: Domain)

-- | Guarantees the existence of a pulse CDC primitive for a given vendor
class Pulse (vendor :: Symbol) where
  -- | Additional constraints on the pulse CDC primitive with a custom configuration
  type
    PulseWithConstraints vendor (stages :: Nat) (a :: Type) (src :: Domain) (dst :: Domain) ::
      Constraint

  -- | Configuration type for a pulse CDC primitive
  type PulseConfig vendor (stages :: Nat) (a :: Type) (src :: Domain) (dst :: Domain) :: Type

  -- | Pulse CDC primitive with a custom configuration
  pulseWith ::
    forall stages a src dst.
    ( KnownDomain src
    , KnownDomain dst
    , HasCallStack
    , Bits a
    , NFDataX a
    , BitPack a
    , PulseWithConstraints vendor stages a src dst
    ) =>
    PulseConfig vendor stages a src dst ->
    Clock src ->
    Reset src ->
    Clock dst ->
    Reset dst ->
    Signal src a ->
    Signal dst a

  -- | Additional constraints on the pulse CDC primitive with the default configuration
  type PulseConstraints vendor (a :: Type) (src :: Domain) (dst :: Domain) :: Constraint

  -- | Default number of stages in a pulse CDC primitive
  type PulseDefaultStages vendor (a :: Type) (src :: Domain) (dst :: Domain) :: Nat

  -- | Default configuration for a pulse CDC primitive
  defaultPulseConfig ::
    forall a src dst.
    (KnownDomain src, KnownDomain dst) =>
    Proxy src ->
    Proxy dst ->
    Proxy a ->
    PulseConfig vendor (PulseDefaultStages vendor a src dst) a src dst

  -- | Pulse CDC primitive with the default configuration
  pulse ::
    forall a src dst.
    ( KnownDomain src
    , KnownDomain dst
    , HasCallStack
    , Bits a
    , NFDataX a
    , BitPack a
    , PulseConstraints vendor a src dst
    ) =>
    Clock src ->
    Clock dst ->
    Signal src a ->
    Signal dst a
  default pulse ::
    ( KnownDomain src
    , KnownDomain dst
    , HasCallStack
    , Bits a
    , NFDataX a
    , BitPack a
    , PulseWithConstraints vendor (PulseDefaultStages vendor a src dst) a src dst
    ) =>
    Clock src ->
    Clock dst ->
    Signal src a ->
    Signal dst a
  pulse
    (srcClk :: Clock src)
    (dstClk :: Clock dst)
    (input :: Signal src a) =
      pulseWith
        @vendor
        @(PulseDefaultStages vendor a src dst)
        @a
        @src
        @dst
        (defaultPulseConfig @vendor @a @src @dst Proxy Proxy Proxy)
        srcClk
        (errorX "src: no reset")
        dstClk
        (errorX "dst: no reset")
        input

-- | Helper typeclass for bringing in the primitive typeclass and additional constraints
class
  (Pulse vendor, PulseConstraints vendor a src dst) =>
  ValidPulse (vendor :: Symbol) (a :: Type) (src :: Domain) (dst :: Domain)

instance
  (Pulse vendor, PulseConstraints vendor a src dst) =>
  ValidPulse (vendor :: Symbol) (a :: Type) (src :: Domain) (dst :: Domain)

-- | Helper typeclass for bringing in the primitive typeclass and additional constraints
class
  (Pulse vendor, PulseWithConstraints vendor stages a src dst) =>
  ValidPulseWith (vendor :: Symbol) (stages :: Nat) (a :: Type) (src :: Domain) (dst :: Domain)

instance
  (Pulse vendor, PulseWithConstraints vendor stages a src dst) =>
  ValidPulseWith (vendor :: Symbol) (stages :: Nat) (a :: Type) (src :: Domain) (dst :: Domain)

-- | Guarantees the existence of a single bit CDC primitive for a given vendor
class Bit (vendor :: Symbol) where
  -- | Additional constraints on the single bit CDC primitive with a custom configuration
  type BitWithConstraints vendor (stages :: Nat) (a :: Type) (src :: Domain) (dst :: Domain) :: Constraint

  -- | Configuration type for a single bit CDC primitive
  type BitConfig vendor (stages :: Nat) (a :: Type) (src :: Domain) (dst :: Domain) :: Type

  -- | Single bit CDC primitive with a custom configuration
  bitWith ::
    forall stages a src dst.
    ( KnownDomain src
    , KnownDomain dst
    , HasCallStack
    , NFDataX a
    , BitPack a
    , BitWithConstraints vendor stages a src dst
    ) =>
    BitConfig vendor stages a src dst ->
    Clock src ->
    Clock dst ->
    Signal src a ->
    Signal dst a

  -- | Additional constraints on the single bit CDC primitive with the default configuration
  type BitConstraints vendor (a :: Type) (src :: Domain) (dst :: Domain) :: Constraint

  -- | Default number of stages in a single bit CDC primitive
  type BitDefaultStages vendor (a :: Type) (src :: Domain) (dst :: Domain) :: Nat

  -- | Default configuration for a single bit CDC primitive
  defaultBitConfig ::
    forall a src dst.
    (KnownDomain src, KnownDomain dst) =>
    Proxy src ->
    Proxy dst ->
    Proxy a ->
    BitConfig vendor (BitDefaultStages vendor a src dst) a src dst

  -- | Single bit CDC primitive with the default configuration
  bit ::
    forall a src dst.
    ( KnownDomain src
    , KnownDomain dst
    , HasCallStack
    , NFDataX a
    , BitPack a
    , BitConstraints vendor a src dst
    ) =>
    Clock src ->
    Clock dst ->
    Signal src a ->
    Signal dst a
  default bit ::
    ( KnownDomain src
    , KnownDomain dst
    , NFDataX a
    , BitPack a
    , BitWithConstraints vendor (BitDefaultStages vendor a src dst) a src dst
    ) =>
    Clock src ->
    Clock dst ->
    Signal src a ->
    Signal dst a
  bit (srcClk :: Clock src) (dstClk :: Clock dst) (input :: Signal src a) =
    bitWith @vendor @(BitDefaultStages vendor a src dst) @a @src @dst
      (defaultBitConfig @vendor @a @src @dst Proxy Proxy Proxy)
      srcClk
      dstClk
      input

-- | Helper typeclass for bringing in the primitive typeclass and additional constraints
class
  (Bit vendor, BitConstraints vendor a src dst) =>
  ValidBit (vendor :: Symbol) (a :: Type) (src :: Domain) (dst :: Domain)

instance
  (Bit vendor, BitConstraints vendor a src dst) =>
  ValidBit (vendor :: Symbol) (a :: Type) (src :: Domain) (dst :: Domain)

-- | Helper typeclass for bringing in the primitive typeclass and additional constraints
class
  (Bit vendor, BitWithConstraints vendor stages a src dst) =>
  ValidBitWith (vendor :: Symbol) (stages :: Nat) (a :: Type) (src :: Domain) (dst :: Domain)

instance
  (Bit vendor, BitWithConstraints vendor stages a src dst) =>
  ValidBitWith (vendor :: Symbol) (stages :: Nat) (a :: Type) (src :: Domain) (dst :: Domain)

-- | Guarantees the existence of a sync reset CDC primitive for a given vendor
class SyncRst vendor where
  -- | Additional constraints on the sync reset CDC primitive with a custom configuration
  type SyncRstWithConstraints vendor (stages :: Nat) (src :: Domain) (dst :: Domain) :: Constraint

  -- | Configuration type for a sync reset CDC primitive
  type SyncRstConfig vendor (stages :: Nat) (src :: Domain) (dst :: Domain) :: Type

  -- | Sync reset CDC primitive with a custom configuration
  syncRstWith ::
    forall stages src dst.
    ( KnownDomain src
    , KnownDomain dst
    , HasCallStack
    , SyncRstWithConstraints vendor stages src dst
    ) =>
    SyncRstConfig vendor stages src dst ->
    Clock src ->
    Clock dst ->
    Reset src ->
    Reset dst

  -- | Additional constraints on the sync reset primitive with the default configuration
  type SyncRstConstraints vendor (src :: Domain) (dst :: Domain) :: Constraint

  -- | Reset assertion type
  type ResetAssert vendor :: Type

  -- | Provides the default reset assert for @ResetAssert vendor@
  defaultAssert :: ResetAssert vendor

  -- | Provides the asserted form of @ResetAssert vendor@
  asserted :: ResetAssert vendor

  -- | Provides the deasserted form of @ResetAssert vendor@
  deasserted :: ResetAssert vendor

  -- | Default number of stages in a sync reset CDC primitive
  type SyncRstDefaultStages vendor (src :: Domain) (dst :: Domain) :: Nat

  -- | Default configuration for a sync reset CDC primitive
  defaultSyncRstConfig ::
    forall src dst.
    (KnownDomain src, KnownDomain dst) =>
    ResetAssert vendor ->
    Proxy src ->
    Proxy dst ->
    SyncRstConfig vendor (SyncRstDefaultStages vendor src dst) src dst

  -- | Sync reset CDC primitive with the default configuration
  syncRst ::
    forall src dst.
    ( KnownDomain src
    , KnownDomain dst
    , HasCallStack
    , SyncRstConstraints vendor src dst
    ) =>
    ResetAssert vendor ->
    Clock src ->
    Clock dst ->
    Reset src ->
    Reset dst
  default syncRst ::
    ( KnownDomain src
    , KnownDomain dst
    , HasCallStack
    , SyncRstWithConstraints vendor (SyncRstDefaultStages vendor src dst) src dst
    ) =>
    ResetAssert vendor ->
    Clock src ->
    Clock dst ->
    Reset src ->
    Reset dst
  syncRst resetAssert (srcClk :: Clock src) (dstClk :: Clock dst) (srcRst :: Reset src) =
    syncRstWith @vendor @(SyncRstDefaultStages vendor src dst) @src @dst
      (defaultSyncRstConfig @vendor @src @dst resetAssert Proxy Proxy)
      srcClk
      dstClk
      srcRst

-- | Helper typeclass for bringing in the primitive typeclass and additional constraints
class
  (SyncRst vendor, SyncRstConstraints vendor src dst) =>
  ValidSyncRst (vendor :: Symbol) (src :: Domain) (dst :: Domain)

instance
  (SyncRst vendor, SyncRstConstraints vendor src dst) =>
  ValidSyncRst (vendor :: Symbol) (src :: Domain) (dst :: Domain)

-- | Helper typeclass for bringing in the primitive typeclass and additional constraints
class
  (SyncRst vendor, SyncRstWithConstraints vendor stages src dst) =>
  ValidSyncRstWith (vendor :: Symbol) (stages :: Nat) (src :: Domain) (dst :: Domain)

instance
  (SyncRst vendor, SyncRstWithConstraints vendor stages src dst) =>
  ValidSyncRstWith (vendor :: Symbol) (stages :: Nat) (src :: Domain) (dst :: Domain)

type HiddenVendor (vendor :: Symbol) = (KnownSymbol vendor, ?vendorName :: SSymbol vendor)

-- | Alias for @let ?vendorName = ssym in f@
withVendor ::
  forall vendor r. (KnownSymbol vendor) => SSymbol vendor -> ((HiddenVendor vendor) => r) -> r
withVendor ssym f = let ?vendorName = ssym in f

-- | Implicitly creates the @SSymbol vendor@ used to call 'withVendor'
withVendorI :: forall vendor r. (KnownSymbol vendor) => ((HiddenVendor vendor) => r) -> r
withVendorI = withVendor (SSymbol @vendor)

-- | `Maybe a` based version of `handshake`.
handshakeMaybe ::
  forall a src dst vendor.
  ( KnownDomain src
  , KnownDomain dst
  , BitPack a
  , NFDataX a
  , HiddenVendor vendor
  , ValidHandshake vendor a src dst
  ) =>
  Clock src ->
  Clock dst ->
  Signal src (Maybe a) ->
  Signal dst Bool ->
  (Signal src Bool, Signal dst (Maybe a))
handshakeMaybe clkSrc clkDst srcIn dstAck = (srcRcv, toMaybe <$> dstReq <*> dstOut)
 where
  (dstOut, dstReq, srcRcv) =
    handshake @vendor clkSrc clkDst (fromMaybe (unpack 0) <$> srcIn) (isJust <$> srcIn) dstAck

{- | Reliable CDC component based on 'handshakeMaybe' without backpressure and with limited
throughput. Data will be lost if the `src` domain provides more inputs than the circuit can handle.
Useful for low granularity synchronization.
-}
maybeLossy ::
  forall a src dst vendor.
  ( KnownDomain src
  , KnownDomain dst
  , BitPack a
  , NFDataX a
  , HiddenVendor vendor
  , ValidHandshake vendor a src dst
  ) =>
  Clock src ->
  Clock dst ->
  Signal src (Maybe a) ->
  Signal dst (Maybe a)
maybeLossy clkSrc clkDst maybeInp =
  mux (isRising clkDst noReset enableGen False dstAck) dstOut (pure Nothing)
 where
  srcReg =
    regEn
      clkSrc
      noReset
      enableGen
      Nothing
      (srcRcv .||. srcRegEmpty)
      $ mux (srcRegEmpty .&&. fmap not srcRcv) maybeInp (pure Nothing)

  srcRegEmpty = isNothing <$> srcReg
  (srcRcv, dstOut) = handshakeMaybe @a @src @dst @vendor clkSrc clkDst srcReg (isJust <$> dstOut)
  dstAck = isJust <$> dstOut
