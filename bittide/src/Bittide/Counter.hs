-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Counter (
  Active,
  domainDiffCounter,
  domainDiffCountersWbC,
) where

import Clash.Explicit.Prelude
import Protocols

import Bittide.SharedTypes (Bytes)
import Clash.Class.BitPackC (ByteOrder)
import Clash.Cores.Xilinx.Xpm (xpmCdcGray)
import Clash.Functor.Extra ((<<$>>))
import Clash.Sized.Extra (concatUnsigneds, unsignedToSigned)
import GHC.Stack (HasCallStack)
import Protocols.MemoryMap (Access (ReadOnly), ConstBwd, MM)
import Protocols.MemoryMap.Registers.WishboneStandard (
  RegisterConfig (access, description),
  deviceWb,
  registerConfig,
  registerWb,
  registerWb_,
 )
import Protocols.Wishbone (Wishbone, WishboneMode (Standard))

-- | State of 'domainDiffCounter'
data DdcState
  = -- | In reset, or waiting for the incoming counter to change
    DdcInReset
  | -- | Counting and comparing with incoming domain
    DdcRunning (Unsigned 64)
  deriving (Generic, NFDataX)

-- | Indicates whether 'domainDiffCounter' is actively counting or still in reset.
type Active = Bool

{- | Determine speed differences between two domains. If the source domain is
faster than the destination domain, the result will become larger. Vice versa,
if the source domain is slower than the destination domain, the result will
become smaller. This is analogous to what would happen to a FIFO's data count
when continuously written to by the source domain and read from by the
destination domain. To ease integration in control algorithms, this component
makes sure it starts counting at zero. It also waits for the incoming counter
to become active, i.e. non-zero, before starting to count itself.

If both domains support initial values, 'domainDiffCounter' does not need to
be reset.

To reset this component, the reset should be asserted for at least one cycle in
the source domain _plus_ four cycles in the destination domain. The reset in the
destination domain should be deasserted at the same time or *after* the one in
the source domain for glitchless operation.

__N.B.__:
  This function will only work properly if the given domains are pretty close
  to another for a number of reasons:

    1. It uses an 8-bit Gray counter internally for CDC

    2. It uses one 64-bit counter in each domain

    3. Its output is constrained to @Signed 32@

  These values have been chosen such that:

    * The 64-bit counter only overflows once every 3000 years at 200 MHz

    * The 32-bit output only overflows after running at maximum divergence
      rate (100 ppm) at 200 MHz for 2 days. We expect systems to stabilize
      after a few milliseconds and reframing should nudge counters back to
      zero ever so often.
-}
domainDiffCounter ::
  forall src dst.
  ( KnownDomain src
  , KnownDomain dst
  ) =>
  Clock src ->
  Reset src ->
  Clock dst ->
  Reset dst ->
  -- | Counter and boolean indicating whether the component is currently active
  Signal dst (Signed 32, Active)
domainDiffCounter clkSrc rstSrc clkDst rstDst =
  mealy clkDst rstDst enableGen go DdcInReset counter
 where
  -- 64 bits is enough for approximately 3 millenia @ 200 MHz
  counter = synchronizedSuccCounter @64 clkSrc rstSrc clkDst rstDst

  go :: DdcState -> Unsigned 64 -> (DdcState, (Signed 32, Bool))
  go DdcInReset c1
    | c1 == 0 = (DdcInReset, (0, False))
    | otherwise = (DdcRunning (c1 + 1), (0, True))
  go (DdcRunning c0) c1 = (DdcRunning (c0 + 1), (c1 `subAndTruncate` c0, True))

  subAndTruncate :: Unsigned 64 -> Unsigned 64 -> Signed 32
  subAndTruncate c0 c1 = truncateB (unsignedToSigned c0 - unsignedToSigned c1)

{- | A bunch of 'domainDiffCounter's that can be accessed over a Wishbone bus. The
counters can be enabled and disabled over the bus. Other than that, they can
only be read from. See 'domainDiffCounter' for more information on domain
difference counters in general.
-}
domainDiffCountersWbC ::
  forall src dst n addrW.
  ( HasCallStack
  , KnownDomain src
  , KnownDomain dst
  , HasSynchronousReset src
  , HasSynchronousReset dst
  , KnownNat n
  , KnownNat addrW
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
  Vec n (Clock src) ->
  Vec n (Reset src) ->
  Clock dst ->
  Reset dst ->
  Circuit
    (ConstBwd MM, Wishbone dst 'Standard addrW (Bytes 4))
    (CSignal dst (Vec n (Signed 32, Active)))
domainDiffCountersWbC srcClocks srcResets clk rst = circuit $ \bus -> do
  [enableWb, countersWb, activesWb] <- deviceWb "DomainDiffCounters" -< bus

  (Fwd enables, _a) <-
    registerWb clk rst enableConfig (repeat False) -< (enableWb, Fwd noWrite)
  registerWb_ clk rst countersConfig (repeat 0) -< (countersWb, Fwd countersWrite)
  registerWb_ clk rst activeConfig (repeat False) -< (activesWb, Fwd countersActiveWrite)

  let
    resets = unsafeFromActiveLow <$> unbundle enables
    counters = zipWith4 domainDiffCounter srcClocks srcResets (repeat clk) resets
    countersB = bundle counters

    countersWrite = Just <$> (fst <<$>> countersB)
    countersActiveWrite = Just <$> (snd <<$>> countersB)

  idC -< Fwd countersB
 where
  noWrite = pure Nothing
  countersConfig =
    (registerConfig "counters")
      { access = ReadOnly
      }

  activeConfig =
    (registerConfig "counters_active")
      { description = "Active state of the counters, i.e. whether they are counting or not."
      , access = ReadOnly
      }

  enableConfig =
    (registerConfig "enable")
      { description = "Counters enabled? Counter is cleared when disabled."
      }

{- | A counter that counts /up/, synchronized from the domain @src@ to domain @dst@. To
reset this component, the reset should be asserted for at least one cycle in the
source domain _plus_ four cycles in the destination domain.

__N.B.__: This function uses an 8-bit Gray counter internally, and will therefore
          only work properly if both clock speeds are pretty close to one another.
-}
synchronizedSuccCounter ::
  forall n src dst.
  ( KnownDomain src
  , KnownDomain dst
  , KnownNat n
  , 8 <= n
  ) =>
  Clock src ->
  Reset src ->
  Clock dst ->
  Reset dst ->
  Signal dst (Unsigned n)
synchronizedSuccCounter clkSrc rstSrc clkDst rstDst =
  extendSuccCounter @8 @(n - 8) clkDst rstDst
    $ xpmCdcGray @8 clkSrc clkDst counter
 where
  counter :: Signal src (Unsigned 8)
  counter = register clkSrc rstSrc enableGen 0 (counter + 1)

-- | State of 'extendSuccCounter'
data EscState m
  = -- | In reset, or waiting for an overflow
    EscInReset
  | -- | Counting - whenever an overflow occurs, this constructors field is upped
    EscRunning (Unsigned m)
  deriving (Generic, NFDataX)

{- | Given a counter that counts /up/, extend the size of the counter. After its
reset is deasserted, it will wait until it sees an overflow to ensure the
counter is glitchless and always starts at 0.

This can be used to extend computationally complex counter components, such as
Gray counters.
-}
extendSuccCounter ::
  forall n m dom.
  ( KnownDomain dom
  , KnownNat n
  , KnownNat m
  ) =>
  Clock dom ->
  Reset dom ->
  Signal dom (Unsigned n) ->
  Signal dom (Unsigned (m + n))
extendSuccCounter clk rst counterLower =
  mealyB
    clk
    rst
    enableGen
    go
    EscInReset
    ( isFalling clk rst enableGen 0 (msb <$> counterLower)
    , counterLower
    )
 where
  go :: EscState m -> (Bool, Unsigned n) -> (EscState m, Unsigned (m + n))
  go EscInReset (False, _) = (EscInReset, 0)
  go EscInReset (True, n0) = (EscRunning 0, extend n0)
  go (EscRunning m0) (overflow, n0) = (EscRunning m1, n1)
   where
    m1 = if overflow then m0 + 1 else m0
    n1 = concatUnsigneds m1 n0
