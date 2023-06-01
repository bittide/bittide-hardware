-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Counter
  ( Active
  , domainDiffCounter
  ) where

import Clash.Explicit.Prelude

import Clash.Cores.Xilinx.Xpm (xpmCdcGray)
import Clash.Sized.Extra (unsignedToSigned, concatUnsigneds)

-- | State of 'domainDiffCounter'
data DdcState
  -- | In reset, or waiting for the incoming counter to change
  = DdcInReset
  -- | Counting and comparing with incoming domain
  | DdcRunning (Unsigned 64)
  deriving (Generic, NFDataX)

-- | Indicates whether 'domainDiffCounter' is actively counting or still in reset.
type Active = Bool

-- | Determine speed differences between two domains. If the source domain is
-- faster than the destination domain, the result will become larger. Vice versa,
-- if the source domain is slower than the destination domain, the result will
-- become smaller. This is analogous to what would happen to a Fido's data count
-- when continuously written to by the source domain and read from by the
-- destination domain. To ease integration in control algorithms, this component
-- makes sure it starts counting at zero. It also waits for the incoming counter
-- to become active, i.e. non-zero, before starting to count itself.
--
-- If both domains support initial values, 'domainDiffCounter' does not need to
-- be reset.
--
-- The counter supports enable signals for both domains, which can be used to
-- simulate fill and drain behavior of an analogous FIFO's data count:
--
--   * source enabled  && destination enabled  corresponds to a FIFO @Pass@
--   * source disabled && destination enabled  corresponds to a FIFO @Drain@
--   * source enabled  && destination disabled corresponds to a FIFO @Fill@
--   * source disabled && destination disabled corresponds to a disabled FIFO
--
-- To reset this component, the reset should be asserted for at least one cycle in
-- the source domain _plus_ four cycles in the destination domain. The reset in the
-- destination domain should be deasserted at the same time or *after* the one in
-- the source domain for glitchless operation.
--
-- __N.B.__:
--   This function will only work properly if the given domains are pretty close
--   to another for a number of reasons:
--
--     1. It uses an 8-bit Gray counter internally for CDC
--
--     2. It uses one 64-bit counter in each domain
--
--   These values have been chosen such that:
--
--     * The 64-bit counter only overflows once every 3000 years at 200 MHz
--
domainDiffCounter ::
  forall n src dst.
  ( KnownDomain src
  , KnownDomain dst
  , KnownNat n
  , n <= 65
  ) =>
  Clock src -> Reset src -> Enable src ->
  Clock dst -> Reset dst -> Enable dst ->
  -- | Counter and boolean indicating whether the component is currently active
  Signal dst (Signed n, Active)

domainDiffCounter clkSrc rstSrc enbSrc clkDst rstDst enbDst =
  mealy clkDst rstDst enableGen go DdcInReset
    $ bundle (fromEnable enbDst, counter)
 where
  -- 64 bits is enough for approximately 3 millenia @ 200 MHz
  counter = synchronizedSuccCounter @64 clkSrc rstSrc enbSrc clkDst rstDst

  go ::
    (KnownNat n, n <= 65) =>
    DdcState ->
    (Bool, Unsigned 64) ->
    (DdcState, (Signed n, Bool))
  go DdcInReset      (enabled, c1)
    | c1 == 0   = (DdcInReset,          (0, False))
    | enabled   = (DdcRunning (c1 + 1), (0, True))
    | otherwise = (DdcRunning c1,       (0, True))
  go (DdcRunning c0) (enabled, c1)
    | enabled   = (DdcRunning (c0 + 1), (c1 `subAndTruncate` c0, True))
    | otherwise = (DdcRunning c0,       (c1 `subAndTruncate` c0, True))

  subAndTruncate :: (KnownNat n, n <= 65) => Unsigned 64 -> Unsigned 64 -> Signed n
  subAndTruncate c0 c1 =
    ( truncateB :: (KnownNat n, n <= 65, n + (65 - n) ~ 65) => Signed 65 -> Signed n
    ) (unsignedToSigned c0 - unsignedToSigned c1)

-- | A counter that counts /up/, synchronized from the domain @src@ to domain @dst@. To
-- reset this component, the reset should be asserted for at least one cycle in the
-- source domain _plus_ four cycles in the destination domain.
--
-- __N.B.__: This function uses an 8-bit Gray counter internally, and will therefore
--           only work properly if both clock speeds are pretty close to one another.
synchronizedSuccCounter ::
  forall n src dst .
  ( KnownDomain src
  , KnownDomain dst
  , KnownNat n
  , 8 <= n
  ) =>
  Clock src -> Reset src -> Enable src ->
  Clock dst -> Reset dst ->
  Signal dst (Unsigned n)
synchronizedSuccCounter clkSrc rstSrc enbSrc clkDst rstDst =
  extendSuccCounter @8 @(n - 8) clkDst rstDst $
    xpmCdcGray @8 clkSrc clkDst counter
 where
  counter :: Signal src (Unsigned 8)
  counter = register clkSrc rstSrc enbSrc 0 (counter + 1)

-- | State of 'extendSuccCounter'
data EscState m
  -- | In reset, or waiting for an overflow
  = EscInReset
  -- | Counting - whenever an overflow occurs, this constructors field is upped
  | EscRunning (Unsigned m)
  deriving (Generic, NFDataX)

-- | Given a counter that counts /up/, extend the size of the counter. After its
-- reset is deasserted, it will wait until it sees an overflow to ensure the
-- counter is glitchless and always starts at 0.
--
-- This can be used to extend computationally complex counter components, such as
-- Gray counters.
extendSuccCounter ::
  forall n m dom .
  ( KnownDomain dom
  , KnownNat n
  , KnownNat m ) =>
  Clock dom -> Reset dom ->
  Signal dom (Unsigned n) ->
  Signal dom (Unsigned (m + n))
extendSuccCounter clk rst counterLower =
  mealyB clk rst enableGen go EscInReset
    ( isFalling clk rst enableGen 0 (msb <$> counterLower)
    , counterLower )
 where
  go :: EscState m -> (Bool, Unsigned n) -> (EscState m, Unsigned (m + n))
  go EscInReset      (False,    _)  = (EscInReset,    0)
  go EscInReset      (True,     n0) = (EscRunning 0,  extend n0)
  go (EscRunning m0) (overflow, n0) = (EscRunning m1, n1)
   where
    m1 = if overflow then m0 + 1 else m0
    n1 = concatUnsigneds m1 n0
