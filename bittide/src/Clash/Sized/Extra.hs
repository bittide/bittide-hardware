-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Clash.Sized.Extra where

import Clash.Prelude

-- | Safe 'Unsigned' to 'Signed' conversion
unsignedToSigned :: forall n. KnownNat n => Unsigned n -> Signed (n + 1)
unsignedToSigned n = bitCoerce (zeroExtend n)
