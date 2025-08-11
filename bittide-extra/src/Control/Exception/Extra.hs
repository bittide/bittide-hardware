-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Control.Exception.Extra where

import Prelude

import Control.Monad.Catch (MonadMask, bracket)

-- | Like 'bracket', but with a list of actions.
brackets :: (MonadMask m) => [m a] -> (a -> m b) -> ([a] -> m c) -> m c
brackets acqs rel act = go [] acqs
 where
  go resL [] = act (reverse resL)
  go resL (acq : acqs1) = bracket acq rel $ \res -> go (res : resL) acqs1
