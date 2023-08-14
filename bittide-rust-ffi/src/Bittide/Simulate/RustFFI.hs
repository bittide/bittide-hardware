-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE ForeignFunctionInterface #-}

module Bittide.Simulate.RustFFI (callisto_rust) where

import Foreign.C.Types (CUInt(..))
import Foreign.Ptr (Ptr)

foreign import ccall safe "__c_callisto_rust" callisto_rust ::
  Ptr () -> -- ^ config
  CUInt  -> -- ^ availability_mask
  Ptr () -> -- ^ stability_checks
  Ptr () -> -- ^ data_counts
  Ptr () -> -- ^ control_state
  IO ()
