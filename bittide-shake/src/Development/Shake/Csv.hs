-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Development.Shake.Csv where

import Data.ByteString.Lazy (ByteString)
import Data.Csv (HasHeader(NoHeader), decode, encode)
import Data.Vector (toList)

import Development.Shake.Vcd (Name, removePrefix, removeBusSuffix)

-- | Vivado generates probe names such as:
--
-- @fullMeshHwCcWithRiscvTest20_Bittide_Instances_Tests_FullMeshHwCcWithRiscv_fullMeshHwCcWithRiscvTest_fullMeshHwCcWithRiscvTest21_result_15/trigger@
--
-- This function shortens that to simply:
--
-- @trigger@
--
--
-- Also removes bus suffixes, see 'removeBusSuffix'.
--
shortenNames ::
  -- | CSV
  ByteString ->
  -- | CSV with short names
  Either String ByteString
shortenNames = rewriteNames (removeBusSuffix . removePrefix)

-- | Rewrite probe names in CSV given as a 'String'
rewriteNames ::
  -- | Name replacement function
  (Name -> Name) ->
  -- | CSV
  ByteString ->
  -- | CSV with updated names
  Either String ByteString
rewriteNames f = fmap (encode . go . toList) . decode NoHeader
 where
  go :: [[String]] -> [[String]]
  go [] = []
  go (header:rest) = map f header : rest
