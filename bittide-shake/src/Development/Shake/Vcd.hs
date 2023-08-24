-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Development.Shake.Vcd where

import Data.List.Extra (splitOn, unsnoc)
import Data.Maybe (fromMaybe)

type Name = String

-- | Vivado generates probe names such as:
--
-- @fullMeshHwCcWithRiscvTest20_Bittide_Instances_Tests_FullMeshHwCcWithRiscv_fullMeshHwCcWithRiscvTest_fullMeshHwCcWithRiscvTest21_result_15/trigger@
--
-- This function shortens that to simply:
--
-- @trigger@
--
-- Also removes bus suffixes, see 'removeBusSuffix'.
--
shortenNames ::
  -- | VCD
  String ->
  -- | VCD with short names
  Either String String
shortenNames = rewriteNames (removeBusSuffix . removePrefix)

-- | Remove prefix from a long probe name, see 'shortenNames'.
removePrefix :: String -> String
removePrefix = maybe "" snd  . unsnoc . splitOn "/"

-- | Remove bus suffixes
--
-- >>> removeBusSuffix "abc [3:0]"
-- "abc"
-- >>> removeBusSuffix "abc [3:0] [4:0]"
-- "abc"
-- >>> removeBusSuffix "abc [3:0][4:0]"
-- "abc"
-- >>> removeBusSuffix "abc[3:0][4:0]"
-- "abc"
-- >>> removeBusSuffix "abc"
-- "abc"
-- >>> removeBusSuffix "abc0"
-- "abc0"
-- >>> removeBusSuffix "abc[0]"
-- "abc[0]"
-- >>> removeBusSuffix "abc[a:0]"
-- "abc[a:0]"
removeBusSuffix :: String -> String
removeBusSuffix = reverse . go . reverse
 where
  go s0 = fromMaybe s0 (parseOneOrMore parseSuffix s0)

  parseSuffix s = do
        Just s
    >>= parseZeroOrMore (parseChar ' ')
    >>= parseBus
    >>= parseZeroOrMore (parseChar ' ')

  parseBus s =
        Just s
    >>= parseChar ']'
    >>= parseOneOrMore parseDigit
    >>= parseChar ':'
    >>= parseOneOrMore parseDigit
    >>= parseChar '['

  parseDigit (s:ss) | s `elem` "0123456789" = Just ss
  parseDigit _ = Nothing

  parseChar c (s:ss) | c == s = Just ss
  parseChar _ _ = Nothing

  parseOneOrMore parser ss0 = parseZeroOrMore parser =<< parser ss0
  parseZeroOrMore parser ss0 = Just (fromMaybe ss0 (parser ss0))

-- | Rewrite probe names in VCD given as a 'String'
rewriteNames ::
  -- | Name replacement function
  (Name -> Name) ->
  -- | VCD
  String ->
  -- | VCD with updated names
  Either String String
rewriteNames f = fmap unlines . traverse go . lines
 where
  go :: String -> Either String String
  go l@('$':'v':'a':'r':' ':_)
    | Just (var : typ : width : id_ : name, end) <- unsnoc (words l)
    , "$end" <- end
    = Right (unwords [var, typ, width, id_, f (unwords name), end])
    | otherwise
    = Left ("Couldn't parse: " <> l)
  go l = Right l
