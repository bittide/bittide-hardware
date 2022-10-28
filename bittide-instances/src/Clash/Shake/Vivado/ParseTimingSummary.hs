-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Clash.Shake.Vivado.ParseTimingSummary where

import Prelude

import Clash.Shake.Vivado.Util
import Data.Char (isSpace)
import Data.List (dropWhileEnd, isPrefixOf)
import Data.List.Split (splitWhen)
import Data.Map (Map)
import Data.Maybe (isJust, listToMaybe)
import Data.String.Utils (strip)

import qualified Data.Map as Map

type Table = Map String [String]
type Report = Map String [Table]


-- | Parses all tables in a report, and divvies them up into sections.
parse :: [String] -> Report
parse = Map.map (parseAll parseTable) . parseSections

-- | Parses all tables in a report, and divvies them up into sections.
parseFile :: FilePath -> IO Report
parseFile path = parse . lines <$> readFile path

-- | Extract all values from a report, paired as column name plus its value.
allValues :: Report -> [(String, String)]
allValues report = do
  (_sectionName, tables) <- Map.toList report
  table <- tables
  (columnName, values) <- Map.toList table
  value <- values
  pure (columnName, value)

-- | Does the design meet timing? Returns 'Just' if any WNS is negative, returns
-- 'Nothing' otherwise.
meetsTiming :: Report -> Maybe String
meetsTiming report = listToMaybe negativeNegativeSlacks
 where
  negativeNegativeSlacks = filter ("-" `isPrefixOf`) negativeSlacks
  negativeSlacks = [value | (colName, value) <- allValues report, "WNS" `isPrefixOf` colName]

-- | Try to parse a table. This only parses a table that looks like this:
--
-- @
-- Clock             WNS(ns)      TNS(ns)
-- -----             -------      -------
-- clk                 1.824        0.000
-- @
--
-- These kinds of tables are found in
parseTable :: [String] -> Maybe (Table, [String])
parseTable (headers:dividers:values:rest0)
  -- Parsing a table is a fairly complicated affair because it spans 2 (and
  -- potentially) many more lines. We check a couple of things.
  --
  -- 1. The (potential) headers and divider lines aren't empty
  | not (any isEmptyLine [headers, dividers, values])

  -- 2. All the divider characters ('-') match up with headers and values. See
  --    'looksLikeTable' for more info.
  , and (zipWith3 looksLikeTable headers dividers values)

  -- 3. None of the column titles is an empty string.
  , not (any isEmptyLine (Map.keys table0))

  -- 4. The table has at least 2 columns.
  , length table0 > 1

  -- Lastly, we try to parse the rest of the table by feeding the same 'headers'
  -- and 'dividers', but with the rest of the lines. This is somewhat inefficient
  -- as we already parsed the headers/dividers, but timing reports are never
  -- large - so yours truly doesn't really care.
  = case parseTable (headers:dividers:rest0) of
      Nothing -> Just (table0, rest0)
      Just (table1, rest1) -> Just (Map.unionWith (<>) table0 table1, rest1)
 where
  table0 =
      Map.fromList
    $ map (\(h, _, v) -> (strip h, [strip v]))
    $ filter (\(_, d, _) -> not (isEmptyLine d))
    $ map unzip3
    $ splitWhen (\(_, d, _) -> isSpace d) (zip3 headers dividers values)

  looksLikeTable header divider value =
       divider `elem` [' ', '-']
    && (isSpace divider      `implies` isSpace value)
    && (not (isSpace value)  `implies` not (isSpace divider))
    && (not (isSpace header) `implies` not (isSpace divider))

parseTable _ = Nothing

-- | Parses a number of lines looking like:
--
-- @
-- ---------------------------------------------
-- | Design Timing Summary
-- | ---------------------
-- ---------------------------------------------
-- @
--
parseSectionHeader :: [String] -> Maybe (String, [String])
parseSectionHeader (a:b:c:d:rest)
  | '-':'-':_     <- a
  , '|':' ':title <- b
  , '|':' ':_     <- c
  , '-':'-':_     <- d
  = Just (dropWhileEnd isSpace title, rest)
parseSectionHeader _ = Nothing

-- | Splits a Vivado report into its (unparsed) sections
parseSections :: [String] -> Map String [String]
parseSections = Map.fromList . go
 where
  go [] = []
  go (l:ls) =
    case parseSectionHeader (l:ls) of
      Just (title, rest0) ->
        case breakOverMultipleLines (isJust . parseSectionHeader) rest0 of
          (body, rest1) ->
            (title, body) : go rest1
      Nothing ->
        go ls
