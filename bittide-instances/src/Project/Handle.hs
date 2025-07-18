-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE PackageImports #-}

module Project.Handle where

import Prelude hiding (filter)

import Data.ByteString (filter, unpack)
import Data.ByteString.Char8 (hGetLine)
import Data.ByteString.Internal (w2c)
import Data.Word8 (isAscii, isControl)
import Debug.Trace
import System.IO (Handle, hGetChar, hReady)
import "extra" Data.List.Extra (trimEnd)

import Test.Tasty.HUnit

import qualified System.IO as IO

data Error = Ok | Error String
data Filter = Continue | Stop Error

-- | Convert an 'Error' to an 'Assertion'.
errorToException :: Error -> Assertion
errorToException Ok = pure ()
errorToException (Error msg) = assertFailure msg

{- | Utility function that reads lines from a handle, and applies a filter to
each line. If the filter returns 'Continue', the function will continue
reading lines. If the filter returns @Stop Ok@, the function will return
successfully. If the filter returns @Stop (Error msg)@, the function will
fail with the given message, along with a log of all processed lines.
-}
expectLine :: (HasCallStack) => Handle -> (String -> Filter) -> Assertion
expectLine = expectLine' ""
 where
  expectLine' s0 h f = do
    byteLine0 <- hGetLine h
    let
      byteLine1 = filter (\c -> isAscii c && not (isControl c)) byteLine0
      line = w2c <$> unpack byteLine1
      trimmed = trimEnd line
      s1 = s0 <> "\n" <> line
      cont = expectLine' s1 h f
    if null trimmed
      then cont
      else case f trimmed of
        Continue -> cont
        Stop Ok -> pure ()
        Stop (Error msg) -> do
          putStrLn s1
          assertFailure msg

{- | Utility function that reads lines from a handle, and waits for a specific
line to appear. Though this function does not fail in the traditional sense,
it will get stuck if the expected line does not appear. Only use in combination
with sensible time outs (also see 'main').
-}
waitForLine :: Handle -> String -> IO ()
waitForLine h expected =
  expectLine h $ \s ->
    trace ("Wait for \"" <> expected <> "\", got: " <> s) $
      if s == expected
        then Stop Ok
        else Continue

-- Utility function that returns the remaining characters in a handle.
readRemainingChars :: Handle -> IO String
readRemainingChars h = do
  rdy <- hReady h
  if rdy
    then do
      c <- hGetChar h
      (c :) <$> readRemainingChars h
    else pure ""

{- | Read characters from a handle until a specific string is encountered.
Do not use on Handles that might return non-ASCII characters.
-}
readUntil :: Handle -> String -> IO String
readUntil handle ending = do
  initBuf <- hGetNChar handle (length ending)
  go "" initBuf
 where
  go _ "" = error $ "readUntil: Never saw expected: " <> ending
  go acc buf@(bufHead : bufTail)
    | buf == ending = pure acc
    | otherwise = do
        c <- hGetChar handle
        go (acc <> [bufHead]) (bufTail <> [c])

{- | Read lines from a handle until a specific line is encountered.
Do not use on Handles that might return non-ASCII characters.
-}
readUntilLine :: Handle -> String -> IO [String]
readUntilLine h expected = do
  line <- IO.hGetLine h
  if line == expected
    then pure []
    else (line :) <$> readUntilLine h expected

{- | Read n characters from a handle.
Do not use on Handles that might return non-ASCII characters.
-}
hGetNChar :: Handle -> Int -> IO String
hGetNChar h n = go n []
 where
  go 0 acc = pure $ reverse acc
  go i acc = do
    c <- hGetChar h
    go (pred i) (c : acc)
