-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE PackageImports #-}

module Project.Handle where

import Prelude hiding (filter)

import Control.Concurrent.Chan
import Data.ByteString (ByteString, filter, unpack)
import Data.ByteString.Char8 (hGetLine)
import Data.ByteString.Internal (w2c)
import Data.Word8 (isAscii, isControl)
import Debug.Trace
import System.IO (Handle, hGetChar, hReady)
import "extra" Data.List.Extra (trimEnd)

import Test.Tasty.HUnit

data Filter = Continue | Stop (Either String ())

-- | Convert an 'Either String a' to an 'Assertion'. Discards the 'a' value.
assertEither :: Either String a -> Assertion
assertEither (Right _) = pure ()
assertEither (Left msg) = error msg

-- | Convert an 'Either String a' to an 'IO a'. Throws an error on 'Left'.
expectRight :: (HasCallStack) => Either String a -> IO a
expectRight (Right v) = pure v
expectRight (Left msg) = error msg

-- | Version of `expectLine` that directly throws an error on `Left`.
expectLine_ :: (HasCallStack) => Handle -> (String -> Filter) -> Assertion
expectLine_ h f = do
  result <- expectLine h f
  assertEither result

expectLineChan_ :: (HasCallStack) => Chan ByteString -> (String -> Filter) -> Assertion
expectLineChan_ c f = do
  result <- expectLineChan c f
  assertEither result

{- | Utility function that reads lines from a handle, and applies a filter to
each line. If the filter returns 'Continue', the function will continue
reading lines. If the filter returns @Stop (Right ())@, the function will return
successfully with the accumulated lines. If the filter returns @Stop (Left msg)@,
the function will fail with the given message, along with a log of all processed lines.
-}
expectLineGeneric ::
  (HasCallStack) => a -> (a -> IO ByteString) -> (String -> Filter) -> IO (Either String [String])
expectLineGeneric = expectLine' []
 where
  expectLine' acc h r f = do
    byteLine0 <- r h
    let
      byteLine1 = filter (\c -> isAscii c && not (isControl c)) byteLine0
      line = w2c <$> unpack byteLine1
      trimmed = trimEnd line
      acc' = acc <> [line]
      cont = expectLine' acc' h r f
    if null trimmed
      then cont
      else case f trimmed of
        Continue -> cont
        Stop (Right ()) -> pure (Right acc')
        Stop (Left msg) -> do
          putStrLn (unlines acc')
          pure (Left msg)

expectLine ::
  (HasCallStack) => Handle -> (String -> Filter) -> IO (Either String [String])
expectLine h = expectLineGeneric h hGetLine

expectLineChan ::
  (HasCallStack) => (Chan ByteString) -> (String -> Filter) -> IO (Either String [String])
expectLineChan c = expectLineGeneric c readChan

{- | Utility function that reads lines from a handle, and waits for a specific
line to appear. Though this function does not fail in the traditional sense,
it will get stuck if the expected line does not appear. Only use in combination
with sensible time outs (also see 'main').
-}
waitForLine :: Handle -> String -> IO ()
waitForLine h expected = do
  expectLine_ h $ \s ->
    trace ("Wait for \"" <> expected <> "\", got: " <> s) $
      if s == expected
        then Stop (Right ())
        else Continue

waitForLineChan :: Chan ByteString -> String -> IO ()
waitForLineChan c expected = do
  expectLineChan_ c $ \s ->
    trace ("Wait for \"" <> expected <> "\", got: " <> s) $
      if s == expected
        then Stop (Right ())
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
  result <- expectLine h $ \s ->
    trace ("Reading until \"" <> expected <> "\", got: " <> s) $
      if s == expected
        then Stop (Right ())
        else Continue
  case result of
    Right lines' -> pure (init lines') -- Remove the expected line from the result
    Left err -> error err

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
