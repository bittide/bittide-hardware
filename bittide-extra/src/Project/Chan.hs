-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Project.Chan where

import Prelude hiding (filter)

import Control.Concurrent.Chan
import Data.ByteString (ByteString)
import Debug.Trace
import Project.Handle (Filter (..), assertEither, expectLineWith, readUntilLineWith)

import Test.Tasty.HUnit

-- | Version of 'expectLine' that directly throws an error on 'Left'.
expectLine_ :: (HasCallStack) => Chan ByteString -> (String -> Filter) -> Assertion
expectLine_ c f = do
  result <- expectLine c f
  assertEither result

{- | Utility function that reads lines from a channel, and applies a filter to
each line. If the filter returns 'Continue', the function will continue
reading lines. If the filter returns @Stop (Right ())@, the function will return
successfully with the accumulated lines. If the filter returns @Stop (Left msg)@,
the function will fail with the given message, along with a log of all processed lines.
-}
expectLine ::
  (HasCallStack) => Chan ByteString -> (String -> Filter) -> IO (Either String [String])
expectLine c = expectLineWith c readChan

{- | Utility function that reads lines from a channel, and waits for a specific
line to appear. Though this function does not fail in the traditional sense,
it will get stuck if the expected line does not appear. Only use in combination
with sensible time outs (also see 'main').
-}
waitForLine :: Chan ByteString -> String -> IO ()
waitForLine c expected = do
  expectLine_ c $ \s ->
    trace ("Wait for \"" <> expected <> "\", got: " <> s) $
      if s == expected
        then Stop (Right ())
        else Continue

{- | Read lines from a channel until a specific line is encountered.
Do not use on Handles that might return non-ASCII characters.
-}
readUntilLine :: Chan ByteString -> String -> IO [String]
readUntilLine h = readUntilLineWith h readChan
