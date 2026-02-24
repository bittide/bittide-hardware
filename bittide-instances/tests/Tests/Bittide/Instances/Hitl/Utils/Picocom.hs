-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Tests.Bittide.Instances.Hitl.Utils.Picocom where

import Prelude

import Test.Tasty
import Test.Tasty.HUnit

import Bittide.Instances.Hitl.Utils.Picocom
import Control.Concurrent (threadDelay)
import Data.Maybe (fromJust)
import GHC.IO.Handle (Handle)
import System.Directory (getFileSize, removeFile)
import System.Process

-- | Spawn a process that will output over 1MB of data to stdout via tee
createVerboseProcess :: String -> IO (Handle, IO ())
createVerboseProcess outputLog = do
  let process =
        ( shell $
            "max=100000; i=0; while [ $i -lt $max ]; do echo \"line $i\"; i=$((i+1)); done | tee " <> outputLog
        )
  (_stdin, stdout, _stderr, pHandle) <- createProcess (process{std_out = CreatePipe})

  pure (fromJust stdout, terminateProcess pHandle >> removeFile outputLog)

-- | Test that writing 1MB of data to a file handle that is not read from will block tee
testWritingToFileHandle :: Assertion
testWritingToFileHandle = do
  let outputLog = "outputNonChan.log"

  (_stdout, pCleanup) <- createVerboseProcess outputLog

  -- Never read chan output
  threadDelay 3_000_000
  logSize <- getFileSize outputLog
  assertBool "File handler log size is well below 1MB" (logSize < 66440)

  -- Clean up
  pCleanup
  pure ()

-- | Test that writing 1MB of data to a channel that is not read from will NOT block tee
testWritingToChan :: Assertion
testWritingToChan = do
  let outputLog = "outputChan.log"

  (stdout, pCleanup) <- createVerboseProcess outputLog
  (_chan, chanCleanup) <- handleToChan stdout

  -- Never read chan output
  threadDelay 3_000_000
  logSize <- getFileSize outputLog
  assertBool "Channel log size is above 1MB" (1000000 < logSize)

  lastLine <- last <$> lines <$> readFile outputLog
  assertEqual "Last line is still captured" "line 99999" lastLine

  -- Clean up
  chanCleanup >> pCleanup
  pure ()

tests :: TestTree
tests =
  testGroup
    "Picocom tests"
    [ testCase
        "Ensure that a process tee-ing to a channel will not block"
        testWritingToChan
    , testCase
        "Check that a process tee-int to a file handle will block"
        testWritingToFileHandle
    ]
