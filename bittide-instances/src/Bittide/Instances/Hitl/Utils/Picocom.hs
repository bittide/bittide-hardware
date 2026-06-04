-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{- | Serial communication with FPGAs for HITL tests.

Historically this module wrapped the external @picocom@ program; it now talks to
the serial device directly via "System.Hardware.Serial". The module (and most of
its API) keeps the @Picocom@ name so the call sites need minimal changes.

Two styles are offered:

  * A raw 'SerialHandle' ('start', 'withSerial') for drivers that read and write
    the port directly.
  * A 'Chan' 'ByteString' of received lines ('initPicocom',
    'startWithLoggingChan') for drivers that only consume output. In this style,
    every received line is also appended to a log file - this replaces
    @picocom@'s @tee@ and is the primary HITL debug artifact.
-}
module Bittide.Instances.Hitl.Utils.Picocom where

import Prelude

import Bittide.Hitl (DeviceInfo (..))
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.Chan
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.ByteString (ByteString, empty)
import Data.ByteString.Char8 (hGetLine)
import GHC.IO.Exception
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))
import System.Hardware.Serial (BaudRate)
import System.IO (IOMode (AppendMode), hClose, hFlush, openFile)
import System.IO.Error (isDoesNotExistError)
import Vivado.Tcl (HwTarget)

import GHC.IO.Handle (BufferMode (..), Handle, hIsEOF, hSetBuffering)

import qualified Data.ByteString.Char8 as BSC
import qualified System.Hardware.Serial as Serial

-- | Default baud rate used by most instances.
defaultBaud :: BaudRate
defaultBaud = 921600

-- | A serial connection to a device, exposed as a bidirectional 'Handle'.
newtype SerialHandle = SerialHandle
  { handle :: Handle
  }

{- | Open a serial connection to a device and stream every received line to a
channel and a log file.

Returns the channel and a cleanup action.
-}
initPicocom :: FilePath -> (HwTarget, DeviceInfo) -> Int -> IO (Chan ByteString, IO ())
initPicocom hitlDir (_hwTarget, deviceInfo) targetIndex = do
  let
    devPath = deviceInfo.serial
    stdoutPath = hitlDir </> "picocom-" <> show targetIndex <> "-stdout.log"

  startWithLoggingChan devPath defaultBaud stdoutPath

{- | Open a serial connection at the given baud rate. Returns the handle and a
cleanup action that closes it.

Callers typically reset the USB adapter right before opening (see
'Bittide.Instances.Hitl.Utils.Usb'), during which the device node briefly
disappears as it re-enumerates. We therefore retry the open for a few seconds
until the node reappears.
-}
start :: FilePath -> BaudRate -> IO (SerialHandle, IO ())
start devPath baud = do
  h <- openWithRetry (50 :: Int) -- ~5 s total at 100 ms per attempt
  pure (SerialHandle h, hClose h)
 where
  openWithRetry n =
    Serial.openSerial devPath (Serial.Settings baud) `catch` \(e :: IOException) ->
      if n > 0 && isDoesNotExistError e
        then threadDelay 100_000 >> openWithRetry (n - 1)
        else throwM e

{- | Open a serial connection and stream every received line to a 'Chan' and a
log file.
-}
startWithLoggingChan :: FilePath -> BaudRate -> FilePath -> IO (Chan ByteString, IO ())
startWithLoggingChan devPath baud logPath = do
  (sh, cleanup) <- start devPath baud
  (chan, cleanupChan) <- handleToChan sh.handle (Just logPath)
  pure (chan, cleanupChan >> cleanup)

{- | Open a serial connection, run an action with the handle, then clean up.

The caller reads and writes the handle directly; output logging for these
drivers is handled by the test harness (e.g. dumping remaining output on
failure), not by a separate reader thread - a reader thread would steal the very
bytes the driver needs to read.
-}
withSerial ::
  (MonadIO m, MonadMask m) =>
  FilePath ->
  BaudRate ->
  (SerialHandle -> m a) ->
  m a
withSerial devPath baud action = do
  (sh, clean) <- liftIO $ start devPath baud
  finally (action sh) (liftIO clean)

{- | Starts a 'Chan' 'ByteString' from a given 'Handle'. The channel acts as a
buffer that prevents the handle from blocking on unread output. Bytestrings
output by the handle can then be read through the channel output. When a log
path is given, each line is also appended to that file.
-}
handleToChan :: Handle -> Maybe FilePath -> IO (Chan ByteString, IO ())
handleToChan h mLogPath = do
  c <- newChan
  hSetBuffering h LineBuffering
  mLogH <- traverse openLog mLogPath
  threadId <-
    forkIO $
      readHandle c mLogH `catch` \(e :: IOException) ->
        putStrLn $ "[handleToChan: " <> show h <> "] IOException: " <> show e
  let
    cleanup = do
      killThread threadId
      mapM_ hClose mLogH
  pure (c, cleanup)
 where
  openLog path = do
    createDirectoryIfMissing True (takeDirectory path)
    openFile path AppendMode

  readHandle chan mLogH = do
    eof <- hIsEOF h
    if eof
      then writeChan chan empty
      else do
        bytes <- hGetLine h
        writeChan chan bytes
        mapM_ (\logH -> BSC.hPutStrLn logH bytes >> hFlush logH) mLogH
        readHandle chan mLogH
