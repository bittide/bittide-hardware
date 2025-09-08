-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE PackageImports #-}

module Gdb.Internal where

import Prelude

import Control.Monad (unless, void)
import Control.Monad.Catch (MonadMask, finally)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Maybe (fromJust)
import Data.String.Interpolate (i)
import GHC.Stack (HasCallStack)
import Project.Handle (readUntil)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.IO (BufferMode (LineBuffering), Handle, hPutStrLn)
import System.IO.Extra (hSetBuffering)
import System.Process (
  CreateProcess (std_err, std_in, std_out),
  ProcessHandle,
  StdStream (CreatePipe),
  createProcess,
  proc,
  waitForProcess,
 )
import System.Process.Extra (cleanupProcess)
import qualified System.Timeout.Extra as T
import "bittide-extra" Control.Exception.Extra (brackets, preferMainBracket)

import qualified Data.List as L
import qualified "extra" Data.List.Extra as L

data Gdb = Gdb
  { stdin :: !Handle
  , stdout :: !Handle
  , stderr :: !Handle
  , process :: !ProcessHandle
  }

{- | Starts GDB and performs an action. At the end, whether an exception occurred
or not, it will terminate the GDB process. If it cannot terminate the process
it will throw an exception. If both the main action and cleanup thrown an
exception, the main action's exception will be thrown.
-}
withGdb :: (MonadIO m, MonadMask m, HasCallStack) => (Gdb -> m a) -> m a
withGdb = preferMainBracket start stop

-- | Like 'withGdb', but spawns multiple processes
withGdbs :: (MonadIO m, MonadMask m, HasCallStack) => Int -> ([Gdb] -> m a) -> m a
withGdbs n = brackets (L.replicate n start) stop

{- | Start a GDB process and pipe its stdin/stdout/stderr. Note that it is always
preferable to use one of 'withGdb' / 'withGdbs'.
-}
start :: (HasCallStack, MonadIO m) => m Gdb
start = liftIO $ do
  (gdbStdin, gdbStdout, gdbStderr, gdbPh) <-
    createProcess $
      (proc "gdb" [])
        { std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        }

  let
    gdb =
      Gdb
        { stdin = fromJust gdbStdin
        , stdout = fromJust gdbStdout
        , stderr = fromJust gdbStderr
        , process = gdbPh
        }

  hSetBuffering gdb.stdin LineBuffering
  hSetBuffering gdb.stdout LineBuffering

  pure gdb

{- | Clean up a GDB process. GDB is asked nicely to quit and waited for. If it
doesn't within 15 seconds, an exception is thrown. Note that this means the
process might still be running.
-}
stop :: (HasCallStack, MonadIO m) => Gdb -> m ()
stop gdb = liftIO $ do
  -- TODO: If GDB doesn't quit after asking nicely, kill it with force.
  cleanupProcess (Just gdb.stdin, Just gdb.stdout, Just gdb.stderr, gdb.process)
  exitCode <-
    T.tryWithTimeout T.NoPrintActionTime "Stop GDB" 10_000_000 (waitForProcess gdb.process)
  case exitCode of
    ExitSuccess -> return ()
    ExitFailure exitCode_ -> error [i|GDB exited with failure code #{exitCode_}|]

{- | Magic string that we'll instruct GDB to echo back to us to signal that
it has finished processing a command.
-}
magic :: String
magic = "8716da0aaa27b0074dd57b25287e7e4f952422378c65b809f1fa3e14056d62e64ebb"

{- | Read a command from GDB and return the raw output. The output is not
processed in any way, so it may contain leading and trailing whitespace.

Note that this function assumes only a single command is sent to GDB. If
multiple commands are sent, the output will contain REPL prompts such as
@(gdb)@.
-}
readCommandRaw :: (HasCallStack, MonadIO m) => Gdb -> String -> m String
readCommandRaw gdb cmd = liftIO $ do
  hPutStrLn gdb.stdin [i|echo \\n> #{cmd}\\n|]
  hPutStrLn gdb.stdin [i|echo #{magic}|]
  hPutStrLn gdb.stdin (L.trimEnd cmd)
  hPutStrLn gdb.stdin [i|echo #{magic}|]

  void $
    T.tryWithTimeout T.NoPrintActionTime ("Wait for magic (start)") 15_000_000 $
      readUntil gdb.stdout [i|#{magic}(gdb) |]

  T.tryWithTimeout T.NoPrintActionTime ("Wait for magic (end)") 15_000_000 $
    readUntil gdb.stdout [i|(gdb) #{magic}|]

{- | Read a command from GDB and return the output as a list of lines. Each line
is stripped of trailing whitespace (see 'Data.Char.isSpace'). Any leading and
trailing empty lines are removed. See 'readCommandRaw' if you need access to
the raw output.
-}
readCommand :: (HasCallStack, MonadIO m) => Gdb -> String -> m [String]
readCommand gdb cmd = do
  output <- readCommandRaw gdb cmd
  pure
    . L.dropWhile (== "")
    . L.dropWhileEnd (== "")
    . L.map L.trimEnd
    . L.lines
    $ output

-- | Execute a command in GDB and read its output. Errors if there is output.
readCommand0 :: (HasCallStack, MonadIO m) => Gdb -> String -> m ()
readCommand0 gdb cmd = do
  result <- readCommand gdb cmd
  case result of
    [] -> return ()
    ls -> error [i|Command '#{cmd}' returned one or more lines, expected no output: #{ls}|]

{- | Read a single line of output from GDB after executing a command. Errors if
the output is not exactly one line.
-}
readCommand1 :: (HasCallStack, MonadIO m) => Gdb -> String -> m String
readCommand1 gdb cmd = do
  result <- readCommand gdb cmd
  case result of
    [] -> error "GDB returned no output, expected at least one line"
    [l] -> return l
    ls -> error [i|Command '#{cmd}' returned multiple lines, expected one: #{ls}|]

-- | Like 'readCommand', but ignore output
runCommand :: (HasCallStack, MonadIO m) => Gdb -> String -> m ()
runCommand gdb cmd = void (readCommandRaw gdb cmd)

-- | Like 'runCommand', but takes a list of commands and runs them sequentially.
runCommands :: (HasCallStack, MonadIO m) => Gdb -> [String] -> m ()
runCommands gdb commands = mapM_ (runCommand gdb) commands

{- | Parse the output of 'show language' to extract the current language.

>>> parseShowLanguage "The current source language is \"c\"."
Just "c"
>>> parseShowLanguage "The current source language is \"auto; currently c\"."
Just "auto"
-}
parseShowLanguage :: String -> Maybe String
parseShowLanguage s0 = do
  s1 <- L.stripPrefix [i|The current source language is "|] s0
  s2 <- L.stripSuffix [i|".|] s1
  Just (if L.isPrefixOf "auto; currently " s2 then "auto" else s2)

-- | Detect the language of the current GDB session
getLanguage :: (HasCallStack, MonadIO m) => Gdb -> m String
getLanguage gdb = do
  output <- readCommand1 gdb "show language"
  case parseShowLanguage output of
    Nothing -> error [i|Could not parse GDB language output: #{output}|]
    Just lang -> return lang

-- | Set the language of the current GDB session
setLanguage :: (HasCallStack, MonadIO m) => Gdb -> String -> m ()
setLanguage gdb lang = runCommand gdb [i|set language #{lang}|]

{- | Run an action in a specific language context. This will set the language
for the duration of the action, and restore it afterwards. This can be useful
for running language-specific commands, without having to implement it for every
language.
-}
withLanguage :: (HasCallStack, MonadIO m, MonadMask m) => Gdb -> String -> m a -> m a
withLanguage gdb lang action = do
  restoreLang <- getLanguage gdb

  unless (restoreLang == lang) $
    setLanguage gdb lang

  finally action $
    unless (restoreLang == lang) $
      setLanguage gdb restoreLang
