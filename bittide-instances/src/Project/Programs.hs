-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Project.Programs where

import Prelude

import Control.Monad (unless)
import Control.Monad.Extra (forM_)
import Data.List.Extra (isPrefixOf, trim)
import Paths.Bittide.Instances
import System.IO
import System.IO.Temp

getOpenOcdStartPath :: IO FilePath
getOpenOcdStartPath = getDataFileName "data/openocd/start.sh"

getPicocomStartPath :: IO FilePath
getPicocomStartPath = getDataFileName "data/picocom/start.sh"

getTcpSprayPath :: IO FilePath
getTcpSprayPath = getDataFileName "data/tcpspray/start.sh"

{- | XXX: Currently hardcoded to a very specific position. Maybe we could probe
       using JTAG to see what device we're connected to?
-}
getUartDev :: IO String
getUartDev = pure "/dev/serial/by-path/pci-0000:00:14.0-usb-0:5.1:1.1-port0"

{- | Take a GDB script, create copy that echoes everything it's doing, and give its path to action

This works by creating a temporary copy with @echo > {line}\n@ prepended to each non-comment, non-empty line.
This effectively emulates Bash's @set -x@ for the GDB script.
And can be used to wait for specific commands to be executed, or simply for debugging.

After the action returns the generated file gets deleted automatically.
-}
withAnnotatedGdbScriptPath :: FilePath -> (FilePath -> IO ()) -> IO ()
withAnnotatedGdbScriptPath srcPath action = do
  withSystemTempFile "gdb-script" $ \dstPath dstHandle -> do
    withFile srcPath ReadMode $ \srcHandle -> do
      srcLines <- lines <$> hGetContents srcHandle
      forM_ srcLines $ \line -> do
        let trimmedLine = trim line
        unless
          (null trimmedLine || "#" `isPrefixOf` trimmedLine)
          ( hPutStr dstHandle "echo > "
              >> hPutStr dstHandle line
              >> hPutStrLn dstHandle "\\n"
          )
        hPutStrLn dstHandle line

    hClose dstHandle
    action dstPath
