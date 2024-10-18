-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Project.Programs where

import Prelude

import Control.Monad (unless)
import Control.Monad.Extra (forM_)
import Data.List.Extra (isPrefixOf, trim)
import Data.Maybe (fromJust)
import Paths.Bittide.Instances
import System.IO
import System.IO.Temp
import System.Process

import Project.Handle

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

-- | Wait until we see "Halting processor", fail if we see an error.
waitForHalt :: String -> Filter
waitForHalt s
  | "Error:" `isPrefixOf` s = Stop (Error ("Found error in OpenOCD output: " <> s))
  | "Halting processor" `isPrefixOf` s = Stop Ok
  | otherwise = Continue

{- | Open the configurations for OpenOCD. Replace the adapter usb location with
the given location.
-}
concatOpenOcdConfigs :: String -> IO String
concatOpenOcdConfigs usbLoc = do
  ports <- getDataFileName "data/openocd/ports.tcl"
  sipeed <- getDataFileName "data/openocd/sipeed.tcl"
  vexriscv <- getDataFileName "data/openocd/vexriscv_init.tcl"

  portsLines <- lines <$> readFile ports
  sipeedLines <- lines <$> readFile sipeed
  vexriscvLines <- lines <$> readFile vexriscv
  let adapterLine = ["adapter usb location " <> usbLoc]

  let allLines = portsLines <> adapterLine <> drop 6 sipeedLines <> vexriscvLines
  return (unlines allLines)

-- | Start OpenOCD with a connection to a specific USB adapter location.
withOpenOcdProc :: String -> (FilePath -> IO ()) -> IO ()
withOpenOcdProc usbLoc action = do
  withSystemTempFile "openocd-script.tcl" $ \dstPath dstHandle -> do
    openOcdScript <- concatOpenOcdConfigs usbLoc
    hPutStrLn dstHandle openOcdScript
    hClose dstHandle

    let openOcdProc = (proc "openocd-vexriscv" ["-f", dstPath]){std_err = CreatePipe}
    withCreateProcess openOcdProc $ \_ _ (fromJust -> openOcdStdErr) _ -> do
      hSetBuffering openOcdStdErr LineBuffering
      putStrLn "Waiting on processor to halt..."
      expectLine openOcdStdErr waitForHalt
      putStrLn "Processor halted, continuing"

      action dstPath

testConcatOpenOcdConfigs :: IO ()
testConcatOpenOcdConfigs = do
  withFile "concatOcdConfig.tcl" WriteMode $ \dstHandle -> do
    s2 <- concatOpenOcdConfigs "1-5-2:1"
    hPutStr dstHandle s2
