-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE ForeignFunctionInterface #-}

{- | A minimal native serial-port interface built directly on top of
@System.Posix.Terminal@. Unlike the @serialport@ package on Hackage (whose
'CommSpeed' enumeration caps at 115200 baud), this module takes a raw
'BaudRate', which - thanks to the @Num@ instance @unix@ provides on 'BaudRate' -
allows arbitrary speeds such as 921600.

The port is opened in fully raw mode (no echo, no canonical processing, no
input/output mapping) and exposed as a standard 'Handle', so that line-oriented
helpers such as 'System.IO.hGetLine' / 'System.IO.hPutStrLn' and the utilities in
"Project.Handle" / "Project.Chan" work directly on it.

Adapted from @serialport@'s POSIX backend
(<https://hackage.haskell.org/package/serialport>).
-}
module System.Hardware.Serial (
  Settings (..),
  BaudRate,
  openSerial,
) where

import Prelude

import Control.Exception (onException)
import Foreign (Ptr, castPtr, nullPtr)
import Foreign.C (CInt (..), throwErrnoIfMinus1_)
import GHC.IO.Handle (Handle, hSetBuffering, BufferMode (NoBuffering))
import System.Posix.IO (
  OpenMode (ReadWrite),
  closeFd,
  defaultFileFlags,
  fdToHandle,
  noctty,
  nonBlock,
  openFd,
  setFdOption,
  FdOption (NonBlockingRead),
 )
import System.Posix.Terminal (
  BaudRate,
  TerminalAttributes,
  TerminalMode (
    EnableEcho,
    EchoErase,
    EchoKill,
    EchoLF,
    ExtendedFunctions,
    HangupOnClose,
    KeyboardInterrupts,
    LocalMode,
    MapCRtoLF,
    ProcessInput,
    ProcessOutput,
    ReadEnable,
    StartStopInput,
    StartStopOutput,
    TwoStopBits
  ),
  TerminalState (Immediately),
  getTerminalAttributes,
  setTerminalAttributes,
  withBits,
  withInputSpeed,
  withMinInput,
  withMode,
  withOutputSpeed,
  withTime,
  withoutMode,
 )
import System.Posix.Types (Fd)

-- | Configuration for a serial port.
newtype Settings = Settings
  { baudRate :: BaudRate
  -- ^ Baud rate, e.g. @921600@.
  }

{- | Open a serial device (e.g. a @\/dev\/serial\/by-id\/...@ path) in raw mode at
the given baud rate and return it as a bidirectional 'Handle' (in 'NoBuffering').
-}
openSerial :: FilePath -> Settings -> IO Handle
openSerial dev settings = do
  fd <- openFd dev ReadWrite defaultFileFlags{noctty = True, nonBlock = True}
  -- Close the raw fd if configuration fails; ownership only passes to the
  -- 'Handle' once 'fdToHandle' succeeds.
  flip onException (closeFd fd) $ do
    setTIOCEXCL fd
    setFdOption fd NonBlockingRead False
    termOpts <- getTerminalAttributes fd
    setTerminalAttributes fd (configure termOpts settings) Immediately
  h <- fdToHandle fd
  hSetBuffering h NoBuffering
  pure h

-- | Configure terminal attributes for raw 8N1 communication at the given baud rate.
configure :: TerminalAttributes -> Settings -> TerminalAttributes
configure termOpts settings =
  termOpts
    `withInputSpeed` settings.baudRate
    `withOutputSpeed` settings.baudRate
    `withBits` 8
    `withoutMode` TwoStopBits -- one stop bit
    `withoutMode` StartStopInput -- no software flow control
    `withoutMode` StartStopOutput
    `withoutMode` EnableEcho
    `withoutMode` EchoErase
    `withoutMode` EchoKill
    `withoutMode` ProcessInput
    `withoutMode` ProcessOutput
    `withoutMode` MapCRtoLF
    `withoutMode` EchoLF
    `withoutMode` HangupOnClose
    `withoutMode` KeyboardInterrupts
    `withoutMode` ExtendedFunctions
    `withMode` LocalMode
    `withMode` ReadEnable
    -- VMIN=1, VTIME=0: a read blocks until at least one byte. With VMIN=0 a
    -- read can return 0 bytes, which the 'Handle' layer reports as EOF. Read
    -- timeouts are enforced at the call site via 'System.Timeout'.
    `withTime` 0
    `withMinInput` 1

#include <sys/ioctl.h>

foreign import ccall "ioctl" c_ioctl :: CInt -> CInt -> Ptr () -> IO CInt

cIoctl' :: Fd -> Int -> Ptr d -> IO ()
cIoctl' f req =
  throwErrnoIfMinus1_ "ioctl"
    . c_ioctl (fromIntegral f) (fromIntegral req)
    . castPtr

-- | Request exclusive use of the terminal (TIOCEXCL), so other processes
-- (e.g. a stray @picocom@) cannot open the same device concurrently.
setTIOCEXCL :: Fd -> IO ()
setTIOCEXCL fd = cIoctl' fd #{const TIOCEXCL} nullPtr
