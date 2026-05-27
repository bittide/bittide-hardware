-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE PackageImports #-}

{- | Utilities for poking at USB devices on the HITL rig.

The JTAG adapters (FT2232C) occasionally get their MPSSE engine stuck, after which OpenOCD
hangs in @mpsse_flush()@ and never finishes initializing. The device stays fully enumerated
at the kernel level, so only a USB-level device reset (equivalent to an unplug/replug, or
a reboot) clears it. This module offers tools to issue such a reset by location, so the
HITL setup can recover without operator intervention. See 'resetUsbDeviceByLocation'.
-}
module Bittide.Instances.Hitl.Utils.Usb (
  resetUsbDeviceByLocation,
) where

import Prelude

import Control.Exception (finally)
import Data.String.Interpolate (i)
import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.Types (CInt (..), CULong (..))
import System.Posix.IO (OpenMode (WriteOnly), closeFd, defaultFileFlags, openFd)
import System.Posix.Types (Fd (..))
import Text.Printf (printf)
import Text.Read (readMaybe)
import "extra" Data.List.Extra (trim)

foreign import ccall unsafe "ioctl"
  c_ioctl :: CInt -> CULong -> CInt -> IO CInt

{- | @USBDEVFS_RESET@ from @<linux/usbdevice_fs.h>@, defined as @_IO('U', 20)@.
On Linux @_IO(type, nr) = (type << 8) | nr@, so this is @(0x55 << 8) | 20@.
-}
usbdevfsReset :: CULong
usbdevfsReset = 0x5514

{- | Reset the USB device at the given adapter location (e.g. @"1-5.4.1:1"@, as
stored in 'Bittide.Hitl.DeviceInfo'). This is the same operation as @usbreset@
or physically replugging the device, and is used to recover JTAG adapters whose
FTDI MPSSE engine has wedged.

Throws if the location can't be resolved or the reset fails (e.g. the device
node isn't present, or we lack permission). A failed reset means we'd be about
to start OpenOCD against an adapter we couldn't recover, so we fail loudly here
rather than letting it manifest as an opaque OpenOCD timeout later.
-}
resetUsbDeviceByLocation :: String -> IO ()
resetUsbDeviceByLocation location = do
  busnum <- readIntFile (sysfsDir <> "/busnum")
  devnum <- readIntFile (sysfsDir <> "/devnum")
  let devNode = printf "/dev/bus/usb/%03d/%03d" busnum devnum :: String
  putStrLn [i|Resetting USB device #{devNode} (location #{location})|]
  fd <- openFd devNode WriteOnly defaultFileFlags
  let Fd cfd = fd
  throwErrnoIfMinus1_ "USBDEVFS_RESET" (c_ioctl cfd usbdevfsReset 0)
    `finally` closeFd fd
 where
  -- The adapter location is "<sysfs-device>:<config>", e.g. "1-5.4.1:1". The
  -- sysfs device directory we need is the part before the colon.
  sysfsName = takeWhile (/= ':') location
  sysfsDir = "/sys/bus/usb/devices/" <> sysfsName

  readIntFile path = do
    contents <- readFile path
    case readMaybe (trim contents) of
      Just n -> pure (n :: Int)
      Nothing ->
        ioError . userError $
          [i|Could not parse integer from #{path}: #{show contents}|]
