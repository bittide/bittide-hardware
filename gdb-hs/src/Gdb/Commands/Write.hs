-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Gdb.Commands.Write (
  writeBytes,
  write,
  writeLe,
  writeBe,
) where

import Clash.Prelude hiding (read)

import Clash.Class.BitPackC (
  BitPackC,
  ByteOrder (BigEndian, LittleEndian),
  Bytes,
  packC,
 )
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO)
import Data.String.Interpolate (i)
import Data.Typeable (Typeable)
import GHC.Stack (HasCallStack)
import Gdb.Internal (Gdb, runCommand, withLanguage)
import Numeric (showHex)

import qualified Clash.Sized.Vector as Vec
import qualified Data.List as L

-- | Write a vector of bytes to a given address in GDB
writeBytes ::
  forall n m.
  (HasCallStack, MonadIO m, MonadMask m, KnownNat n) =>
  Gdb ->
  Integer ->
  Vec n (Bytes 1) ->
  m ()
writeBytes gdb address bytes = do
  -- The syntax expected by GDB is language dependent. We sometimes run commands
  -- in a C context and sometimes in a Rust context, so we set the language to
  -- a known one and use that to write the bytes.
  withLanguage gdb "c" $ do
    runCommand gdb [i|set {unsigned char[#{natToInteger @n}]}(#{addressHex}) = {#{bytesHex}}|]
 where
  toHex s = "0x" <> showHex s ""
  bytesHex = L.intercalate ", " (toHex . toInteger <$> Vec.toList bytes)
  addressHex = "0x" <> showHex address ""

{- | Write an arbitrary type to the GDB process at the given address. The type
must be laid out in memory according to C FFI rules. E.g., in Rust types must
be annotated with @#[repr(C)]@.
-}
write ::
  (HasCallStack, BitPackC a, Typeable a, NFDataX a, MonadIO m, MonadMask m) =>
  ByteOrder ->
  Gdb ->
  Integer ->
  a ->
  m ()
write byteOrder gdb address value = do
  let bytes = packC byteOrder value
  writeBytes gdb address bytes

-- | Like 'write', but uses little-endian byte order
writeLe ::
  (HasCallStack, BitPackC a, Typeable a, NFDataX a, MonadIO m, MonadMask m) =>
  Gdb ->
  Integer ->
  a ->
  m ()
writeLe = write LittleEndian

-- | Like 'write', but uses big-endian byte order
writeBe ::
  (HasCallStack, BitPackC a, Typeable a, NFDataX a, MonadIO m, MonadMask m) =>
  Gdb ->
  Integer ->
  a ->
  m ()
writeBe = write BigEndian
