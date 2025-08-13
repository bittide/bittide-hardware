-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}

-- | Commands for reading data from memory addresses
module Gdb.Commands.Read (
  readBytes,
  readHalfWords,
  readWords,
  readGiants,
  readByte,
  readHalfWord,
  readWord,
  readGiant,
  readLe,
  readBe,
  read,
) where

import Clash.Prelude hiding (read)

import Clash.Class.BitPackC (
  BitPackC,
  ByteOrder (BigEndian, LittleEndian),
  Bytes,
  unpackOrErrorC,
 )
import Data.String.Interpolate (i)
import Data.Typeable (Typeable)
import GHC.Stack (HasCallStack)
import Gdb.Internal (Gdb, readCommand)
import Numeric (showHex)
import Text.Read (readMaybe)

import qualified Clash.Sized.Vector as Vec
import qualified Data.List as L

-- | Parse a single line of an examine memory output
parseExamineMemoryOutput :: forall n. (KnownNat n) => String -> Either String [Bytes n]
parseExamineMemoryOutput =
  traverse parseHex -- parse hexadecimal numbers to Integer
    . L.drop 1 -- drop the address
    . words -- split on tabs
 where
  integerToBytes :: Integer -> Either String (Bytes n)
  integerToBytes x
    | x > fromIntegral (maxBound :: Bytes n) =
        Left [i|Value exceeds maximum for Bytes #{natToInteger @n}: #{show x}|]
    | otherwise = Right (fromIntegral x)

  parseHex :: String -> Either String (Bytes n)
  parseHex s =
    case readMaybe s of
      Just x -> integerToBytes x
      Nothing -> Left [i|Could not parse hex value: #{s}|]

{- | Like 'parseExamineMemoryOutput', but parses multiple lines of output and
returns a vector of the parsed values.
-}
parseExamineMemoryOutputs ::
  forall m n.
  (KnownNat m, KnownNat n) =>
  [String] ->
  Either String (Vec m (Bytes n))
parseExamineMemoryOutputs output = do
  case traverse parseExamineMemoryOutput output of
    Left err ->
      Left
        [i|
      Error while parsing examine memory output:

        #{output}

      An error occurred while parsing a hexadecimal value:

        #{err}
    |]
    Right parsed ->
      case Vec.fromList (L.concat parsed) of
        Nothing ->
          Left
            [i|Unexpected number of items parsed: #{parsed}. Expected #{natToInteger @m}. Parsing: #{output}.|]
        Just vec ->
          Right vec

{- | Examine memory at the given address using the specified format. Currently,
GDB supports 'b' (byte), 'h' (half-word), 'w' (word), and 'g' (giant).
-}
examineMemory ::
  forall n m.
  (HasCallStack, KnownNat n, KnownNat m) =>
  Char ->
  Gdb ->
  Integer ->
  IO (Vec n (Bytes m))
examineMemory format gdb address = do
  results <- readCommand gdb [i|x/#{natToInteger @n}#{format}x 0x#{showHex address ""}|]
  pure $ either error id (parseExamineMemoryOutputs results)

-- | Read multiple /byte/s from the GDB process at the given address
readBytes ::
  forall n.
  (HasCallStack, KnownNat n) =>
  Gdb ->
  Integer ->
  IO (Vec n (Bytes 1))
readBytes = examineMemory 'b'

-- | Read multiple /half-word/s (2 bytes) from the GDB process at the given address
readHalfWords ::
  forall n.
  (HasCallStack, KnownNat n) =>
  Gdb ->
  Integer ->
  IO (Vec n (Bytes 2))
readHalfWords = examineMemory 'h'

-- | Read multiple /word/s (4 bytes) from the GDB process at the given address
readWords ::
  forall n.
  (HasCallStack, KnownNat n) =>
  Gdb ->
  Integer ->
  IO (Vec n (Bytes 4))
readWords = examineMemory 'w'

-- | Read multiple /giant/s (8 bytes) from the GDB process at the given address
readGiants ::
  forall n.
  (HasCallStack, KnownNat n) =>
  Gdb ->
  Integer ->
  IO (Vec n (Bytes 8))
readGiants = examineMemory 'g'

-- | Read a single /byte/ from the GDB process at the given address
readByte :: (HasCallStack) => Gdb -> Integer -> IO (Bytes 1)
readByte gdb address = head <$> readBytes @1 gdb address

-- | Read a single /half-word/ (2 bytes) from the GDB process at the given address
readHalfWord :: (HasCallStack) => Gdb -> Integer -> IO (Bytes 2)
readHalfWord gdb address = head <$> readHalfWords @1 gdb address

-- | Read a single /word/ (4 bytes) from the GDB process at the given address
readWord :: (HasCallStack) => Gdb -> Integer -> IO (Bytes 4)
readWord gdb address = head <$> readWords @1 gdb address

-- | Read a single /giant/ (8 bytes) from the GDB process at the given address
readGiant :: (HasCallStack) => Gdb -> Integer -> IO (Bytes 8)
readGiant gdb address = head <$> readGiants @1 gdb address

{- | Read an arbitrary type from the GDB process at the given address. The type
must be laid out in memory according to C FFI rules. E.g., in Rust types must
be annotated with @#[repr(C)]@.
-}
read ::
  forall a.
  (HasCallStack, BitPackC a, Typeable a, NFDataX a) =>
  ByteOrder ->
  Gdb ->
  Integer ->
  IO a
read byteOrder gdb address = do
  bytes <- readBytes gdb address
  pure $ unpackOrErrorC byteOrder bytes

-- | Like 'read', but sets the byte order to 'LittleEndian'
readLe ::
  (HasCallStack, BitPackC a, Typeable a, NFDataX a) =>
  Gdb ->
  Integer ->
  IO a
readLe = read LittleEndian

-- | Like 'read', but sets the byte order to 'BigEndian'
readBe ::
  (HasCallStack, BitPackC a, Typeable a, NFDataX a) =>
  Gdb ->
  Integer ->
  IO a
readBe = read BigEndian
