-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoFieldSelectors #-}

module Bittide.Instances.Tests.RegisterWbC where

import Clash.Prelude

-- Local
import Bittide.DoubleBufferedRam (
  ContentType (Vec),
  InitialContent (Reloadable),
 )
import Bittide.Instances.Domains (Basic50)
import Bittide.ProcessingElement (
  PeConfig (
    PeConfig,
    dBusTimeout,
    iBusTimeout,
    includeIlaWb,
    initD,
    initI,
    prefixD,
    prefixI,
    whoAmID,
    whoAmIPfx
  ),
  processingElement,
 )
import Bittide.ProcessingElement.Util (
  vecFromElfData,
  vecFromElfInstr,
 )
import Bittide.SharedTypes (Bytes)
import Bittide.Wishbone (uartInterfaceWb, uartSim)
import Project.FilePath (
  CargoBuildType (Release),
  TargetArch (RiscV),
  findParentContaining,
  firmwareBinariesDir,
 )

import Clash.Class.BitPackC (BitPackC, ByteOrder (BigEndian, LittleEndian))
import Data.Char (chr)
import Data.Maybe (catMaybes)
import Protocols (Circuit (Circuit), Df, Drivable (sampleC), idC, toSignals)
import Protocols.Idle (idleSource)
import Protocols.MemoryMap (ConstBwd, MM, MemoryMap, constBwd, getMMAny)
import Protocols.MemoryMap.FieldType (ToFieldType)
import Protocols.MemoryMap.Registers.WishboneStandard (
  deviceWbC,
  registerConfig,
  registerWbC_,
 )
import Protocols.Wishbone (Wishbone, WishboneMode (Standard))
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty.HUnit (HasCallStack)
import VexRiscv (DumpVcd (DumpVcd, NoDumpVcd))

type U8 = Unsigned 8
type U16 = Unsigned 16
type U32 = Unsigned 32
type U64 = Unsigned 64

type B8 = BitVector 8
type B16 = BitVector 16
type B32 = BitVector 32
type B64 = BitVector 64

type S8 = Signed 8
type S16 = Signed 16
type S32 = Signed 32
type S64 = Signed 64

data Abc = A | B | C
  deriving (Generic, NFDataX, ShowX, Show, ToFieldType, BitPackC, BitPack)

data Xyz = H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
  deriving (Generic, BitPackC, NFDataX, ShowX, Show, ToFieldType, BitPack)

data F = F {f :: Float, u :: Double}
  deriving (Generic, BitPackC, NFDataX, ShowX, Show, ToFieldType, BitPack)

data X3 = X3 B16 B8 B8
  deriving (Generic, BitPackC, NFDataX, ShowX, Show, ToFieldType, BitPack)

data X2 = X2 B8 X3
  deriving (Generic, BitPackC, NFDataX, ShowX, Show, ToFieldType, BitPack)

data P0 = P0 U16 U8 U8
  deriving (Generic, BitPackC, NFDataX, ShowX, Show, ToFieldType, BitPack)

data P1 = P1 U16 U8 U16
  deriving (Generic, BitPackC, NFDataX, ShowX, Show, ToFieldType, BitPack)

data P2 = P2 U16 U8 (Index 10)
  deriving (Generic, BitPackC, NFDataX, ShowX, Show, ToFieldType, BitPack)

data P3 = P3 P2 U8
  deriving (Generic, BitPackC, NFDataX, ShowX, Show, ToFieldType, BitPack)

data SoP
  = SoP0
  | SoP1 {u :: Unsigned 32}
  | SoP2
  deriving (Generic, NFDataX, ShowX, Show, ToFieldType, BitPackC, BitPack)

data Inner = Inner
  { innerA :: BitVector 8
  , innerB :: BitVector 16
  }
  deriving (Generic, NFDataX, ShowX, Show, ToFieldType, BitPackC, BitPack)

-- | Example that exports a bunch of different types.
manyTypesWb ::
  forall wordSize aw dom.
  ( HasCallStack
  , HiddenClock dom
  , HiddenReset dom
  , KnownNat wordSize
  , KnownNat aw
  , 1 <= wordSize
  , 4 <= aw
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
  Circuit
    (ConstBwd MM, Wishbone dom 'Standard aw (Bytes wordSize))
    ()
manyTypesWb = circuit $ \(mm, wb) -> do
  [ wbS0
    , wbS1
    , wbS2
    , wbS3
    , wbS4
    , wbU0
    , wbU1
    , wbU2
    , wbU3
    , wbBv0
    , wbBv1
    , wbBv2
    , wbF0
    , wbF1
    , wbD0
    , wbD1
    , wbB0
    , wbV0
    , wbV1
    , wbV2
    , wbSum0
    , wbSum1
    , wbSop0
    , wbSop1
    , wbE0
    , wbOI0
    , wbX2
    , wbMe0
    , wbMe1
    , wbP0
    , wbP1
    , wbP2
    , wbP3
    , wbT0
    , wbI20
    , wbMI12
    ] <-
    deviceWbC "ManyTypes" -< (mm, wb)

  registerWbC_ hasClock hasReset (registerConfig "s0") initWbS0 -< (wbS0, Fwd noWrite)
  registerWbC_ hasClock hasReset (registerConfig "s1") initWbS1 -< (wbS1, Fwd noWrite)
  registerWbC_ hasClock hasReset (registerConfig "s2") initWbS2 -< (wbS2, Fwd noWrite)
  registerWbC_ hasClock hasReset (registerConfig "s3") initWbS3 -< (wbS3, Fwd noWrite)
  registerWbC_ hasClock hasReset (registerConfig "s4") initWbS4 -< (wbS4, Fwd noWrite)

  registerWbC_ hasClock hasReset (registerConfig "u0") initWbU0 -< (wbU0, Fwd noWrite)
  registerWbC_ hasClock hasReset (registerConfig "u1") initWbU1 -< (wbU1, Fwd noWrite)
  registerWbC_ hasClock hasReset (registerConfig "u2") initWbU2 -< (wbU2, Fwd noWrite)
  registerWbC_ hasClock hasReset (registerConfig "u3") initWbU3 -< (wbU3, Fwd noWrite)

  registerWbC_ hasClock hasReset (registerConfig "bv0") initWbBv0 -< (wbBv0, Fwd noWrite)
  registerWbC_ hasClock hasReset (registerConfig "bv1") initWbBv1 -< (wbBv1, Fwd noWrite)
  registerWbC_ hasClock hasReset (registerConfig "bv2") initWbBv2 -< (wbBv2, Fwd noWrite)

  registerWbC_ hasClock hasReset (registerConfig "f0") initWbF0 -< (wbF0, Fwd noWrite)
  registerWbC_ hasClock hasReset (registerConfig "f1") initWbF1 -< (wbF1, Fwd noWrite)

  registerWbC_ hasClock hasReset (registerConfig "d0") initWbD0 -< (wbD0, Fwd noWrite)
  registerWbC_ hasClock hasReset (registerConfig "d1") initWbD1 -< (wbD1, Fwd noWrite)

  registerWbC_ hasClock hasReset (registerConfig "b0") initWbB0 -< (wbB0, Fwd noWrite)

  registerWbC_ hasClock hasReset (registerConfig "v0") initWbV0 -< (wbV0, Fwd noWrite)
  registerWbC_ hasClock hasReset (registerConfig "v1") initWbV1 -< (wbV1, Fwd noWrite)
  registerWbC_ hasClock hasReset (registerConfig "v2") initWbV2 -< (wbV2, Fwd noWrite)

  registerWbC_ hasClock hasReset (registerConfig "sum0") initSum0 -< (wbSum0, Fwd noWrite)
  registerWbC_ hasClock hasReset (registerConfig "sum1") initSum1 -< (wbSum1, Fwd noWrite)

  registerWbC_ hasClock hasReset (registerConfig "sop0") initSop0 -< (wbSop0, Fwd noWrite)
  registerWbC_ hasClock hasReset (registerConfig "sop1") initSop1 -< (wbSop1, Fwd noWrite)

  registerWbC_ hasClock hasReset (registerConfig "e0") initWbE0 -< (wbE0, Fwd noWrite)

  registerWbC_ hasClock hasReset (registerConfig "oi") initOI0 -< (wbOI0, Fwd noWrite)

  registerWbC_ hasClock hasReset (registerConfig "x2") initWbX2 -< (wbX2, Fwd noWrite)

  registerWbC_ hasClock hasReset (registerConfig "me0") initWbMe0 -< (wbMe0, Fwd noWrite)
  registerWbC_ hasClock hasReset (registerConfig "me1") initWbMe1 -< (wbMe1, Fwd noWrite)

  registerWbC_ hasClock hasReset (registerConfig "p0") initP0 -< (wbP0, Fwd noWrite)
  registerWbC_ hasClock hasReset (registerConfig "p1") initP1 -< (wbP1, Fwd noWrite)
  registerWbC_ hasClock hasReset (registerConfig "p2") initP2 -< (wbP2, Fwd noWrite)
  registerWbC_ hasClock hasReset (registerConfig "p3") initP3 -< (wbP3, Fwd noWrite)

  registerWbC_ hasClock hasReset (registerConfig "t0") initT0 -< (wbT0, Fwd noWrite)

  registerWbC_ hasClock hasReset (registerConfig "i20") initI20 -< (wbI20, Fwd noWrite)
  registerWbC_ hasClock hasReset (registerConfig "mi12") initMI12 -< (wbMI12, Fwd noWrite)

  idC
 where
  initWbS0 :: Signed 8
  initWbS0 = -8

  initWbS1 :: Signed 8
  initWbS1 = 8

  initWbS2 :: Signed 16
  initWbS2 = 16

  initWbS3 :: Signed 64
  initWbS3 = 3721049880298531338

  initWbS4 :: Signed 7
  initWbS4 = -12

  initWbU0 :: Unsigned 8
  initWbU0 = 8

  initWbU1 :: Unsigned 16
  initWbU1 = 16

  initWbU2 :: Unsigned 64
  initWbU2 = 3721049880298531338

  initWbU3 :: Unsigned 32
  initWbU3 = 0xBADC_0FEE

  initWbBv0 :: BitVector 8
  initWbBv0 = 8

  initWbBv1 :: BitVector 16
  initWbBv1 = 16

  initWbBv2 :: BitVector 64
  initWbBv2 = 3721049880298531338

  initWbF0 :: Float
  initWbF0 = -8

  initWbF1 :: Float
  initWbF1 = 8

  initWbD0 :: Double
  initWbD0 = -8

  initWbD1 :: Double
  initWbD1 = 8

  initWbB0 :: Bool
  initWbB0 = True

  initWbV0 :: Vec 8 (BitVector 8)
  initWbV0 = 0x8 :> 0x16 :> 0x24 :> 0x32 :> 0x40 :> 0x4E :> 0x5C :> 0x6A :> Nil

  initWbV1 :: Vec 3 (BitVector 64)
  initWbV1 = 0x8 :> 0x16 :> 3721049880298531338 :> Nil

  initWbV2 :: Vec 2 (Vec 2 (BitVector 8))
  initWbV2 = (0x8 :> 0x16 :> Nil) :> (0x24 :> 0x32 :> Nil) :> Nil

  noWrite :: forall a. Signal dom (Maybe a)
  noWrite = pure Nothing

  initSum0 :: Abc
  initSum0 = C

  initSum1 :: Xyz
  initSum1 = S

  initSop0 :: F
  initSop0 = F 3.14 6.28

  initSop1 :: SoP
  initSop1 = SoP1 0xBADC_0FEE

  initWbE0 :: Either (BitVector 8) (BitVector 16)
  initWbE0 = Left 8

  -- We purposely do not have a register for the `Inner` type, see issue
  -- https://github.com/bittide/bittide-hardware/issues/815.
  initOI0 :: Maybe Inner
  initOI0 = Just (Inner 0x16 0x24)

  initWbX2 :: X2
  initWbX2 = X2 8 (X3 16 32 64)

  initWbMe0 :: Maybe (Either (BitVector 8) (BitVector 16))
  initWbMe0 = Just (Left 8)

  initWbMe1 :: Maybe (Either (BitVector 16) (BitVector 8))
  initWbMe1 = Just (Left 8)

  initP0 :: P0
  initP0 = P0 0xBADC 0x0F 0xEE

  initP1 :: P1
  initP1 = P1 0xBADC 0x0F 0xBEAD

  initP2 :: P2
  initP2 = P2 0xBADC 0x0F 6

  initP3 :: P3
  initP3 = P3 (P2 0xBADC 0x0F 6) 0xEE

  initT0 :: (BitVector 8, Signed 16)
  initT0 = (12, 584)

  initI20 :: Index 20
  initI20 = 17

  initMI12 :: Maybe (Index 12)
  initMI12 = Just 5

sim :: IO ()
sim = putStrLn simResult

simResult :: String
simResult = chr . fromIntegral <$> catMaybes uartStream
 where
  uartStream = sampleC def dut0

  dut0 :: Circuit () (Df Basic50 (BitVector 8))
  dut0 = Circuit $ ((),) . snd . toSignals dut . ((),) . snd

{- | An instance connecting a vexriscv to a UART and a memory mapped devices that
has many (read/write) registers.
-}
dut :: Circuit (ConstBwd MM) (Df Basic50 (BitVector 8))
dut =
  let
    ?busByteOrder = BigEndian
    ?regByteOrder = LittleEndian
   in
    withClockResetEnable clockGen resetGen enableGen
      $ circuit
      $ \mm -> do
        (uartRx, jtag) <- idleSource
        [(prefixUart, (mmUart, uartBus)), (prefixManyTypes, manyTypes)] <-
          processingElement dumpVcd peConfig -< (mm, jtag)
        (uartTx, _uartStatus) <- uartInterfaceWb d2 d2 uartSim -< (mmUart, (uartBus, uartRx))
        constBwd 0b000 -< prefixUart
        manyTypesWb -< manyTypes
        constBwd 0b110 -< prefixManyTypes
        idC -< uartTx
 where
  dumpVcd =
    unsafePerformIO $ do
      mVal <- lookupEnv "REGISTERWBC_DUMP_VCD"
      case mVal of
        Just s -> pure (DumpVcd s)
        _ -> pure NoDumpVcd

  peConfig = unsafePerformIO $ do
    root <- findParentContaining "cabal.project"
    let elfPath = root </> firmwareBinariesDir RiscV Release </> "registerwbc_test"
    pure
      PeConfig
        { initI =
            Reloadable @IMemWords
              $ Vec
              $ unsafePerformIO
              $ vecFromElfInstr BigEndian elfPath
        , prefixI = 0b100
        , initD =
            Reloadable @DMemWords
              $ Vec
              $ unsafePerformIO
              $ vecFromElfData BigEndian elfPath
        , prefixD = 0b010
        , iBusTimeout = d0 -- No timeouts on the instruction bus
        , dBusTimeout = d0 -- No timeouts on the data bus
        , includeIlaWb = False
        , whoAmID = 0x3075_7063
        , whoAmIPfx = 0b001
        }
{-# NOINLINE dut #-}

memoryMap :: MemoryMap
memoryMap = getMMAny dut0
 where
  dut0 :: Circuit (ConstBwd MM, Df System ()) ()
  dut0 = circuit $ \(mm, _df) -> do
    _uart <- dut -< mm
    idC

type IMemWords = DivRU (128 * 1024) 4
type DMemWords = DivRU (128 * 1024) 4
