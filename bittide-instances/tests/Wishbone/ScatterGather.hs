-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

module Wishbone.ScatterGather where

import Clash.Explicit.Prelude
import Clash.Prelude (HiddenClockResetEnable, withClockResetEnable)

import Data.Char (chr)
import Data.Maybe (mapMaybe)
import Project.FilePath
import Protocols
import Protocols.Idle
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH
import VexRiscv (DumpVcd (NoDumpVcd))

import Bittide.Calendar
import Bittide.DoubleBufferedRam
import Bittide.ProcessingElement
import Bittide.ProcessingElement.Util
import Bittide.ScatterGather
import Bittide.SharedTypes
import Bittide.Wishbone

import qualified Protocols.Df as Df
import qualified Prelude as P

sim :: IO ()
sim = putStr simResult

simResult :: String
simResult = chr . fromIntegral <$> mapMaybe Df.dataToMaybe uartStream
 where
  uartStream =
    sampleC def{timeoutAfter = 100_000}
      $ withClockResetEnable clockGen resetGen enableGen
      $ dut @System @4 @32 scatterConfig gatherConfig

  scatterConfig = ScatterConfig $ CalendarConfig d32 scatterCal scatterCal
  gatherConfig = GatherConfig $ CalendarConfig d32 gatherCal gatherCal

  -- Padding is required to increase the duration of a metacycle, giving the CPU
  -- enough time to write to the gather memory and read from the scatter memory.
  -- The calendar for the scatter unit is delayed by one cycle.
  padding = 512
  incrementingCal = genIncrementingCalendar @16
  gatherCal = incrementingCal :< ValidEntry maxBound (padding - 1 :: Unsigned 16)
  scatterCal = (ValidEntry 0 0 :> incrementingCal) :< ValidEntry maxBound (padding - 2)

case_scatter_gather_echo_test :: Assertion
case_scatter_gather_echo_test = do
  assertBool
    msg
    (P.head (lines simResult) == "Written data was read back correctly")
 where
  msg = "Received the following from the CPU over UART:\n" <> simResult

genIncrementingCalendar ::
  forall size repititionBits.
  (KnownNat size, KnownNat repititionBits) =>
  Calendar size (Index size) repititionBits
genIncrementingCalendar = iterateI f (ValidEntry 0 0)
 where
  f (ValidEntry i _) = ValidEntry (succ i) 0

{- | A simulation-only instance containing just VexRisc with UART and a scatter and a
gather unit. The VexRiscv runs the `scatter_gather_test` binary from `firmware-binaries`.
-}
dut ::
  forall dom nBytes addrW.
  ( HiddenClockResetEnable dom
  , KnownNat nBytes
  , 1 <= nBytes
  , KnownNat addrW
  ) =>
  ScatterConfig nBytes addrW ->
  GatherConfig nBytes addrW ->
  Circuit () (Df dom (BitVector 8))
dut scatterConfig gatherConfig = circuit $ do
  (uartRx, jtagIdle, wbGuCal, wbSuCal) <- idleSource -< ()
  [uartBus, wbSu, wbGu] <- processingElement NoDumpVcd peConfig -< jtagIdle
  (uartTx, _uartStatus) <- uartInterfaceWb d16 d2 uartSim -< (uartBus, uartRx)
  link <- gatherUnitWbC gatherConfig -< (wbGu, wbGuCal)
  scatterUnitWbC scatterConfig -< (link, wbSu, wbSuCal)
  idC -< uartTx
 where
  memMap = 0b000 :> 0b001 :> 0b010 :> 0b011 :> 0b100 :> Nil
  peConfig = unsafePerformIO $ do
    root <- findParentContaining "cabal.project"
    let
      elfDir = root </> firmwareBinariesDir "riscv32imc" Release
      elfPath = elfDir </> "scatter_gather_test"
    (iMem, dMem) <- vecsFromElf @IMemWords @DMemWords BigEndian elfPath Nothing
    pure
      PeConfig
        { memMapConfig = memMap
        , initI = Reloadable (Vec iMem)
        , initD = Reloadable (Vec dMem)
        , iBusTimeout = d0 -- No timeouts on the instruction bus
        , dBusTimeout = d0 -- No timeouts on the data bus
        , includeIlaWb = False
        }

type IMemWords = DivRU (64 * 1024) 4
type DMemWords = DivRU (32 * 1024) 4

tests :: TestTree
tests = $(testGroupGenerator)
