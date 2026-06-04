-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}

module Tests.TimedReset where

import Clash.Prelude
import Protocols

import Bittide.SharedTypes (withByteOrder)
import Bittide.TimedReset (timedResetWb)
import Clash.Class.BitPackC (ByteOrder (..))
import Clash.Class.BitPackC.Words (packWordCI)
import Data.Maybe (fromJust)
import Protocols.Experimental.Hedgehog (defExpectOptions)
import Protocols.Experimental.Simulate (SimulationConfig (..), sampleC)
import Protocols.Experimental.Wishbone
import Protocols.Experimental.Wishbone.Standard.Hedgehog (WishboneMasterRequest (..))
import Protocols.MemoryMap

import Clash.Hedgehog.Sized.Unsigned (genUnsigned)
import Hedgehog (Property)
import Test.Tasty
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.TH (testGroupGenerator)

import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.String.Interpolate as Str
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Protocols.Experimental.Wishbone.Standard.Hedgehog as Wb

{- | Writing @release_cycle = N@ must make the reset deassert exactly when the local
counter exceeds @N@: at every cycle the reset is asserted iff @counter <= N@.
-}
prop_timedReset_deasserts_after_release_cycle :: Property
prop_timedReset_deasserts_after_release_cycle = H.property $ do
  -- Lower bound of 100 ensures the register write lands before the counter reaches it.
  releaseCycle <- H.forAll $ genUnsigned @64 (Range.linear 100 (fromIntegral simLength - 20))
  byteOrder <- H.forAll $ Gen.element [BigEndian, LittleEndian]
  withByteOrder byteOrder $ do
    let
      -- 8-byte data width so the 64-bit release_cycle is written in one request.
      dut ::
        Circuit
          (ToConstBwd Mm, Wishbone System 'Standard 32 8)
          (CSignal System (Unsigned 64), CSignal System Bool)
      dut =
        withClockResetEnable @System clockGen resetGen enableGen
          $ circuit
          $ \wb -> do
            let counter = register 0 (counter + 1)
            Fwd rst <- timedResetWb counter -< wb
            idC -< (Fwd counter, Fwd (unsafeToActiveHigh rst))

      deviceName = "TimedReset"
      defs = (getMMAny dut).deviceDefs Map.! deviceName
      releaseCycleLoc = L.find (\loc -> loc.name.name == "release_cycle") defs.registers
      releaseCycleAddr = fromIntegral (fromJust releaseCycleLoc).value.address `div` 8

      releaseCycleBv = pack $ packWordCI @8 releaseCycle
      requests = fmap (,0) [Write releaseCycleAddr maxBound releaseCycleBv]

      (counters, resets) =
        sampleC
          def{timeoutAfter = simLength}
          (unMemmap dut <| Wb.driveStandard defExpectOptions requests)

      -- The reset must be asserted (active-high True) exactly while counter <= N.
      mismatches =
        [ (c, r)
        | (c, r) <- L.zip counters resets
        , r /= (c <= releaseCycle)
        ]

    H.footnote [Str.i|release_cycle = #{releaseCycle}; first mismatches: #{L.take 5 mismatches}|]
    L.null mismatches H.=== True
 where
  simLength = 200

tests :: TestTree
tests = $(testGroupGenerator)
