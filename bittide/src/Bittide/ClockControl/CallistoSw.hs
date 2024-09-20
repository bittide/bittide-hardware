-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
-- {-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=20 #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

module Bittide.ClockControl.CallistoSw where

import Clash.Explicit.Prelude hiding (PeriodToCycles)

-- import qualified Clash.Explicit.Prelude as E
import Clash.Prelude (withClockResetEnable)

import Language.Haskell.TH (runIO)
import System.FilePath

import Bittide.Arithmetic.Time
import Bittide.ClockControl (RelDataCount, SpeedChange)
import Bittide.ClockControl.Registers (clockControlWb2)
import Bittide.ClockControl.StabilityChecker (StabilityIndication)
import Bittide.DoubleBufferedRam (ContentType (Blob), InitialContent (Reloadable))
import Bittide.ProcessingElement (PeConfig (..), processingElement)
import Bittide.ProcessingElement.Util (memBlobsFromElf)
import Bittide.SharedTypes (ByteOrder (BigEndian))

import Project.FilePath

import Protocols

import VexRiscv

data CallistoSwResult (n :: Nat) = CallistoSwResult
  { maybeSpeedChange :: Maybe SpeedChange
  , stability :: Vec n StabilityIndication
  , allStable :: Bool
  , allSettled :: Bool
  , updatePeriod :: Int
  , updatePeriodMin :: Int
  , updatePeriodMax :: Int
  }

callistoSwClockControl ::
  forall nLinks eBufBits dom.
  ( KnownDomain dom
  , KnownNat nLinks
  , KnownNat eBufBits
  , 1 <= nLinks
  , 1 <= eBufBits
  , nLinks + eBufBits <= 32
  ) =>
  Clock dom ->
  Reset dom ->
  Enable dom ->
  Signal dom Bool ->
  Signal dom (BitVector nLinks) ->
  Vec nLinks (Signal dom (RelDataCount eBufBits)) ->
  Signal dom (CallistoSwResult nLinks)
callistoSwClockControl clk rst ena reframe mask ebs = callistoSwResult
 where
  callistoSwResult =
    CallistoSwResult
      <$> fincFdec
      <*> stabilities
      <*> ccAllStable
      <*> ccAllSettled
      <*> ccUpdatePeriod
      <*> ccUpdatePeriodMin
      <*> ccUpdatePeriodMax

  margin' = d2
  framesize' = SNat @(PeriodToCycles dom (Seconds 1))

  (_, (fincFdec, stabilities, ccAllStable, ccAllSettled, ccUpdatePeriod)) =
    toSignals
      ( circuit $ \jtag -> do
          [wbB] <-
            withClockResetEnable clk rst ena $ processingElement peConfig -< jtag
          (fincFdec, stabilities, allStable, allSettled, updatePeriod) <-
            withClockResetEnable clk rst enableGen
              $ clockControlWb2 margin' framesize' mask reframe ebs
              -< wbB
          idC -< (fincFdec, stabilities, allStable, allSettled, updatePeriod)
      )
      (pure $ JtagIn low low low, (pure (), pure (), pure (), pure (), pure ()))
  -- _
  ccUpdatePeriodMin = register clk rst ena (complement 0) (min <$> ccUpdatePeriodMin <*> ccUpdatePeriod)
  ccUpdatePeriodMax = register clk rst ena 0 (max <$> ccUpdatePeriodMax <*> ccUpdatePeriod)
  (iMem, dMem) =
    $( do
        root <- runIO $ findParentContaining "cabal.project"
        let
          elfDir = root </> firmwareBinariesDir "riscv32imc-unknown-none-elf" Release
          elfPath = elfDir </> "clock-control"
          iSize = 64 * 1024 -- 64 KB
          dSize = 64 * 1024 -- 64 KB
        memBlobsFromElf BigEndian (Just iSize, Just dSize) elfPath Nothing
     )
  peConfig =
    PeConfig
      (0b10 :> 0b01 :> 0b11 :> Nil)
      (Reloadable $ Blob iMem)
      (Reloadable $ Blob dMem)
