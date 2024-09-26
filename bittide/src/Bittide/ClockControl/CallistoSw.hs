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

import Bittide.ClockControl (RelDataCount, SpeedChange)
import Bittide.ClockControl.Callisto.Types (ReframingState)
import Bittide.ClockControl.Registers (clockControlWb2)
import Bittide.ClockControl.StabilityChecker (StabilityIndication)
import Bittide.DoubleBufferedRam (ContentType (Blob), InitialContent (Reloadable))
import Bittide.ProcessingElement (PeConfig (..), processingElement)
import Bittide.ProcessingElement.Util (memBlobsFromElf)
import Bittide.SharedTypes (ByteOrder (BigEndian))

import Project.FilePath

import Data.Maybe (isJust)

import Protocols

import VexRiscv

data CallistoSwResult (n :: Nat) = CallistoSwResult
  { maybeSpeedChange :: Maybe SpeedChange
  , reframingState :: ReframingState
  , stability :: Vec n StabilityIndication
  , allStable :: Bool
  , allSettled :: Bool
  , updatePeriod :: Int
  , updatePeriodMin :: Int
  , updatePeriodMax :: Int
  }
  deriving (Generic, NFDataX)

callistoSwClockControl ::
  forall nLinks eBufBits dom margin framesize.
  ( KnownDomain dom
  , KnownNat nLinks
  , KnownNat eBufBits
  , 1 <= nLinks
  , 1 <= eBufBits
  , nLinks + eBufBits <= 32
  , 1 <= framesize
  ) =>
  SNat margin ->
  SNat framesize ->
  Clock dom ->
  Reset dom ->
  Enable dom ->
  Signal dom Bool ->
  Signal dom (BitVector nLinks) ->
  Vec nLinks (Signal dom (RelDataCount eBufBits)) ->
  Signal dom (CallistoSwResult nLinks)
callistoSwClockControl mgn fsz clk rst ena reframe mask ebs = callistoSwResult
 where
  callistoSwResult =
    CallistoSwResult
      <$> fincFdec
      <*> ccReframingState
      <*> stabilities
      <*> ccAllStable
      <*> ccAllSettled
      <*> ccUpdatePeriod
      <*> ccUpdatePeriodMin
      <*> ccUpdatePeriodMax

  (_, (fincFdec, ccReframingState, stabilities, ccAllStable, ccAllSettled, ccUpdatePeriod)) =
    toSignals
      ( circuit $ \jtag -> do
          [wbB] <-
            withClockResetEnable clk rst ena $ processingElement peConfig -< jtag
          (fincFdec, ccReframingState, stabilities, ccAllStable, ccAllSettled, ccUpdatePeriod) <-
            withClockResetEnable clk rst ena
              $ clockControlWb2 mgn fsz mask reframe ebs
              -< wbB
          idC
            -< (fincFdec, ccReframingState, stabilities, ccAllStable, ccAllSettled, ccUpdatePeriod)
      )
      (pure $ JtagIn low low low, (pure (), pure (), pure (), pure (), pure (), pure ()))
  updated :: Signal dom Bool
  updated = isJust <$> fincFdec
  ccUpdatePeriodMin :: Signal dom Int
  ccUpdatePeriodMin = register clk rst ena maxBound $ liftA3 go updated ccUpdatePeriodMin ccUpdatePeriod
   where
    go update prevMin new = if update then min prevMin new else prevMin
  ccUpdatePeriodMax :: Signal dom Int
  ccUpdatePeriodMax = register clk rst ena minBound $ liftA3 go updated ccUpdatePeriodMax ccUpdatePeriod
   where
    go update prevMax new = if update then max prevMax new else prevMax
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
