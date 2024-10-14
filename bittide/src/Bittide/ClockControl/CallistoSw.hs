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

import Bittide.ClockControl (RelDataCount)
import Bittide.ClockControl.Callisto.Types (CallistoResult (..))
import Bittide.ClockControl.Registers (clockControlWb, FadjHoldCycles)
import Bittide.DoubleBufferedRam (ContentType (Blob), InitialContent (Reloadable))
import Bittide.ProcessingElement (PeConfig (..), processingElement)
import Bittide.ProcessingElement.Util (memBlobsFromElf)
import Bittide.SharedTypes (ByteOrder (BigEndian))

import Project.FilePath

import Protocols

import VexRiscv
import Clash.Cores.Xilinx.Ila (IlaConfig(..), ila, ilaConfig, Depth (..))
import Data.Maybe (isJust)
import Bittide.ElasticBuffer (sticky)

callistoSwClockControl ::
  forall nLinks eBufBits dom margin framesize.
  ( KnownDomain dom
  , KnownNat nLinks
  , KnownNat eBufBits
  , 1 <= nLinks
  , 1 <= eBufBits
  , nLinks + eBufBits <= 32
  , 1 <= framesize
  , 1 <= FadjHoldCycles dom
  , 1 <= DomainPeriod dom
  ) =>
  SNat margin ->
  SNat framesize ->
  Clock dom ->
  Reset dom ->
  Enable dom ->
  Signal dom Bool ->
  Signal dom (BitVector nLinks) ->
  Vec nLinks (Signal dom (RelDataCount eBufBits)) ->
  Signal dom (CallistoResult nLinks)
callistoSwClockControl mgn fsz clk rst ena reframe mask ebs =
  hwSeqX callistoSwIla callistoResult
 where
  callistoResult =
    CallistoResult
      <$> fincFdec
      <*> stabilities
      <*> ccAllStable
      <*> ccAllSettled
      <*> ccReframingState

  callistoSwIla :: Signal dom ()
  callistoSwIla =
    setName @"callistoSwIla"
      $ ila
        ( ilaConfig
            $ "trigger_0"
            :> "capture_0"
            :> "probe_updatePeriod"
            :> "probe_updatePeriodMin"
            :> "probe_updatePeriodMax"
            :> Nil
        )
          { depth = D16384
          }
        clk
        (unsafeToActiveLow rst)
        capture
        ccUpdatePeriod
        updatePeriodMin
        updatePeriodMax

  capture = isRising clk rst ena False (isJust <$> fincFdec)
  skippedFirst = sticky clk rst $ delay clk ena False capture

  updatePeriodMin =
    regEn
      clk
      rst
      ena
      maxBound
      (capture .&&. skippedFirst)
      (liftA2 min updatePeriodMin ccUpdatePeriod)
  updatePeriodMax =
    regEn
      clk
      rst
      ena
      minBound
      (capture .&&. skippedFirst)
      (liftA2 max updatePeriodMax ccUpdatePeriod)

  (_, (fincFdec, ccReframingState, stabilities, ccAllStable, ccAllSettled, ccUpdatePeriod)) =
    toSignals
      ( circuit $ \jtag -> do
          [wbB] <-
            withClockResetEnable clk rst ena $ processingElement peConfig -< jtag
          (fincFdec, ccReframingState, stabilities, ccAllStable, ccAllSettled, ccUpdatePeriod) <-
            withClockResetEnable clk rst ena
              $ clockControlWb mgn fsz mask reframe ebs
              -< wbB
          idC
            -< (fincFdec, ccReframingState, stabilities, ccAllStable, ccAllSettled, ccUpdatePeriod)
      )
      (pure $ JtagIn low low low, (pure (), pure (), pure (), pure (), pure (), pure ()))
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
