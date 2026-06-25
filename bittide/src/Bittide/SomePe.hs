-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.SomePe (somePe) where

import Clash.Prelude
import Protocols

import Clash.Class.BitPackC (ByteOrder)
import GHC.Stack (HasCallStack)
import Protocols.MemoryMap (Mm)
import VexRiscv (DumpVcd (NoDumpVcd), Jtag)

import Bittide.ProcessingElement (PeConfig (..), processingElement)
import Bittide.Wishbone (timeWb)

import qualified Bittide.Cpus.Riscv32imc as Riscv32imc

{- | A small (probably not minimal) reproducer of a bug(?) in circuit-notation where
monomorphism restriction causes weird type errors. To reproduce, remove the type
annotation of 'withRefClockResetEnable'.
-}
somePe ::
  forall dom.
  ( HasCallStack
  , KnownDomain dom
  , 1 <= DomainPeriod dom
  , ?byteOrder :: ByteOrder
  ) =>
  Clock dom ->
  Reset dom ->
  Circuit
    (ToConstBwd Mm, Jtag dom)
    ()
somePe refClk refRst = circuit $ \(mm, jtag) -> do
  [timeBus] <- withRefClockResetEnable $ processingElement NoDumpVcd peConfig -< (mm, jtag)

  Fwd _localCounter <- withRefClockResetEnable $ timeWb Nothing -< timeBus

  idC -< ()
 where
  -- XXX: leaving out this type annotation causes really weird type errors in circuit notation.
  withRefClockResetEnable :: forall r. ((HiddenClockResetEnable dom) => r) -> r
  withRefClockResetEnable = withClockResetEnable refClk refRst enableGen

  peConfig :: PeConfig 3
  peConfig =
    PeConfig
      { cpu = Riscv32imc.vexRiscv0
      , depthI = SNat @(Div (1024) 4)
      , depthD = SNat @(Div (1024) 4)
      , initI = Nothing
      , initD = Nothing
      , iBusTimeout = d0
      , dBusTimeout = d0
      , includeIlaWb = False
      }
