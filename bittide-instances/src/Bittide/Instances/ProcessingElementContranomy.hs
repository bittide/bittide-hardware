-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.ProcessingElementContranomy where

import Clash.Prelude
import Bittide.ProcessingElement
import Bittide.ProcessingElement.Util

import Bittide.DoubleBufferedRam
import Paths_bittide_instances
import Language.Haskell.TH

contranomy :: Clock System -> Reset System -> Signal System (Unsigned 8)
contranomy clk rst = out
 where
  (wbM2S :> Nil) =
    withClockResetEnable clk rst enableGen $ contranomyProcessingElement peConfig (wbS2M :> Nil)

  (  (_iStart, _iSize, iMem)
   , (_dStart, _dSize, dMem)) = $(do
      elfPath <- runIO $ getDataFileName "/demo-files/binaries/instance-demo-test-program"
      -- fdtPath <- runIO $ getDataFileName "/device-trees/hello.dts"
      memBlobsFromElf elfPath Nothing (0x20000000 :: Int))

  peConfig = PeConfig (0 :> 1 :> 2 :> Nil) (Reloadable $ Blob iMem) (Reloadable $ Blob dMem)
  (out, wbS2M) = withClockResetEnable clk rst enableGen $ registerWb WishbonePriority 0 wbM2S (pure Nothing)
