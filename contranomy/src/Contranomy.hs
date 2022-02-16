{-|
Copyright  :  (C) 2020, Christiaan Baaij
              (C) 2021, Google LLC
License    :  Apache-2.0
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NamedFieldPuns #-}

module Contranomy where

import Clash.Prelude
import Clash.Annotations.TH

import Contranomy.Core
import Contranomy.RegisterFile
import Contranomy.RVFI
import Contranomy.Wishbone
import qualified Data.List as L
import Data.IntMap (IntMap)

createDomain vXilinxSystem{vName="Core", vPeriod=hzToPeriod 100e6}

-- | Contranomy RV32I core
contranomy ::
  "" ::: BitVector 32 ->
  "clk" ::: Clock Core ->
  "reset" ::: Reset Core ->
  ( "" ::: Signal Core CoreIn) ->
  ( "" ::: Signal Core CoreOut)
contranomy entry clk rst coreIn = withClockResetEnable clk rst enableGen $
  let (coreResult,regWrite,_) = core entry (coreIn,regOut)
      regOut = registerFile regWrite
   in coreResult

contranomyTE ::
  "clk" ::: Clock Core ->
  "reset" ::: Reset Core ->
  ( "" ::: Signal Core CoreIn) ->
  ( "" ::: Signal Core CoreOut)
contranomyTE = contranomy 0
makeTopEntity 'contranomyTE


-- | Contranomy RV32I core with RVFI interface
contranomyRVFI ::
  "" ::: BitVector 32 ->
  "clk" ::: Clock Core ->
  "reset" ::: Reset Core ->
  ( "" ::: Signal Core CoreIn) ->
  ( "" ::: Signal Core CoreOut
  , "" ::: Signal Core RVFI)
contranomyRVFI entry clk rst coreIn = withClockResetEnable clk rst enableGen $
  let (coreResult,regWrite,rvfiOut) = core entry (coreIn,regOut)
      regOut = registerFile regWrite
   in (coreResult,rvfiOut)

contranomyRVFITE ::
  "clk" ::: Clock Core ->
  "reset" ::: Reset Core ->
  ( "" ::: Signal Core CoreIn) ->
  ( "" ::: Signal Core CoreOut
  , "" ::: Signal Core RVFI)
contranomyRVFITE = contranomyRVFI 0
makeTopEntity 'contranomyRVFITE

contranomy'
  :: Clock Core
  -> Reset Core
  -> BitVector 32
  -> IntMap (BitVector 8)
  -> IntMap (BitVector 8)
  -> Signal Core (Bool, Bool, BitVector 32)
  -> Signal Core ((Maybe (Unsigned 32, Signed 32)),(Maybe (Unsigned 32, Signed 32)))
contranomy' clk rst entry iMem dMem (unbundle -> (tI, sI, eI)) = bundle (iWritten, dWritten) where
  coreOut' = contranomy entry clk rst $ (\ (tI', sI', eI', iS', dS') -> CoreIn{timerInterrupt=tI', softwareInterrupt = sI', externalInterrupt = eI', iBusS2M = iS', dBusS2M = dS'}) <$> (bundle (tI, sI, eI, iStorage, dStorage))
  instructionM2S = iBusM2S <$> coreOut'
  dataM2S = dBusM2S <$> coreOut'
  iStorage = wishboneStorage "Instruction storage" iMem instructionM2S
  dStorage = wishboneStorage "Data storage" dMem dataM2S
  checkWritten bus = if writeEnable bus then Just (unpack @(Unsigned 32) $ addr bus, unpack @(Signed 32) $ writeData bus) else Nothing
  iWritten = checkWritten <$> instructionM2S
  dWritten = checkWritten <$> dataM2S

bytesToWords :: [BitVector 8] -> [BitVector 32]
bytesToWords (a:b:c:d:es) = [a ++# b ++# c ++# d] L.++ bytesToWords es
bytesToWords [] = []
bytesToWords _  = error "Conversion from bytes to words failed."

wordsToBytes :: [BitVector 32] -> [BitVector 8]
wordsToBytes (a:bs) = [slice d7 d0 a, slice d15 d8 a, slice d23 d16 a, slice d31 d24 a] L.++ wordsToBytes bs
wordsToBytes [] = []
