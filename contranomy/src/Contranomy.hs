{-|
Copyright  :  (C) 2020, Christiaan Baaij
              (C) 2021, Google LLC
License    :  Apache-2.0
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Contranomy where

import Clash.Prelude
import Clash.Annotations.TH

import Contranomy.Core
import Contranomy.RegisterFile
import Contranomy.RVFI

createDomain vXilinxSystem{vName="Core", vPeriod=hzToPeriod 100e6}

-- | Contranomy RV32I core
contranomy ::
  "clk" ::: Clock Core ->
  "reset" ::: Reset Core ->
  ( "" ::: Signal Core CoreIn) ->
  ( "" ::: Signal Core CoreOut)
contranomy clk rst coreIn = withClockResetEnable clk rst enableGen $
  let (coreResult,regWrite,_) = core (coreIn,regOut)
      regOut = registerFile regWrite
   in coreResult

makeTopEntity 'contranomy

-- | Contranomy RV32I core with RVFI interface
contranomyRVFITE ::
  "clk" ::: Clock Core ->
  "reset" ::: Reset Core ->
  ( "" ::: Signal Core CoreIn) ->
  ( "" ::: Signal Core CoreOut
  , "" ::: Signal Core RVFI)
contranomyRVFITE clk rst coreIn = withClockResetEnable clk rst enableGen $
  let (coreResult,regWrite,rvfiOut) = core (coreIn,regOut)
      regOut = registerFile regWrite
   in (coreResult,rvfiOut)

makeTopEntity 'contranomyRVFITE
