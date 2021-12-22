{-|
Copyright  :  (C) 2020, Christiaan Baaij
              (C) 2021, Google LLC
License    :  Apache-2.0
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

module Contranomy.Core.SharedTypes where

import Clash.Prelude

type MachineWord = BitVector 32
type PC = BitVector 32
type Alignment = BitVector 1

alignPC :: PC -> PC
alignPC pc = pc' ++# (0 :: Alignment)
  where
    (pc', _) = split pc