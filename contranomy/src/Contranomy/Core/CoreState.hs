{-|
Copyright  :  (C) 2020, Christiaan Baaij
              (C) 2021, Google LLC
License    :  Apache-2.0
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

module Contranomy.Core.CoreState where

import Clash.Class.AutoReg
import Clash.Prelude

import Contranomy.Core.MachineState
import Contranomy.Core.SharedTypes

data CoreStage
  = InstructionFetch
  | Execute { accessFault :: Bool }
  deriving (Generic, NFDataX, AutoReg)

data CoreState
  = CoreState
  { stage :: CoreStage
  , pc :: PC
  , instruction :: MachineWord
  , machineState :: MachineState
  , rvfiOrder :: Unsigned 64
  }
  deriving (Generic, NFDataX)

deriveAutoReg ''CoreState
