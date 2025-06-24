-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Clash.Protocols.Axi4.Extra where

import Clash.Prelude

import Protocols.Axi4.ReadAddress (M2S_ReadAddress (M2S_NoReadAddress))
import Protocols.Axi4.ReadData (S2M_ReadData (S2M_NoReadData))
import Protocols.Axi4.WriteAddress (M2S_WriteAddress (M2S_NoWriteAddress))
import Protocols.Axi4.WriteData (M2S_WriteData (M2S_NoWriteData))
import Protocols.Axi4.WriteResponse (S2M_WriteResponse (S2M_NoWriteResponse))

isReadAddress :: M2S_ReadAddress conf userType -> Bool
isReadAddress M2S_NoReadAddress = False
isReadAddress _ = True

isReadData :: S2M_ReadData conf userType dataType -> Bool
isReadData S2M_NoReadData = False
isReadData _ = True

isWriteAddress :: M2S_WriteAddress conf userType -> Bool
isWriteAddress M2S_NoWriteAddress = False
isWriteAddress _ = True

isWriteData :: M2S_WriteData conf userType -> Bool
isWriteData M2S_NoWriteData = False
isWriteData _ = True

isWriteResponse :: S2M_WriteResponse conf userType -> Bool
isWriteResponse S2M_NoWriteResponse = False
isWriteResponse _ = True
