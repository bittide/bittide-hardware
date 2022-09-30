{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
import Prelude hiding (head, repeat)
import Clash.Prelude hiding (Exp)

import Bittide.Calendar
import Bittide.DoubleBufferedRam
import Bittide.Link
import Bittide.Node
import Bittide.ProcessingElement
import Bittide.ScatterGather
import Bittide.Switch

import qualified Data.List as L
import Control.Monad (forM_)

import TH (processElfFile, ElfFileConfig (ElfFileConfig))
import Bittide.SharedTypes (DataLink)


main :: IO ()
main = do
  putStrLn "Demo 1"

  let
    output = simulate_lazy (nodeCircuit clockGen resetGen) (L.repeat $ repeat Nothing)

  forM_ output $ \case
    (head -> Nothing) -> pure ()
    (head -> Just _) -> putStrLn "something??"


nodeCircuit
  :: "CLK" ::: Clock System
  -> "RST" ::: Reset System
  -> "IN_LINKS" ::: Signal System (Vec 1 (DataLink 64))
  -> "OUT_LINKS" ::: Signal System (Vec 1 (DataLink 64))
nodeCircuit clk rst (unbundle -> inLinks) =
  let
    gppeMemMap :: BitVector 32 -> BitVector 32 -> Vec 4 (BitVector 32)
    gppeMemMap iMem dMem =
      iMem :>
      dMem :>
      0x00010000 :> -- scatter memory
      0x00018000 :> -- gather memory
      Nil

    managementMemMap :: BitVector 32 -> BitVector 32 -> Vec 25 (BitVector 32)
    managementMemMap iMem dMem =
      iMem :>
      dMem :>
      0x00010000 :> -- scatter memory
      0x00018000 :> -- gather memory
      0x00020000 :> -- rx Unit
      0x00020400 :> -- Scatter unit calendar
      0x00022400 :> -- tx Unit
      0x00022800 :> -- Gather unit calendar
      0x00030000 :> -- Switch calendar
      0x00032000 :> -- Link 0 rx Unit
      0x00032400 :> -- Link 0 tx Unit
      0x00040000 :> -- GPPE 0 rx Unit
      0x00040400 :> -- GPPE 0 Scatter unit calendar
      0x00042400 :> -- GPPE 0 tx Unit
      0x00042800 :> -- GPPE 0 Gather unit calendar
      0x00050000 :> -- GPPE 1 rx Unit
      0x00050400 :> -- GPPE 1 Scatter unit calendar
      0x00052400 :> -- GPPE 1 tx Unit
      0x00052800 :> -- GPPE 1 Gather unit calendar
      0x10000000 :> -- ???
      0x11000000 :> -- ???
      0x12000000 :> -- ???
      0x13000000 :> -- ???
      0x14000000 :> -- ???
      0x15000000 :> -- ???
      Nil


    pingPeConfig = peConfigFromElfConfig gppeMemMap $(processElfFile "ping" "gppe")
    pongPeConfig = peConfigFromElfConfig gppeMemMap $(processElfFile "pong" "gppe")
    managementPeConfig = peConfigFromElfConfig managementMemMap $(processElfFile "management" "management")

    switchConfig = SwitchConfig preamble' switchCal
    switchCal = CalendarConfig (SNat @1024) (repeat @1 $ repeat 0) (repeat @1 $ repeat 0)
    linkConfig = LinkConfig preamble' (ScatterConfig sgConfig) (GatherConfig sgConfig)
    sgConfig = CalendarConfig (SNat @1024) (repeat @1 (0 :: Index 1024)) (repeat @1 0)
    preamble' = 0xDEADBEEFA5A5A5A5FACADE :: BitVector 96

    nodeConfig :: NodeConfig 1 2
    nodeConfig =
        NodeConfig
          (ManagementConfig linkConfig managementPeConfig)
          switchConfig
          (GppeConfig linkConfig pingPeConfig :> GppeConfig linkConfig pongPeConfig :> Nil)

  in
    bundle $ withClockResetEnable clk rst enableGen $ node nodeConfig inLinks

peConfigFromElfConfig :: (BitVector 32 -> BitVector 32 -> Vec n (BitVector 32)) -> ElfFileConfig -> PeConfig n
peConfigFromElfConfig mmapFn (ElfFileConfig iStartAddr SNat iMem dStartAddr SNat dMem entry) =
  PeConfig
    (mmapFn iStartAddr dStartAddr)
    iMem
    dMem
    entry
