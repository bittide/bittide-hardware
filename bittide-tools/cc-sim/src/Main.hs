-- SPDX-FileCopyrightText: 2022-2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}

module Main (main) where

import Domain

import Bittide.Plot
import Bittide.Topology

import Data.Aeson (ToJSON(..), FromJSON(..), encode, decode)
import Data.ByteString.Lazy qualified as BS
import Data.Proxy (Proxy(..))

import GHC.Generics (Generic)
import GHC.Int (Int64)

import Language.Dot.Pretty (render)

import Options.Applicative

import System.Exit (exitSuccess, die)
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing)

data Options =
  Options
    { topology           :: Maybe Topology
    , outMode            :: OutputMode
    , simulationSteps    :: Int
    , simulationSamples  :: Int
    , stabilityMargin    :: Int
    , stabilityFrameSize :: Int
    , disableReframing   :: Bool
    , rusty              :: Bool
    , waitTime           :: Int
    , stopWhenStable     :: Bool
    , stopAfterStable    :: Maybe Int
    , clockOffsets       :: [Int64]
    , startupOffsets     :: [Int]
    , maxStartupOffset   :: Int
    , outDir             :: FilePath
    , jsonArgs           :: Maybe FilePath
    , stable             :: Maybe Bool
    }
  deriving (Show, Ord, Eq, Generic, ToJSON, FromJSON)

optionParser :: Parser Options
optionParser =
  Options
    <$> optional topologyParser
    <*> option auto
          (  long "output-mode"
          <> short 'm'
          <> metavar "MODE"
          <> value PDF
          <> showDefault
          <> help "Available modes are: csv, pdf"
          )
    <*> option auto
          (  long "steps"
          <> short 's'
          <> metavar "NUM"
          <> value 150000
          <> showDefault
          <> help "Number of clock cycles to simulate"
          )
    <*> option auto
          (  long "samples"
          <> short 'a'
          <> metavar "NUM"
          <> value 100
          <> showDefault
          <> help "Number of samples to keep & pass to matplotlib"
          )
    <*> option auto
          (  long "margin"
          <> short 'g'
          <> metavar "NUM"
          <> value 8
          <> showDefault
          <> help
               (  "Maximum number of elements a buffer occupancy is "
               <> "allowed to deviate to be considered stable"
               )
          )
    <*> option auto
          (  long "frame-size"
          <> short 'f'
          <> metavar "NUM"
          <> value 1500000
          <> showDefault
          <> help
               (  "Minimum number of clock cycles a buffer occupancy "
               <> "must remain within to be considered stable"
               )
          )
    <*> flag False True
          (  long "disable-reframing"
          <> short 'e'
          <> help "Disables clock control reframing"
          )
    <*> flag False True
          (  long "get-rusty"
          <> short 'y'
          <> help "Simulate clock control via the Rust FFI"
          )
    <*> option auto
          (  long "wait-time"
          <> short 'w'
          <> metavar "NUM"
          <> value 100000
          <> showDefault
          <> help
               (  "Number of clock cycles to wait until reframing takes place "
               <> "(after stability has been detected, for all elastic buffers)"
               )
          )
    <*> flag False True
          (  long "stop-when-stable"
          <> short 'x'
          <> help "Stop simulation as soon as all buffers get stable"
          )
    <*> optional
          ( option auto
              (  long "stop-after-stable"
              <> short 'X'
              <> metavar "NUM"
              <> help
                   (  "Stop simulation after all buffers have been stable for "
                   <> " at least the given number of simulation steps"
                   )
              )
          )
    <*> option auto
          (  long "clock-offsets"
          <> short 't'
          <> metavar "NUM LIST"
          <> value []
          <> showDefault
          <> help "Initital clock offsets (randomly generated if missing)"
          )
    <*> option auto
          (  long "startup-offsets"
          <> short 'T'
          <> metavar "NUM LIST"
          <> value []
          <> showDefault
          <> help (  "Initital startup offsets, i.e, the number of clock cycles "
                  <> "to wait before a node gets started (according to the "
                  <> "node's individual clock, randomly generated if missing)"
                  )

          )
    <*> option auto
          (  long "max-startup-offset"
          <> short 'u'
          <> metavar "NUM"
          <> value 0
          <> showDefault
          <> help
               (  "Maximal number of clock cycles the startup of a node may be "
               <> "delayed (bounds the randomly generated offsets)"
               )
          )
    <*> strOption
          (  long "output-directory"
          <> short 'o'
          <> metavar "DIR"
          <> action "directory"
          <> value "_build"
          <> showDefault
          <> help "Directory, to which the generated files are written"
          )
    <*> optional
          ( strOption
              (  long "json-args"
              <> short 'j'
              <> metavar "FILE"
              <> action "file"
              <> help
                   (  "Read arguments from a 'simulate.json' file "
                   <> "(overwrites all arguments other than '-jz')"
                   )
              )
          )
    <*> pure Nothing

cliParser :: ParserInfo Options
cliParser = info (optionParser <**> helper)
  (  fullDesc
  <> header "Bittide Hardware Topology Simulator"
  )

main :: IO ()
main = do
  options@Options{..} <- do
    opts@Options{..} <- execParser cliParser
    case jsonArgs of
      Nothing   -> return opts
      Just file -> do
        cnt <- BS.readFile file
        case decode cnt of
          Nothing -> die $ "ERROR: Invalid JSON file - " <> file
          Just o  -> return o { jsonArgs }

  ccc <- clockControlConfig (Proxy @Bittide)
           (not disableReframing) rusty waitTime
           stabilityMargin stabilityFrameSize

  let
    safeToFile name g clockOffs startOffs isStable = do
      createDirectoryIfMissing True outDir
      let topologyFile = outDir </> "topology.gv"
      writeFile topologyFile $ (<> "\n") $ render $ toDot g name
      BS.writeFile (outDir </> "simulate.json") $ encode options
        { stable   = isStable
        , clockOffsets = clockOffs
        , startupOffsets = startOffs
        , jsonArgs = Nothing
        , topology = case topology of
            Just (Random _) -> Just $ DotFile topologyFile
            _               -> topology
        }

    settings =
      SimulationSettings
        { samples      = simulationSamples
        , periodsize   = simulationSteps `quot` simulationSamples
        , mode         = outMode
        , dir          = outDir
        , stopStable   =
            if stopWhenStable
            then Just 0
            else (`quot` simulationSamples) <$> stopAfterStable
        , fixClockOffs = clockOffsets
        , fixStartOffs = startupOffsets
        , maxStartOff  = maxStartupOffset
        , ccConfig     = ccc
        , save         = \_ _ _ _ -> return ()
        }

  isStable <- case topology of
    Just t -> do
      let ?settings = settings { save = safeToFile $ topologyName t }
      case t of
        Diamond       -> plotDiamond
        Line n        -> plotLine n
        HyperCube n   -> plotHyperCube n
        Grid r c      -> plotGrid r c
        Torus2D r c   -> plotTorus2D r c
        Torus3D r c p -> plotTorus3D r c p
        Tree d c      -> plotTree d c
        Star n        -> plotStar n
        Cycle n       -> plotCyclic n
        Complete n    -> plotComplete n
        Hourglass n   -> plotHourglass n
        Random n      -> randomGraph n >>= plotGraph
        DotFile f     -> (fromDot <$> readFile f) >>= \case
          Right (g, name) ->
            let ?settings = ?settings { save = safeToFile name }
            in plotGraph g
          Left err -> die $ "ERROR: Invalid DOT file - " <> f <> "\n" <> err
    Nothing ->
      handleParseResult $ Failure
        $ parserFailure defaultPrefs cliParser (ShowHelpText Nothing) []

  if isStable
  then exitSuccess
  else die "Simulated topology did not stabilize in time."
