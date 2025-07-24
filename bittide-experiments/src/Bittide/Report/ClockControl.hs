-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ImplicitPrelude #-}

module Bittide.Report.ClockControl (
  generateReport,
  checkDependencies,
  checkIntermediateResults,
  formatThousands,
) where

import Clash.Prelude (Domain, KnownDomain, Milliseconds, natToNum)

import Data.Bool (bool)
import Data.List (intercalate)
import Data.Proxy (Proxy (..))
import GHC.Float.RealFracMethods (roundFloatInteger)
import System.Directory (doesDirectoryExist, doesFileExist, findExecutable)
import System.Environment (lookupEnv)
import System.FilePath (takeFileName, (</>))
import System.IO (
  BufferMode (..),
  IOMode (..),
  hClose,
  hFlush,
  hPutStr,
  hSetBuffering,
  withFile,
 )
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess, readProcess)
import "extra" Data.List.Extra (chunksOf)

import Bittide.Arithmetic.PartsPer (toPpm)
import Bittide.Arithmetic.Time (PeriodToCycles)
import Bittide.Plot
import Bittide.Simulate.Config (CcConf (..), simTopologyFileName)

{- | Format a number with underscores every three digits.

>>> formatThousands 123456789
"123,456,789"
>>> formatThousands 100000000
"100,000,000"
>>> formatThousands 1000000
"1,000,000"
-}
formatThousands :: (Num a, Show a) => a -> String
formatThousands = reverse . intercalate "," . chunksOf 3 . reverse . show

generateReport ::
  (KnownDomain refDom) =>
  -- | Reference domain
  Proxy (refDom :: Domain) ->
  -- | Document description header
  String ->
  -- | Directory containing the intermediate plot results
  FilePath ->
  -- | Node identifiers
  [(Integer, String)] ->
  -- | The utilized simulation configuration
  CcConf ->
  IO ()
generateReport refDom (("Bittide - " <>) -> header) dir ids cfg =
  withSystemTempDirectory "generate-report" $ \tmpDir -> do
    Just runref <- lookupEnv "RUNREF"
    -- remove the 'n' prefix from the node names
    readProcess
      "sed"
      [ "-e"
      , "s/n\\([0-9]*\\)/\\1/g"
      , topologyGv
      ]
      []
      >>= writeFile (tmpDir </> takeFileName topologyGv)
    -- render the topology as a tikz picture
    topTikz <-
      readProcess
        "dot2tex"
        [ "-f"
        , "tikz"
        , "--figonly"
        , "--progoptions"
        , "-K neato"
        , "--nodeoptions"
        , "every node/.style={"
            <> intercalate
              ","
              [ "fill"
              , "text=white"
              , "font=\\Large\\tt"
              , "minimum size=2em"
              , "inner sep=0pt"
              ]
            <> "}"
        , "--edgeoptions"
        , "line width=0.3em"
        , tmpDir </> takeFileName topologyGv
        ]
        []
    -- get the current date/time reference
    datetime <-
      readProcess
        "date"
        [ "+%Y-%m-%d %H:%M:%S"
        ]
        []
    -- create the latex report
    withFile (tmpDir </> "report.tex") WriteMode $ \h -> do
      hSetBuffering h NoBuffering
      hPutStr h $ toLatex refDom datetime runref header clocksPdf ebsPdf topTikz ids cfg
      hFlush h
      hClose h
    -- create the report pdf
    callProcess
      "lualatex"
      [ "--output-directory=" <> tmpDir
      , tmpDir </> "report.tex"
      ]
    -- move it to the target directory
    callProcess
      "mv"
      [ tmpDir </> "report.pdf"
      , dir </> "report.pdf"
      ]
 where
  clocksPdf = dir </> plotClocksFileName
  ebsPdf = dir </> plotElasticBuffersFileName
  topologyGv = dir </> simTopologyFileName

checkDependencies :: IO (Maybe String)
checkDependencies =
  either Just (const Nothing)
    . sequence_
    <$> sequence
      [ checkEVarExists "RUNREF"
      , checkProgExists "mv"
      , checkProgExists "sed"
      , checkProgExists "dot2tex"
      , checkProgExists "date"
      ]
 where
  checkEVarExists e =
    maybe (Left $ "Missing environment variable: " <> e) (const $ Right ())
      <$> lookupEnv e

  checkProgExists p =
    maybe (Left $ "Missing dependency: " <> p) (const $ Right ())
      <$> findExecutable p

checkIntermediateResults :: FilePath -> IO (Maybe String)
checkIntermediateResults dir =
  either Just (const Nothing)
    . sequence_
    <$> sequence
      [ checkDirExists dir
      , checkFileExists $ dir </> plotClocksFileName
      , checkFileExists $ dir </> plotElasticBuffersFileName
      , checkFileExists $ dir </> simTopologyFileName
      ]
 where
  checkDirExists d =
    bool (Left $ "No such directory: " <> d) (Right ())
      <$> doesDirectoryExist d

  checkFileExists f =
    bool (Left $ "Missing file: " <> f) (Right ())
      <$> doesFileExist f

toLatex ::
  forall refDom.
  (KnownDomain refDom) =>
  -- | Reference domain
  Proxy (refDom :: Domain) ->
  -- | date & time reference
  String ->
  -- | Github run reference
  String ->
  -- | Document description header
  String ->
  -- | File path of the clocks plot pdf
  FilePath ->
  -- | File path of the elastic buffers plot pdf
  FilePath ->
  -- | Tikz plot of the topology
  String ->
  -- | Node identifiers
  [(Integer, String)] ->
  -- | The utilized simulation configuration
  CcConf ->
  String
toLatex _refDom datetime runref header clocksPdf ebsPdf topTikz ids CcConf{..} =
  unlines
    [ "\\documentclass[landscape]{article}"
    , ""
    , "\\usepackage[top=3cm,bottom=3cm,left=1.5cm,right=1.5cm]{geometry}"
    , "\\usepackage[hidelinks]{hyperref}"
    , "\\usepackage[dvipsnames]{xcolor}"
    , "\\usepackage{graphicx}"
    , "\\usepackage{pifont}"
    , "\\usepackage{fancyhdr}"
    , "\\usepackage{tikz}"
    , "\\usepackage{siunitx}"
    , ""
    , "\\usetikzlibrary{shapes, calc, shadows}"
    , ""
    , "% Not actually an SI unit"
    , "\\DeclareSIUnit\\ppm{ppm}"
    , ""
    , "\\pagestyle{fancy}"
    , "\\fancyhf{}"
    , "\\fancyhead[L]{\\large \\textbf{" <> header <> "}}"
    , "\\fancyhead[C]{\\large Topology Type: \\texttt{"
        <> show ccTopologyType
        <> "}}"
    , "\\fancyhead[R]{\\large " <> datetime <> "}"
    , "\\renewcommand{\\headrulewidth}{0.4pt}"
    , "\\fancyfoot[L]{\\large\\textit{\\href{"
        <> runref
        <> "}{"
        <> runref
        <> "}}}"
    , "\\fancyfoot[R]{\\large\\copyright~Google Inc., QBayLogic B.V.}"
    , "\\renewcommand{\\footrulewidth}{0.4pt}"
    , ""
    , "\\parindent0pt"
    , ""
    , "\\begin{document}"
    , ""
    , "\\ \\vspace{3em}"
    , ""
    , "\\begin{center}"
    , "\\begin{tikzpicture}[overlay, xshift=0.27\\textwidth, yshift=-3]"
    , "\\node {"
    , "\\begin{tikzpicture}"
    , "\\node (A) {\\resizebox{!}{10em}{"
    , topTikz
    , "}};"
    , if null ids
        then ""
        else
          unlines $
            [ "\\node at ($ (A.east) + (3,0) $) {"
            , "\\small\\tt"
            , "\\begin{tabular}{r|c}"
            , "  & \\textbf{FPGA ID} \\\\[0.1em]"
            , "\\hline \\\\[-0.9em]"
            ]
              <> map (\(i, n) -> show i <> " & " <> n <> " \\\\") ids
              <> [ "\\end{tabular}"
                 , "};"
                 ]
    , "\\end{tikzpicture}"
    , "};"
    , "\\end{tikzpicture}"
    , "\\end{center}"
    , ""
    , "\\vspace{-5em}"
    , ""
    , "\\begin{large}"
    , "  \\begin{tabular}{rl}"
    , "    clock offsets:"
    , "      & " <> maybe "\\textit{not used}" formatOffsets clockOffsets <> " \\\\"
    , "    startup delays:"
    , "      & " <> intercalate "; " (qtyMs <$> startupDelaysMs) <> " \\\\"
    , "    reframing:"
    , "      & "
        <> "\\textit{"
        <> bool "disabled" "enabled" reframe
        <> "} \\\\"
    , if reframe then "    wait time: & " <> show waitTime <> " \\\\" else ""
    , "    all buffers stable end of run:"
    , "      & "
        <> maybe
          ""
          ( bool
              "\\textcolor{red!50!black}{\\ding{55}}"
              "\\textcolor{green!50!black}{\\ding{51}}"
          )
          stable
        <> " \\\\"
    , "  \\end{tabular}"
    , "\\end{large}"
    , ""
    , "\\vfill"
    , ""
    , "\\begin{center}"
    , "  \\begin{tikzpicture}"
    , "    \\node (clocks) at (0,0) {"
    , "      \\includegraphics[width=.49\\textwidth]{" <> clocksPdf <> "}"
    , "    };"
    , "    \\node (ebs) at (0.51\\textwidth, 0) {"
    , "      \\includegraphics[width=.49\\textwidth]{" <> ebsPdf <> "}"
    , "    };"
    , "    \\node at ($ (clocks.north) + (0,0) $) {"
    , "      \\textbf{Clocks}"
    , "    };"
    , "    \\node at ($ (ebs.north) + (0,0) $) {"
    , "      \\textbf{Elastic Buffer Occupancies}"
    , "    };"
    , "    \\node[overlay,anchor=north west,fill=blue]"
    , "    (A) at ($ (ebs.south west) + (1.55,0) $) {};"
    , "    \\node[overlay,anchor=north west,inner sep=0pt]"
    , "    (B) at ($ (A.north east) + (0.2,0) $) {"
    , "      \\small\\textit{buffer is stable}"
    , "    };"
    , "    \\node[overlay,anchor=north west,fill=OliveGreen]"
    , "    (C) at ($ (B.north east) + (0.6,0) $) {};"
    , "    \\node[overlay,anchor=north west,inner sep=0pt]"
    , "    (D) at ($ (C.north east) + (0.2,0) $) {"
    , "      \\small\\textit{buffer is stable and centered}"
    , "    };"
    , if not reframe
        then ""
        else
          unlines
            [ "    \\node[overlay,anchor=north west,fill=red]"
            , "    (E) at ($ (A.south west) + (0,-0.2) $) {};"
            , "    \\node[overlay,anchor=north west,inner sep=0pt]"
            , "    (F) at ($ (E.north east) + (0.2,0) $) {"
            , "      \\small\\textit{waiting period (reframing)}"
            , "    };"
            ]
    , "  \\end{tikzpicture}"
    , "\\end{center}"
    , ""
    , "~"
    , ""
    , "\\end{document}"
    ]
 where
  formatOffsets =
    intercalate "; "
      . (fmap (qtyPpm . roundFloatInteger . toPpm))

  qtyMs ms = "\\qty{" <> show ms <> "}{\\milli\\second}"
  qtyPpm ppm = "\\qty{" <> show ppm <> "}{\\ppm}"

  nCyclesOneMs = natToNum @(PeriodToCycles refDom (Milliseconds 1))
  startupDelaysMs = (`div` nCyclesOneMs) <$> startupDelays
