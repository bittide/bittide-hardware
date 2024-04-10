-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE ImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
module Bittide.Report.ClockControl
  ( generateReport
  , checkDependencies
  , checkIntermediateResults
  ) where

import Data.Bool (bool)
import Data.List (intercalate)
import System.Directory (doesDirectoryExist, doesFileExist, findExecutable)
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeFileName)
import System.IO ( BufferMode(..), IOMode(..)
                 , withFile, hSetBuffering, hPutStr, hFlush, hClose
                 )
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess, readProcess)

import Bittide.Plot
import Bittide.Simulate.Config

generateReport ::
  String ->
  -- ^ Document description header
  FilePath ->
  -- ^ Directory containing the intermediate plot results
  SimConf ->
  -- ^ The utilized simulation configuration
  IO ()
generateReport (("Bittide - " <>) -> header) dir cfg =
  withSystemTempDirectory "generate-report" $ \tmpDir -> do
    Just runref <- lookupEnv "RUNREF"
    -- remove the 'n' prefix from the node names
    readProcess "sed"
      [ "-e", "s/n\\([0-9]*\\)/\\1/g"
      , topologyGv
      ] []
      >>= writeFile (tmpDir </> takeFileName topologyGv)
    -- render the topology as a tikz picture
    topTikz <- readProcess "dot2tex"
      [ "-f", "tikz"
      , "--figonly"
      , "--progoptions", "-K neato"
      , "--nodeoptions", "every node/.style={"
          <> intercalate ","
               [ "fill"
               , "text=white"
               , "font=\\Large\tt"
               , "minimum size=2em"
               , "inner sep=0pt"
               ]
          <> "}"
      , "--edgeoptions", "line width=0.3em"
      , tmpDir </> takeFileName topologyGv
      ] []
    -- get the current date/time reference
    datetime <- readProcess "date"
      [ "+%Y-%m-%d %H:%M:%S"
      ] []
    -- create the latex report
    withFile (tmpDir </> "report.tex") WriteMode $ \h -> do
      hSetBuffering h NoBuffering
      hPutStr h $ toLatex datetime runref header clocksPdf ebsPdf topTikz cfg
      hFlush h
      hClose h
    -- create the report pdf
    callProcess "lualatex"
      [ "--output-directory=" <> tmpDir
      , tmpDir </> "report.tex"
      ]
    -- move it to the target directory
    callProcess "mv"
      [ tmpDir </> "report.pdf"
      , dir </> "report.pdf"
      ]
 where
  clocksPdf  = dir </> plotClocksFileName
  ebsPdf     = dir </> plotElasticBuffersFileName
  topologyGv = dir </> simTopologyFileName

checkDependencies :: IO (Maybe String)
checkDependencies =
  either Just (const Nothing) . sequence_ <$> sequence
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
  either Just (const Nothing) . sequence_ <$> sequence
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
  String ->
  -- ^ date & time reference
  String ->
  -- ^ Github run reference
  String ->
  -- ^ Document description header
  FilePath ->
  -- ^ File path of the clocks plot pdf
  FilePath ->
  -- ^ File path of the elastic buffers plot pdf
  String ->
  -- ^ Tikz plot of the topology
  SimConf ->
  -- ^ The utilized simulation configuration
  String
toLatex datetime runref header clocksPdf ebsPdf topTikz SimConf{..} = unlines
  [ "\\documentclass[landscape]{article}"
  , ""
  , "\\usepackage[top=3cm,bottom=3cm,left=1.5cm,right=1.5cm]{geometry}"
  , "\\usepackage[hidelinks]{hyperref}"
  , "\\usepackage{graphicx}"
  , "\\usepackage{pifont}"
  , "\\usepackage{fancyhdr}"
  , "\\usepackage{tikz}"
  , ""
  , "\\usetikzlibrary{shapes, calc}"
  , ""
  , "\\pagestyle{fancy}"
  , "\\fancyhf{}"
  , "\\fancyhead[L]{\\large \\textbf{" <> header <> "}}"
  , "\\fancyhead[C]{\\large Topology Type: \\texttt{"
      <> maybe "unknown" show mTopologyType <> "}}"
  , "\\fancyhead[R]{\\large " <> datetime <> "}"
  , "\\renewcommand{\\headrulewidth}{0.4pt}"
  , "\\fancyfoot[L]{\\large\\textit{\\href{"
      <> runref <> "}{" <> runref <> "}}}"
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
  , "  \\begin{tikzpicture}[overlay, xshift=0.30\\textwidth]"
  , "    \\node {\\resizebox{!}{10em}{" <> topTikz <> "}};"
  , "  \\end{tikzpicture}"
  , "\\end{center}"
  , ""
  , "\\vspace{-5em}"
  , ""
  , "\\begin{large}"
  , "  \\begin{tabular}{rl}"
  , "    simulation steps:"
  , "      & " <> show simulationSteps <> " \\\\"
  , "    collected samples:"
  , "      & " <> show simulationSamples <> " \\\\"
  , "    stability detector - framesize:"
  , "      & " <> show stabilityFrameSize <> " \\\\"
  , "    stability detector - margin:"
  , "      & \\textpm\\," <> show stabilityMargin <> " elements \\\\"
  , "    when stable, automatically stop after:"
  , "      & " <> maybe "not used" ((<> " steps") . show) stopAfterStable
               <> " \\\\"
  , "    clock offsets:"
  , "      & " <> intercalate "\\,\\textit{fs}, " (show <$> clockOffsets)
               <> " \\\\"
  , "    startup offsets (\\# clock cycles):"
  , "      & " <> intercalate ", " (show <$> startupOffsets)
               <> " \\\\"
  , "    stable at the end of simulation:"
  , "      & " <> maybe ""
                   ( bool "\\textcolor{green!50!black}{\\ding{51}}"
                          "\\textcolor{red!50!black}{\\ding{55}}"
                   ) stable
               <> " \\\\"
  , "  \\end{tabular}"
  , "\\end{large}"
  , ""
  , "\\vfill"
  , ""
  , "\\begin{center}"
  , "  \\begin{tikzpicture}"
  , "    \\node (clocks) at (0,0) {"
  , "      \\includegraphics[width=.5\\textwidth]{" <> clocksPdf <> "}"
  , "    };"
  , "    \\node (ebs) at (0.5\\textwidth, 0) {"
  , "      \\includegraphics[width=.5\\textwidth]{" <> ebsPdf <> "}"
  , "    };"
  , "    \\node at ($ (clocks.north) + (0,-1) $) {"
  , "      \\textbf{Clocks}"
  , "    };"
  , "    \\node at ($ (ebs.north) + (0,-1) $) {"
  , "      \\textbf{Elastic Buffer Occupancies}"
  , "    };"
  , "  \\end{tikzpicture}"
  , "\\end{center}"
  , ""
  , "~"
  , ""
  , "\\end{document}"
  ]
