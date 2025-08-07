-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Hitl.Driver.ClockControl.Plot.Report where

import Prelude

import Bittide.ClockControl.Config (CcConf (topology))
import Bittide.ClockControl.Topology (Topology)
import Bittide.Instances.Hitl.Setup (fpgaSetup)
import Control.Applicative (Alternative ((<|>)))
import Control.Monad (void)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO (hClose, hPutStr)
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import System.Process (readProcess)

import qualified Bittide.ClockControl.Topology as Topology
import qualified Clash.Prelude as C

renderLatex :: FilePath -> String -> IO ()
renderLatex path content = do
  withSystemTempDirectory "bittide-report" $ \tmpDir -> do
    let
      texPath = tmpDir </> "report.tex"
      pdfPath = tmpDir </> "report.pdf"
    writeFile texPath content
    void $ readProcess "lualatex" ["--output-directory=" <> tmpDir, texPath] ""

    -- Use the system utility 'mv' instead of 'renameFile' because 'renameFile'
    -- does not work across filesystem boundaries, which breaks CI.
    void $ readProcess "mv" [pdfPath, path] ""

data ToLatexArgs = ToLatexArgs
  { url :: Maybe String
  , header :: Maybe String
  , clocksPdf :: FilePath
  , dataCountsPdf :: FilePath
  , ccConf :: CcConf Topology
  }

asLatex :: ToLatexArgs -> IO String
asLatex args = do
  now <- iso8601Show <$> getCurrentTime

  reportUrl <- lookupEnv "REPORT_URL"
  reportName <- lookupEnv "REPORT_NAME"

  let
    header = fromMaybe "Clock control report" (reportName <|> args.header)
    url = fromMaybe "" (reportUrl <|> args.url)
    fpgaNames0 = C.toList (C.map fst fpgaSetup)
    fpgaNames1 = take (Topology.size args.ccConf.topology) fpgaNames0
    ids = zip [(0 :: Int) ..] fpgaNames1
    fpgaTable =
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

  topologyAsTikz <- topologyToTex args.ccConf.topology

  pure $
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
          <> show args.ccConf.topology.type_
          <> "}}"
      , "\\fancyhead[R]{\\large " <> now <> "}"
      , "\\renewcommand{\\headrulewidth}{0.4pt}"
      , "\\fancyfoot[L]{\\large\\textit{\\href{"
          <> url
          <> "}{"
          <> url
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
      , topologyAsTikz
      , "}};"
      , fpgaTable
      , "\\end{tikzpicture}"
      , "};"
      , "\\end{tikzpicture}"
      , "\\end{center}"
      , ""
      , "\\vspace{-5em}"
      , ""
      , "\\begin{large}"
      , "  \\begin{tabular}{rl}"
      , "  \\end{tabular}"
      , "\\end{large}"
      , ""
      , "\\vfill"
      , ""
      , "\\begin{center}"
      , "  \\begin{tikzpicture}"
      , "    \\node (clocks) at (0,0) {"
      , "      \\includegraphics[width=.49\\textwidth]{" <> args.clocksPdf <> "}"
      , "    };"
      , "    \\node (ebs) at (0.51\\textwidth, 0) {"
      , "      \\includegraphics[width=.49\\textwidth]{" <> args.dataCountsPdf <> "}"
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
      , "  \\end{tikzpicture}"
      , "\\end{center}"
      , ""
      , "~"
      , ""
      , "\\end{document}"
      ]

-- | Convert a 'Topology' to a LaTeX representation using TikZ.
topologyToTex :: Topology -> IO String
topologyToTex = dotToTex . Topology.toDot

-- | Convert a DOT representation of a topology to LaTeX using dot2tex.
dotToTex :: String -> IO String
dotToTex dot =
  withSystemTempFile "topology" $ \path handle -> do
    hPutStr handle dot
    hClose handle

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
      , path
      ]
      ""
