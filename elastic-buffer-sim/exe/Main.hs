{-# LANGUAGE NoTemplateHaskell #-}

module Main (main) where

import Bittide.Topology

import Options.Applicative qualified as OA

main :: IO ()
main = run =<< OA.execParser full
 where
  full :: OA.ParserInfo Cmd
  full = OA.info (OA.helper <*> p) OA.fullDesc
  p = OA.hsubparser
    (OA.command "csv"
      (OA.info (DumpCsv <$> steps)
      (OA.progDesc "Dump to .csv")))

steps :: OA.Parser Int
steps = OA.argument OA.auto
    (OA.metavar "STEPS"
    <> OA.help "Number of iterations/steps to simulate")

run :: Cmd -> IO ()
run (DumpCsv n) = dumpCsv n

data Cmd = DumpCsv !Int
