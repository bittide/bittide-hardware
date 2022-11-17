-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

-- | Shake utilities related to Clash. Although 'clash-shake' already exists, it
-- assumes Clash runs in non-project mode. We should discuss with the author if
-- he sees a way of upstreaming the code in this module.
--
module Clash.Shake.Extra where

import Prelude

import Clash.Annotations.Primitive (HDL (Verilog))
import Development.Shake.FilePath ((</>))
import Data.Char (toLower)

import qualified Language.Haskell.TH.Syntax as TH
import qualified Language.Haskell.TH.Extra as TH
import Development.Shake
import Clash.Shake.Vivado (LocatedManifest (LocatedManifest))
import Development.Shake.Extra (decodeFile)

hdlToFlag :: HDL -> String
hdlToFlag = ("--" <>) . map toLower . show

-- | Generate command to run and arguments to supply for a top entity passed as
-- a @TemplateHaskell@ name. Generates the expected location of a Clash manifest
-- file.
clashCmd ::
  -- | Build directory
  FilePath ->
  -- | HDL to compile to
  HDL ->
  -- | Entity to compile
  TH.Name ->
  -- | Extra arguments to pass to Clash
  [String] ->
  -- (command, arguments)
  (String, [String])
clashCmd buildDir hdl topName extraArgs =
  ( "cabal"
  , [ "run"
    , pkgName <> ":clash"
    , "--"
    , modName
    , "-fclash-hdldir", buildDir
    , "-main-is", funcName
    , hdlToFlag hdl
    , "-fclash-clear"
    , "-fclash-spec-limit=100"
    ] <> extraArgs
  )
 where
  (pkgName, modName, funcName) = TH.splitName topName

defaultClashCmd :: FilePath -> TH.Name -> (String, [String])
defaultClashCmd buildDir topName = clashCmd buildDir Verilog topName []

-- | Given a top entity name, return expected location of Clash manifest file.
getManifestLocation :: FilePath -> TH.Name -> String
getManifestLocation buildDir topName =
  buildDir </> show topName </> "clash-manifest.json"

decodeLocatedManifest :: FilePath -> Action LocatedManifest
decodeLocatedManifest path = LocatedManifest path <$> decodeFile path
