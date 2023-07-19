-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Development.Shake.Extra where

import Prelude

import Data.Maybe (fromJust)
import Development.Shake

import qualified Data.Aeson as Aeson

suppressOutput :: [CmdOption]
suppressOutput = [EchoStdout False, EchoStderr True]

decodeFile :: Aeson.FromJSON a => FilePath -> Action a
decodeFile path = do
  need [path]
  fromJust <$> liftIO (Aeson.decodeFileStrict path)
