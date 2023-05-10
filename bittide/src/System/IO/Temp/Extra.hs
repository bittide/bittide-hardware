-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module System.IO.Temp.Extra(withTempBinaryFile) where

import Prelude

import Control.Monad.IO.Class
import System.Directory
import System.IO

import qualified Control.Monad.Catch as MC

-- | Create, open, and use a temporary binary file in the given directory.
--
-- The temp file is deleted after use.
withTempBinaryFile ::
  (MonadIO m, MC.MonadMask m) =>
  -- | Parent directory to create the file in
  FilePath ->
  -- | File name template
  String ->
  -- | Callback that can use the file
  (FilePath -> Handle -> m a) ->
  -- | Callback result
  m a
withTempBinaryFile tmpDir template action =
  MC.bracket
    (liftIO (openBinaryTempFile tmpDir template))
    (\(name, handle) -> liftIO (hClose handle >> ignoringIOErrors (removeFile name)))
    (uncurry action)

ignoringIOErrors :: MC.MonadCatch m => m () -> m ()
ignoringIOErrors ioe = ioe `MC.catch` (\e -> const (return ()) (e :: IOError))
