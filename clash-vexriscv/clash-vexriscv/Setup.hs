-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

import Data.Maybe
import Distribution.PackageDescription hiding (Flag)
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import System.Directory

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { preConf = makeExtLib,
        confHook = \a f -> confHook simpleUserHooks a f >>= updateExtraLibDirs
      }

makeExtLib :: Args -> ConfigFlags -> IO HookedBuildInfo
makeExtLib _ flags = do
  let verbosity = fromFlag $ configVerbosity flags
  rawSystemExit
    verbosity
    "env"
    ["make"]
  return emptyHookedBuildInfo

updateExtraLibDirs :: LocalBuildInfo -> IO LocalBuildInfo
updateExtraLibDirs localBuildInfo = do
  let packageDescription = localPkgDescr localBuildInfo
      lib = fromJust $ library packageDescription
      libBuild = libBuildInfo lib
  dir <- getCurrentDirectory
  return
    localBuildInfo
      { localPkgDescr =
          packageDescription
            { library =
                Just $
                  lib
                    { libBuildInfo =
                        libBuild
                          { extraLibDirs =
                              (dir ++ "/build_out_dir") :
                              extraLibDirs libBuild
                          }
                    }
            }
      }
