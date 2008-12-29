module Config.Master (
        module Config.Master,
        module Config.App
        ) where

import Config.App

----------------------------------------------------------------
-- Arguments to the make system used in the Dynamic Loader
----------------------------------------------------------------

compileArgs =
        [ "-fglasgow-exts"
        , "-XOverlappingInstances"
        , "-XUndecidableInstances"
        , "-F", "-pgmFtrhsx"
        , "-fno-warn-overlapping-patterns" 
        , "-odir " ++ compiledDir
        , "-hidir " ++ compiledDir
        , "-package HDBC"
        ] ++ (map ("-i"++) searchDirs)

mUserPkgConf = [""]

----------------------------------------------------------------
-- Paths
----------------------------------------------------------------

modelDir       = "App/Models"
viewDir        = "App/Views"
viewStub       = "Turbinado/Stubs/View.hs"
layoutDir      = "App/Layouts"
layoutStub     = "Turbinado/Stubs/Layout.hs"
controllerDir  = "App/Controllers"
controllerStub = "Turbinado/Stubs/Controller.hs"

configDir = "Config"
searchDirs = [modelDir, viewDir, layoutDir, controllerDir, rootDir, configDir, compiledDir]

staticDirs = ["static", "tmp/cache"]
compiledDir = "tmp/compiled"

rootDir = "./"
