module ExtractPackage where


import Development.Shake
import Development.Shake.FilePath

import Paths

extractPackage :: FilePath -> Action ()
extractPackage out = do
    need ["ExtractPackage.hs"]
    need [filenameToArchiveName out]
    system' "tar" ["xzf",filenameToArchiveName out,"-C",targetDirectory out]
    writeFile' out []

filenameToArchiveName :: FilePath -> FilePath
filenameToArchiveName = undefined

targetDirectory :: FilePath -> FilePath
targetDirectory = undefined



