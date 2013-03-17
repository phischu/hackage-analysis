module ExtractPackage where


import Development.Shake
import Development.Shake.FilePath

import Paths

extractPackage :: FilePath -> Action ()
extractPackage out = do
    need ["ExtractPackage.hs"]
    writeFile' out []





