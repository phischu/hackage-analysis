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
filenameToArchiveName path = wd</>"PackageArchives"</>name</>version</>name++"-"++version<.>"tar"<.>"gz" where
    name = head (drop 2 path)
    version = head (drop 3 path)

targetDirectory :: FilePath -> FilePath
targetDirectory = wd</>"ExtractedPackages"</>name</>version</>name++"-"++version where
    name = head (drop 2 path)
    version = head (drop 3 path)



