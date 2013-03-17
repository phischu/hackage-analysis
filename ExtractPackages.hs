module ExtractPackages where


import Development.Shake
import Development.Shake.FilePath

import Paths


extractPackages :: FilePath -> Action ()
extractPackages out = do
    need ["ExtractPackages.hs"]
    need [wd</>"PackageList.txt"]
    packages <- readFileLines (wd</>"PackageList.txt")
    need (map packageToExtractedName packages)
    writeFile' out ""

packageToExtractedName :: String -> FilePath
packageToExtractedName package = wd</>"PackageArchives"</>name</>version</>name++"-"++version<.>"extracted"where
    (name,version) = read package



