module DownloadPackages where


import Development.Shake
import Development.Shake.FilePath

import Paths


downloadPackages :: FilePath -> Action ()
downloadPackages out = do
    need ["DownloadPackages.hs"]
    need [wd</>"PackageList.txt"]
    packages <- readFileLines (wd</>"PackageList.txt")
    need (map packageToFileName packages)
    writeFile' out ""

packageToFileName :: String -> FilePath
packageToFileName package = wd</>"PackageArchives"</>name</>version</>name++"-"++version<.>"tar"<.>"gz"where
    (name,version) = read package



