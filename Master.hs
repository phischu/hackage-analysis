module Main where

import Development.Shake
import Development.Shake.FilePath


import Paths
import ExtractPackageList
import DownloadPackages
import DownloadPackage


main :: IO ()
main = shakeWithArgs (removeFiles "gen" []) shakeOptions $ do

    want [wd</>"DownloadedPackages"]

    wd</>"00-index.tar.gz" *> \out -> do
        systemCwd wd "wget" [hackageUrl++"00-index.tar.gz"]

    wd</>"PackageList.txt" *> extractPackageList

    wd</>"DownloadedPackages" *> downloadPackages

    wd</>"PackageArchives/*/*/*.tar.gz" *> downloadPackage



