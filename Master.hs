module Main where

import Development.Shake
import Development.Shake.FilePath

import System.Directory hiding (doesFileExist)


import Paths
import ExtractPackageList
import DownloadPackages
import DownloadPackage
import ExtractPackages
import ExtractPackage


main :: IO ()
main = shakeWithArgs (removeDirectoryRecursive "gen") shakeOptions {shakeThreads = 4} $ do

    want [wd</>"DownloadedPackages"]

    wd</>"00-index.tar.gz" *> \out -> do
        exists <- doesFileExist "00-index.tar.gz"
        if exists then
            copyFile' "00-index.tar.gz" out
        else
            systemCwd wd "wget" [hackageUrl++"00-index.tar.gz"]

    wd</>"PackageList.txt" *> extractPackageList

    wd</>"DownloadedPackages" *> downloadPackages

    wd</>"PackageArchives/*/*/*.tar.gz" *> downloadPackage

    wd</>"PackagesExtracted" *> extractPackages

    wd</>"ExtractedPackages/*/*/*" *> extractPackage














