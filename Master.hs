module Main where

import Development.Shake
import Development.Shake.FilePath


import Paths
import ExtractPackageList
import DownloadPackages


main :: IO ()
main = shakeWithArgs (removeFiles "gen" []) shakeOptions $ do

    want [wd</>"getPackages"]

    wd</>"00-index.tar.gz" *> \out -> do
        systemCwd wd "wget" [hackageUrl++"00-index.tar.gz"]

    wd</>"packageList.txt" *> extractPackageList

    wd</>"getPackages" *> downloadPackages





