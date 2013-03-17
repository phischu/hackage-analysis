module Main where

import Development.Shake
import Development.Shake.FilePath


import Paths
import ExtractPackageList


main :: IO ()
main = shakeWithArgs (removeFiles "gen" []) shakeOptions $ do
    want [wd</>"packageList.txt"]

    wd</>"00-index.tar.gz" *> \out -> do
        systemCwd wd "wget" [hackageUrl++"00-index.tar.gz"]

    wd</>"packageList.txt" *> extractPackageList







