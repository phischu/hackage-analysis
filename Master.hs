module Main where

import Development.Shake
import Development.Shake.FilePath

import ExtractPackageList

wd :: FilePath
wd = "gen"

hackageUrl :: String
hackageUrl = "hackage.haskell.org/packages/archive/"


main :: IO ()
main = shakeWithArgs (removeFiles "gen" []) shakeOptions $ do
    want [wd</>"00-index.tar.gz"]

    wd</>"00-index.tar.gz" *> \out -> do
        systemCwd wd "wget" [hackageUrl++"00-index.tar.gz"]

    wd</>"packageList.txt" *> \out -> extractPackageList







