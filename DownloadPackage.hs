module DownloadPackage where


import Development.Shake
import Development.Shake.FilePath

import Paths

downloadPackage :: FilePath -> Action ()
downloadPackage out = do
    need ["DownloadPackage.hs"]
    system' "wget" ["-O",out,packageFileNameToUrl out]

packageFileNameToUrl :: FilePath -> String
packageFileNameToUrl path = hackageUrl</>path' where
    path' = joinPath (drop 2 (splitPath path))




