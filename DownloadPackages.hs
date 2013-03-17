module DownloadPackages where


import Development.Shake


downloadPackages :: FilePath -> Action ()
downloadPackages out = do
    need ["DownloadPackages.hs"]
    return ()





