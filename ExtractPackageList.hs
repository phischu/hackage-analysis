module ExtractPackageList where

import Development.Shake
import Development.Shake.FilePath

import Distribution.Hackage.DB
import Distribution.Text

import qualified Data.Map as M

import Paths

extractPackageList :: FilePath -> Action ()
extractPackageList out = do
    need ["ExtractPackageList.hs","Paths.hs"]
    need [wd</>"00-index.tar.gz"]
    systemCwd wd "gunzip" ["00-index.tar.gz"]
    hackage <- liftIO (readHackage' (wd</>"00-index.tar"))
    let packagenames = [name++"-"++show (disp version)| name <- M.keys hackage, version <- M.keys (hackage M.! name)]
    writeFileLines out packagenames

