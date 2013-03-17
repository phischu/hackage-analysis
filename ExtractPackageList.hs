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
    systemCwd wd "gunzip" ["-f","00-index.tar.gz"]
    hackage <- liftIO (readHackage' (wd</>"00-index.tar"))
    let packagenames = [show (name,show (disp version))| name <- M.keys hackage, version <- M.keys (hackage M.! name)]
    writeFileChanged out (unlines (every 100 packagenames))

every :: Int -> [a] -> [a]
every nth [] = []
every nth xs = head xs : every nth (drop nth xs)

