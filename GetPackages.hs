module Main where

import Distribution.Hackage.DB hiding (map)
import Distribution.PackageDescription
import qualified Data.Map as M
import Distribution.Text

import Control.Proxy

packageNames :: (Proxy p,Monad m) => Hackage -> () -> Producer p String m ()
packageNames hackage = fromListS $ [name ++ "-" ++ show (disp version) | name <- M.keys hackage, version <- M.keys (hackage M.! name)]

main :: IO ()
main = do
    hackage <- readHackage
    print $ runWriter $ runProxy $ packageNames hackage >-> lengthD

