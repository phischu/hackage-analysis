module Main where

import Distribution.Hackage.DB hiding (map)
import Distribution.PackageDescription
import qualified Data.Map as M
import Distribution.Text
import Network.HTTP

import Control.Proxy

import Data.ByteString.Lazy (ByteString)

import Data.Time (getCurrentTime,UTCTime)

packageNames :: (Proxy p,Monad m) => Hackage -> () -> Producer p (String,String) m ()
packageNames hackage = fromListS $ [(name,show (disp version))| name <- M.keys hackage, version <- M.keys (hackage M.! name)]

main :: IO ()
main = do
    time <- getCurrentTime
    let foldername = "./packages-" ++ timeToName time
    hackage <- readHackage
    runProxy $
            packageNames hackage
        >-> mapMD (\(name,version) -> simpleHTTP (getRequest (nameAndVersionToUrl (name,version))) >>= getResponseBody)
        >-> blah

nameAndVersionToUrl :: (String,String) -> String
nameAndVersionToUrl = undefined

blah :: (Proxy p) => () -> Consumer p ByteString IO ()
blah = undefined


timeToName :: UTCTime -> String
timeToName = ("attempt"++) . replace '.' '-' . replace ':' '-' . replace ' ' '_' . show

replace :: (Eq a) => a -> a -> [a] -> [a]
replace a b = map (\ x -> if x==a then b else x)

