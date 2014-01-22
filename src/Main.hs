module Main where

import Repository (loadRepository)
import Instances (parseAllPackages)

main :: IO ()
main = do
    repository <- loadRepository
    parseAllPackages repository
    print "hallo"
