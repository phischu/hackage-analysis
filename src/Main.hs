module Main where

import Repository (loadRepository)
import ParsePackage (parseAndSaveAllPackages)

main :: IO ()
main = do
    repository <- loadRepository
    parseAndSaveAllPackages repository
    putStrLn "done"
