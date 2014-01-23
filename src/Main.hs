module Main where

import Repository (loadRepository)
import ParsePackage (parseAndSaveAllPackages)
import NameResolution (resolveAndSaveAllPackageNames)

main :: IO ()
main = do
    sourcerepository <- loadRepository
    parsedrepository <- parseAndSaveAllPackages sourcerepository
    resolveAndSaveAllPackageNames parsedrepository
    putStrLn "done"
