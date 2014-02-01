module Main where

import Repository (loadRepository)
import ParsePackage (parseAndSaveAllPackages)
import NameResolution (resolveAndSaveAllPackageNames)
import Fragmentation (splitAndSaveAllDeclarations)

main :: IO ()
main = do
    sourcerepository <- loadRepository
    parsedrepository <- parseAndSaveAllPackages sourcerepository
    resolveAndSaveAllPackageNames parsedrepository
    splitAndSaveAllDeclarations parsedrepository
    putStrLn "done"
