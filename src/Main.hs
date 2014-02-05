module Main where

import Repository (loadRepository)
import ParsePackage (parseAndSaveAllPackages)
import NameResolution (resolveAndSaveAllPackageNames)
import Fragmentation (splitAndSaveAllDeclarations)
import Insertion (insertAllPackages)

main :: IO ()
main = do
    sourcerepository <- loadRepository
    parsedrepository <- parseAndSaveAllPackages sourcerepository
    resolveAndSaveAllPackageNames parsedrepository
    splitAndSaveAllDeclarations parsedrepository
    insertAllPackages parsedrepository
    putStrLn "done"
