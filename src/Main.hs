module Main where

import Repository (loadRepository)
import ParsePackage (parseAndSaveAllPackages)
import NameResolution (resolveAndSaveAllPackageNames)
import Fragmentation (splitAndSaveAllDeclarations)
import Insertion (insertAllPackages)

import System.Process (system)
import System.Directory (removeDirectoryRecursive,createDirectory)
import Control.Concurrent (threadDelay)

import Control.Monad (void)

main :: IO ()
main = do
    sourcerepository <- loadRepository
    parsedrepository <- parseAndSaveAllPackages sourcerepository
    resolveAndSaveAllPackageNames parsedrepository
    splitAndSaveAllDeclarations parsedrepository
    resetDatabase
    insertAllPackages parsedrepository
    putStrLn "done"

resetDatabase :: IO ()
resetDatabase = do
    void (system "neo4j-community-2.0.1/bin/neo4j stop")
    removeDirectoryRecursive "neo4j-community-2.0.1/data/"
    createDirectory "neo4j-community-2.0.1/data"
    void (system "neo4j-community-2.0.1/bin/neo4j start")
    threadDelay 1000000

