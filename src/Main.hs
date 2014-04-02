module Main where

import Repository (loadRepository)
import ParsePackage (parseAndSaveAllPackages)
import NameResolution (resolveAndSaveAllPackageNames)
import Fragmentation (splitAndSaveAllDeclarations)
import Insertion (insertAllPackages)
import PropertyGraph (PropertyGraph(PropertyGraph),graphviz)

main :: IO ()
main = do
    sourcerepository <- loadRepository
    parsedrepository <- parseAndSaveAllPackages sourcerepository
    resolveAndSaveAllPackageNames parsedrepository
    splitAndSaveAllDeclarations parsedrepository
    propertygraph <- insertAllPackages parsedrepository
    let PropertyGraph _ _ _ _ n = propertygraph
    putStrLn ("Number of nodes: " ++ show n)
    writeFile "graph.gv" (graphviz propertygraph)
    putStrLn "done"

