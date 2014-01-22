module Instances where

import Types (Repository,PackageName,VersionNumber,Instance)

import Data.Map (Map,traverseWithKey)


parseAllPackages :: Repository -> IO (Map PackageName (Map VersionNumber Instance))
parseAllPackages = traverseWithKey (\packagename ->
    traverseWithKey (\_ -> parsePackage packagename))

parsePackage :: PackageName -> FilePath -> IO Instance
parsePackage packagename filepath = do
    putStrLn ("Parsing " ++ packagename ++ " at " ++ filepath)
    return undefined


