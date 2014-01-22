module Instances where

import Types (Repository,PackageName,VersionNumber,Instance)

import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.PackageDescription.Configuration (finalizePackageDescription)
import Distribution.Verbosity (silent)
import Distribution.System (Platform(Platform),Arch(I386),OS(Linux))
import Distribution.Compiler (CompilerId(CompilerId),CompilerFlavor(GHC))
import Data.Version (Version(Version))

import Data.Map (Map,traverseWithKey)


parseAllPackages :: Repository -> IO (Map PackageName (Map VersionNumber Instance))
parseAllPackages = traverseWithKey (\packagename ->
    traverseWithKey (\_ -> parsePackage packagename))

parsePackage :: PackageName -> FilePath -> IO Instance
parsePackage packagename filepath = do
    let cabalfilepath = filepath ++ packagename ++ ".cabal"
    genericpackagedescription <- readPackageDescription silent cabalfilepath
    let packagedescription = finalizePackageDescription [] (const True) defaultPlatform defaultCompiler [] genericpackagedescription
    putStrLn ("Parsing " ++ packagename ++ " at " ++ filepath)
    return undefined

defaultPlatform :: Platform
defaultPlatform = Platform I386 Linux

defaultCompiler :: CompilerId
defaultCompiler = CompilerId GHC (Version [7,6,3] [])

