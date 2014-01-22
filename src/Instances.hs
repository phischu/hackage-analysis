module Instances where

import Types (Repository,PackageName,VersionNumber,Instance)

import Distribution.PackageDescription (
    GenericPackageDescription,PackageDescription,FlagAssignment)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.PackageDescription.Configuration (finalizePackageDescription)
import Distribution.Verbosity (silent)
import Distribution.System (Platform(Platform),Arch(I386),OS(Linux))
import Distribution.Compiler (CompilerId(CompilerId),CompilerFlavor(GHC))
import Data.Version (Version(Version))
import Distribution.Package (Dependency)

import Control.Error (EitherT,runEitherT,hoistEither,fmapLT,scriptIO)

import Data.Map (Map,traverseWithKey)

data PackageError =
    PackageFinalizationError [Dependency] |
    PackageReadingError String


parseAllPackages :: Repository -> IO (Map PackageName (Map VersionNumber (Either PackageError Instance)))
parseAllPackages = traverseWithKey (\packagename ->
    traverseWithKey (\_ -> parsePackage packagename))

parsePackage :: PackageName -> FilePath -> IO (Either PackageError Instance)
parsePackage packagename filepath = runEitherT (do

    let cabalfilepath = filepath ++ packagename ++ ".cabal"

    genericpackagedescription <- scriptIO (readPackageDescription silent cabalfilepath)
        `onFailure` PackageReadingError

    (packagedescription,_) <- hoistEither (simpleConfigure genericpackagedescription)
        `onFailure` PackageFinalizationError

    return undefined)

defaultPlatform :: Platform
defaultPlatform = Platform I386 Linux

defaultCompiler :: CompilerId
defaultCompiler = CompilerId GHC (Version [7,6,3] [])

simpleConfigure :: GenericPackageDescription -> Either [Dependency] (PackageDescription,FlagAssignment)
simpleConfigure = finalizePackageDescription [] (const True) defaultPlatform defaultCompiler []

onFailure :: Monad m => EitherT a m r -> (a -> b) -> EitherT b m r
onFailure = flip fmapLT
