module Instances where

import Types (
    Repository,PackageName,VersionNumber,ModuleAST)

import Distribution.PackageDescription (
    GenericPackageDescription,PackageDescription,FlagAssignment,
    library,libModules)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.PackageDescription.Configuration (finalizePackageDescription)
import Distribution.Verbosity (silent)
import Distribution.System (Platform(Platform),Arch(I386),OS(Linux))
import Distribution.Compiler (CompilerId(CompilerId),CompilerFlavor(GHC))
import Data.Version (Version(Version))
import Distribution.Package (Dependency)
import Distribution.ModuleName (ModuleName)

import Control.Error (
    EitherT,runEitherT,hoistEither,fmapLT,scriptIO,note)

import Control.Monad (forM)

import Data.Map (Map,traverseWithKey,fromList)

data PackageError =
    PackageReadingError String |
    PackageFinalizationError [Dependency] |
    PackageNoLibrary

data ModuleError = ModuleError

parseAllPackages ::
    Repository ->
    IO (
    Map PackageName (
    Map VersionNumber (
        Either PackageError (
        Map ModuleName (
            Either ModuleError
            ModuleAST)))))
parseAllPackages = traverseWithKey (\packagename ->
    traverseWithKey (\_ -> parsePackage packagename))

parsePackage :: PackageName -> FilePath -> IO (Either PackageError (Map ModuleName (Either ModuleError ModuleAST)))
parsePackage packagename packagepath = runEitherT (do

    let cabalfilepath = packagepath ++ packagename ++ ".cabal"

    genericpackagedescription <- scriptIO (readPackageDescription silent cabalfilepath)
        `onFailure` PackageReadingError

    (packagedescription,_) <- hoistEither (simpleConfigure genericpackagedescription)
        `onFailure` PackageFinalizationError

    librarysection <- hoistEither (note () (library packagedescription))
        `onFailure` (const PackageNoLibrary)

    modules <- forM (libModules librarysection) (\modulename -> do

        eithermoduleast <- runEitherT (do

            undefined)

        return (modulename,eithermoduleast))
    return (fromList modules))

defaultPlatform :: Platform
defaultPlatform = Platform I386 Linux

defaultCompiler :: CompilerId
defaultCompiler = CompilerId GHC (Version [7,6,3] [])

simpleConfigure :: GenericPackageDescription -> Either [Dependency] (PackageDescription,FlagAssignment)
simpleConfigure = finalizePackageDescription [] (const True) defaultPlatform defaultCompiler []

onFailure :: Monad m => EitherT a m r -> (a -> b) -> EitherT b m r
onFailure = flip fmapLT
