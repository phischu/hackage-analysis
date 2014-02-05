module Insertion where

import Common (
    ParsedRepository,traverseRepository,PackagePath,
    PackageName,VersionNumber,
    PackageInformation(PackageError,PackageInformation),PackageError,
    ModuleError,Declaration,NameErrors,
    loadPackage)

import Distribution.ModuleName (ModuleName)
import Distribution.Package (Dependency)

import Data.Map (Map)
import qualified Data.Map as Map (fromList)

import Control.Monad (void)

insertAllPackages :: ParsedRepository -> IO ()
insertAllPackages =
    void . (traverseRepository (\packagename versionnumber packagepath -> do
        maybepackageinformation <- loadPackage packagepath
        case maybepackageinformation of
            Nothing -> return ()
            Just (PackageError packageerror) -> insertPackageError packagename versionnumber packageerror
            Just (PackageInformation modulenames dependencies) -> do
                modulemap <- loadModuleDeclarations packagepath modulenames >>= return . Map.fromList
                maybenameerrors <- loadNameErrors packagepath
                insertPackage packagename versionnumber dependencies modulemap maybenameerrors))

loadModuleDeclarations :: PackagePath -> [ModuleName] -> IO [(ModuleName,Either ModuleError [Declaration])]
loadModuleDeclarations = undefined

loadNameErrors :: PackagePath -> IO (Maybe NameErrors)
loadNameErrors = undefined

insertPackage ::
    PackageName ->
    VersionNumber ->
    [Dependency] ->
    Map ModuleName (Either ModuleError [Declaration]) ->
    Maybe NameErrors ->
    IO ()
insertPackage = undefined

insertPackageError :: PackageName -> VersionNumber -> PackageError -> IO ()
insertPackageError = undefined

