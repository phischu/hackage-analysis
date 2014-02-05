module Insertion where

import Common (
    ParsedRepository,traverseRepository,
    PackageName,VersionNumber,PackageInformation,
    ModuleError,Declaration,NameErrors)

import Distribution.ModuleName (ModuleName)

import Data.Map (Map)

import Control.Monad (void)

insertAllPackages :: ParsedRepository -> IO ()
insertAllPackages =
    void . (traverseRepository (\packagename versionnumber packagepath -> do
        packageinformation <- undefined
        modulemap <- undefined
        maybenameerrors <- undefined
        insertPackage packagename versionnumber packageinformation modulemap maybenameerrors))

insertPackage ::
    PackageName ->
    VersionNumber ->
    PackageInformation ->
    Map ModuleName (Either ModuleError [Declaration]) ->
    Maybe NameErrors ->
    IO ()
insertPackage = undefined