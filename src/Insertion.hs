{-# LANGUAGE OverloadedStrings #-}
module Insertion where

import Common (
    ParsedRepository,traverseRepository,PackagePath,
    PackageName,VersionNumber,
    PackageInformation(PackageError,PackageInformation),PackageError,loadPackage,
    ModuleInformation(ModuleError,ModuleInformation),ModuleError(..),loadModuleInformation,
    loadDeclarations,Declaration,NameErrors,loadNameErrors)

import Web.Neo (NeoT,defaultRunNeoT,cypher)

import Distribution.ModuleName (ModuleName)
import Distribution.Package (Dependency(Dependency))
import Distribution.Text (display)

import Data.Aeson (object,(.=))

import Data.Map (Map,traverseWithKey,mapEither)
import qualified Data.Map as Map (fromList)

import Control.Monad (void,forM,forM_)

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
loadModuleDeclarations packagepath modulenames = forM modulenames (\modulename -> do
    maybemoduleinformation <- loadModuleInformation packagepath modulename
    case maybemoduleinformation of
        Nothing -> return (modulename,Left ModuleInformationFileError)
        Just (ModuleError moduleerror) -> return (modulename,Left moduleerror)
        Just (ModuleInformation _) -> do
            maybedeclarations <- loadDeclarations packagepath modulename
            case maybedeclarations of
                Nothing -> return (modulename,Left DeclarationsFileError)
                Just declarations -> return (modulename,Right declarations))

insertPackage ::
    PackageName ->
    VersionNumber ->
    [Dependency] ->
    Map ModuleName (Either ModuleError [Declaration]) ->
    Maybe NameErrors ->
    IO ()
insertPackage packagename versionnumber dependencies modulemap maybenameerrors = defaultRunNeoT (do
    insertDependencies packagename versionnumber dependencies
    let (moduleerrormap,declarationsmap) = splitModuleMap modulemap
    traverseWithKey (\modulename declarations -> insertDeclarations packagename versionnumber modulename declarations) declarationsmap
    traverseWithKey (\modulename moduleerror -> insertModuleError packagename versionnumber modulename moduleerror) moduleerrormap
    insertNameErrors packagename versionnumber maybenameerrors
    return ()) >>= print

insertPackageError :: PackageName -> VersionNumber -> PackageError -> IO ()
insertPackageError packagename versionnumber packageerror = return ()

insertDependencies :: (Monad m) => PackageName -> VersionNumber -> [Dependency] -> NeoT m ()
insertDependencies packagename versionnumber dependencies = do
    forM_ dependencies (\(Dependency dependencyname _) -> do
        cypher
            "MERGE (rootnode:ROOTNODE)\
            \CREATE UNIQUE (rootnode)-[:PACKAGE]->(package:Package {packagename : {packagename}})\
            \CREATE UNIQUE (package)-[:VERSION]->(version:Version {versionnumber : {versionnumber}})\
            \CREATE UNIQUE (rootnode)-[:PACKAGE]->(otherpackage:Package {packagename : {dependencyname}})\
            \CREATE UNIQUE (version)-[:DEPENDENCY]->(otherpackage)"
            (object [
                "packagename" .= packagename,
                "versionnumber" .= display versionnumber,
                "dependencyname" .= dependencyname]))

splitModuleMap :: Map ModuleName (Either ModuleError [Declaration]) -> (Map ModuleName ModuleError,Map ModuleName [Declaration])
splitModuleMap = mapEither id

insertDeclarations :: (Monad m) => PackageName -> VersionNumber -> ModuleName -> [Declaration] -> NeoT m ()
insertDeclarations packagename versionnumber modulename declarations = return ()

insertModuleError :: (Monad m) => PackageName -> VersionNumber -> ModuleName -> ModuleError -> NeoT m ()
insertModuleError packagename versionnumber modulename moduleerror = return ()

insertNameErrors :: (Monad m) => PackageName -> VersionNumber -> Maybe NameErrors -> NeoT m ()
insertNameErrors packagename versionnumber maybenameerrors = return ()

