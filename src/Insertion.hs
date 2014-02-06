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

import Data.Map (Map,traverseWithKey)
import qualified Data.Map as Map (fromList)

import Control.Monad (void,forM)

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
    cypher
        "MERGE (rootnode:ROOTNODE)\
        \CREATE UNIQUE (rootnode)-[:PACKAGE]->(package {packagename : {packagename}})\
        \CREATE UNIQUE (package)-[:VERSION]->(version {versionnumber : {versionnumber}})\
        \CREATE UNIQUE (rootnode)-[:PACKAGE]->(otherpackage {packagename : {dependencyname}})\
        \CREATE UNIQUE (version)-[:DEPENDENCY]->(otherpackage)"
        (object [
            "packagename" .= packagename,
            "versionnumber" .= display versionnumber,
            "dependencyname" .= map (\(Dependency dependencyname _) -> dependencyname) dependencies])
    return ()

splitModuleMap :: Map ModuleName (Either ModuleError [Declaration]) -> (Map ModuleName ModuleError,Map ModuleName [Declaration])
splitModuleMap = undefined

insertDeclarations :: (Monad m) => PackageName -> VersionNumber -> ModuleName -> [Declaration] -> NeoT m ()
insertDeclarations = undefined

insertModuleError :: (Monad m) => PackageName -> VersionNumber -> ModuleName -> ModuleError -> NeoT m ()
insertModuleError = undefined

insertNameErrors :: (Monad m) => PackageName -> VersionNumber -> Maybe NameErrors -> NeoT m ()
insertNameErrors = undefined

