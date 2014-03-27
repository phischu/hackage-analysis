{-# LANGUAGE OverloadedStrings #-}
module Insertion where

import Common (
    ParsedRepository,traverseRepository,PackagePath,
    PackageName,VersionNumber,
    PackageInformation(PackageError,PackageInformation),PackageError,loadPackage,
    ModuleInformation(ModuleError,ModuleInformation),ModuleError(..),loadModuleInformation,
    loadDeclarations,Declaration(Declaration),NameErrors(NameErrors),loadNameErrors,
    PackageGraph)

import PropertyGraph (
    PG,PropertyGraph,runPropertyGraph)

import Language.Haskell.Names (
    Symbols(Symbols),SymValueInfo(..),SymTypeInfo(..),OrigName(OrigName),GName(GName))

import Distribution.ModuleName (ModuleName)
import Distribution.Package (Dependency(Dependency))
import Distribution.Version (withinRange)
import qualified Distribution.Package as Cabal (PackageName(PackageName))
import Distribution.Text (display)

import Data.Graph.Inductive (Node)
import qualified Data.Graph.Inductive as Gr (empty)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Map (Map)
import qualified Data.Map as Map (fromList,lookup,keys,empty,mapEither,toList)
import Data.Text (Text)

import Control.Monad (forM)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict (StateT,execStateT)


insertAllPackages :: ParsedRepository -> IO PropertyGraph
insertAllPackages parsedrepository = runPropertyGraph (flip traverseRepository parsedrepository (
    \packagename versionnumber packagepath -> do
        maybepackageinformation <- liftIO (loadPackage packagepath)
        case maybepackageinformation of
            Nothing -> return ()
            Just (PackageError packageerror) -> insertPackageError packagename versionnumber packageerror
            Just (PackageInformation modulenames dependencies) -> do
                modulemap <- liftIO (loadModuleDeclarations packagepath modulenames >>= return . Map.fromList)
                maybenameerrors <- liftIO (loadNameErrors packagepath)
                let actualdependencies = concatMap (lookupActualDependencies parsedrepository) dependencies
                insertPackage packagename versionnumber actualdependencies modulemap maybenameerrors))

insertPackage ::
    (Monad m) =>
    PackageName ->
    VersionNumber ->
    [ActualDependency] ->
    Map ModuleName (Either ModuleError [Declaration]) ->
    Maybe NameErrors ->
    PG m ()
insertPackage packagename versionnumber actualdependencies modulemap maybenameerrors = undefined

insertPackageError :: (Monad m) => PackageName -> VersionNumber -> PackageError -> PG m ()
insertPackageError = undefined

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

type ActualDependency = (PackageName,VersionNumber)

lookupActualDependencies :: ParsedRepository -> Dependency -> [ActualDependency]
lookupActualDependencies parsedrepository (Dependency (Cabal.PackageName packagename) versionrange) =
    case Map.lookup packagename parsedrepository of
        Nothing -> []
        Just versionmap -> [(packagename,versionnumber) |
            versionnumber <- Map.keys versionmap,withinRange versionnumber versionrange]

splitModuleMap :: Map ModuleName (Either ModuleError [Declaration]) -> (Map ModuleName ModuleError,Map ModuleName [Declaration])
splitModuleMap = Map.mapEither id

{-

import Common (
    ParsedRepository,traverseRepository,PackagePath,
    PackageName,VersionNumber,
    PackageInformation(PackageError,PackageInformation),PackageError,loadPackage,
    ModuleInformation(ModuleError,ModuleInformation),ModuleError(..),loadModuleInformation,
    loadDeclarations,Declaration(Declaration),NameErrors(NameErrors),loadNameErrors)

import Language.Haskell.Names (
    Symbols(Symbols),SymValueInfo(..),SymTypeInfo(..),OrigName(OrigName),GName(GName))

import Distribution.ModuleName (ModuleName)
import Distribution.Package (Dependency(Dependency))
import Distribution.Version (withinRange)
import qualified Distribution.Package as Cabal (PackageName(PackageName))
import Distribution.Text (display)

import Data.Graph.Inductive.PatriciaTree (Gr)

import Data.Text (Text)

import Data.Map (Map,mapWithKey,mapEither)
import qualified Data.Map as Map (fromList,toList,lookup,keys)
import qualified Data.Set as Set (toList)

import Data.Foldable (fold)
import Control.Monad (void,forM,forM_,when)


symModuleData :: OrigName -> String
symModuleData (OrigName _ (GName originalmodulename _)) = originalmodulename

symNameData :: OrigName -> String
symNameData (OrigName _ (GName _ symbolname)) = symbolname

symValueGenre :: SymValueInfo a -> Text
symValueGenre (SymValue _ _) = "Value"
symValueGenre (SymMethod _ _ _) = "Method"
symValueGenre (SymSelector _ _ _ _) = "Selector"
symValueGenre (SymConstructor _ _ _) = "Constructor"

symTypeGenre :: SymTypeInfo a -> Text
symTypeGenre (SymType _ _) = "Type"
symTypeGenre (SymData _ _) = "Data"
symTypeGenre (SymNewType _ _) = "Newtype"
symTypeGenre (SymTypeFam _ _) = "TypeFamily"
symTypeGenre (SymDataFam _ _) = "DataFamily"
symTypeGenre (SymClass _ _) = "Class"

-}