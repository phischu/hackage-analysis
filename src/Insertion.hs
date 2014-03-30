{-# LANGUAGE OverloadedStrings #-}
module Insertion where

import Common (
    ParsedRepository,traverseRepository,PackagePath,
    PackageName,VersionNumber,
    PackageInformation(PackageError,PackageInformation),PackageError,loadPackage,
    ModuleInformation(ModuleError,ModuleInformation),ModuleError(..),loadModuleInformation,
    loadDeclarations,Declaration(Declaration),NameErrors(NameErrors),loadNameErrors)

import PropertyGraph (
    PG,PropertyGraph,runPropertyGraph,Node,unique,rootnode,next,newNext,start,has,newLinkTo)

import Language.Haskell.Names (
    Symbols(Symbols),SymValueInfo(..),SymTypeInfo(..),OrigName(OrigName),GName(GName))

import Distribution.ModuleName (ModuleName)
import Distribution.Package (Dependency(Dependency))
import Distribution.Version (withinRange)
import qualified Distribution.Package as Cabal (PackageName(PackageName))
import Distribution.Text (display)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (fromList,lookup,keys,empty,mapEither,toList,insert)
import Data.Text (Text,pack)

import Control.Monad (forM,forM_,(>=>))

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT,evalStateT,gets,modify)


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
insertPackage packagename versionnumber actualdependencies modulemap maybenameerrors = do
    packagenode <- insertPackageNode packagename
    versionnode <- insertVersionNode versionnumber packagenode
    forM_ actualdependencies (flip insertDependency versionnode)

property :: (Monad m) => Text -> Text -> Node -> PG m Node
property propertyname propertyvalue = next propertyname >=> next propertyvalue

newProperty :: (Monad m) => Text -> Text -> Node -> PG m Node
newProperty propertyname propertyvalue = newNext propertyname >=> newNext propertyvalue

insertPackageNode :: (Monad m) => PackageName -> PG m Node
insertPackageNode packagename = do
    unique (getPackageNode packagename) >>= maybe (newPackageNode packagename) return

getPackageNode :: (Monad m) => PackageName -> PG m Node
getPackageNode packagename = do
    start rootnode >>= next "Package" >>= has (property "packagename" (pack packagename))

newPackageNode :: (Monad m) => PackageName -> PG m Node
newPackageNode packagename = do
    start rootnode >>= newNext "Package" >>= has (newProperty "packagename" (pack packagename))

insertVersionNode :: (Monad m) => VersionNumber -> Node -> PG m Node
insertVersionNode versionnumber packagenode =
    unique (getVersionNode versionnumber packagenode) >>=
        maybe (newVersionNode versionnumber packagenode) return

getVersionNode :: (Monad m) => VersionNumber -> Node -> PG m Node
getVersionNode versionnumber packagenode = do
    start packagenode >>= next "Version" >>= has (property "versionnumber" (pack (display versionnumber)))

newVersionNode :: (Monad m) => VersionNumber -> Node -> PG m Node
newVersionNode versionnumber packagenode = do
    start packagenode >>= newNext "Version" >>= has (newProperty "versionnumber" (pack (display versionnumber)))

insertDependency :: (Monad m) => ActualDependency -> Node -> PG m ()
insertDependency (dependencypackagename,dependencyversionnumber) versionnode = do
    dependencyversionnode <- insertPackageNode dependencypackagename >>= insertVersionNode dependencyversionnumber
    start versionnode >>= newNext "Dependency" >>= newLinkTo dependencyversionnode

insertPackageError :: (Monad m) => PackageName -> VersionNumber -> PackageError -> PG m ()
insertPackageError packagename versionnumber packageerror = return ()

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