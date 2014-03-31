{-# LANGUAGE OverloadedStrings #-}
module Insertion where

import Common (
    ParsedRepository,traverseRepository,PackagePath,
    PackageName,VersionNumber,
    PackageInformation(PackageError,PackageInformation),PackageError,loadPackage,
    ModuleInformation(ModuleError,ModuleInformation),ModuleError(..),loadModuleInformation,
    loadDeclarations,Declaration(Declaration),NameErrors(NameErrors),loadNameErrors)

import PropertyGraph (
    PG,PropertyGraph,Label,runPropertyGraph,Node,rootnode,start,newEdgeTo,newNextLabeled)

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
    let (moduleerrormap,declarationsmap) = splitModuleMap modulemap
    forM_ (Map.toList declarationsmap) (flip insertModuleNode versionnode)

newProperty :: (Monad m) => Text -> Text -> Node -> PG m Node
newProperty propertyname propertyvalue = newNextLabeled propertyname >=> newNextLabeled propertyvalue

insertPackageNode :: (Monad m) => PackageName -> PG m Node
insertPackageNode packagename = do
    packagenode <- start rootnode >>= newNextLabeled "Package"
    start packagenode >>= newProperty "packagename" (pack packagename)
    return packagenode

insertVersionNode :: (Monad m) => VersionNumber -> Node -> PG m Node
insertVersionNode versionnumber packagenode = do
    versionnode <- start packagenode >>= newNextLabeled "Version"
    start versionnode >>= newProperty "versionnumber" (pack (display versionnumber))
    return versionnode

insertDependency :: (Monad m) => ActualDependency -> Node -> PG m ()
insertDependency (dependencypackagename,dependencyversionnumber) versionnode = do
    dependencyversionnode <- insertPackageNode dependencypackagename >>= insertVersionNode dependencyversionnumber
    start versionnode >>= newNextLabeled "Dependency" >>= newEdgeTo dependencyversionnode

insertModuleNode :: (Monad m) => (ModuleName,[Declaration]) -> Node -> PG m ()
insertModuleNode (modulename,declarations) versionnode = do
    modulenode <- newModuleNode modulename versionnode
    forM_ declarations (flip insertDeclaration modulenode)

insertDeclaration :: (Monad m) => Declaration -> Node -> PG m ()
insertDeclaration (Declaration declarationgenre declarationast declaredsymbols usedsymbols) modulenode = do
    declarationnode <- start modulenode >>= newNextLabeled "Declaration"
    start declarationnode >>= newProperty "declarationgenre" (pack (show declarationgenre))
    start declarationnode >>= newProperty "declarationast" (pack declarationast)
    return ()

newModuleNode :: (Monad m) => ModuleName -> Node -> PG m Node
newModuleNode modulename versionnode = do
    modulenode <- start versionnode >>= newNextLabeled "Module"
    start modulenode >>= newProperty "modulename" (pack (display modulename))
    return modulenode

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