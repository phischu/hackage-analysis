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
    PG,PropertyGraph,runPropertyGraph,Properties,
    newNode,newEdge,
    unique,suc,has,properties,strain)

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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (fromList,lookup,keys,empty,mapEither,toList,insert)
import Data.Text (Text,pack)

import Control.Monad (forM,(>=>))

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT,evalStateT,gets,modify)

data Indices = Indices {
    packageIndex :: Map Properties Node,
    symbolIndex :: Map Properties Node}

emptyIndices :: Indices
emptyIndices = Indices Map.empty Map.empty

insertAllPackages :: ParsedRepository -> IO PropertyGraph
insertAllPackages parsedrepository = flip evalStateT emptyIndices (runPropertyGraph (flip traverseRepository parsedrepository (
    \packagename versionnumber packagepath -> do
        maybepackageinformation <- liftIO (loadPackage packagepath)
        case maybepackageinformation of
            Nothing -> return ()
            Just (PackageError packageerror) -> insertPackageError packagename versionnumber packageerror
            Just (PackageInformation modulenames dependencies) -> do
                modulemap <- liftIO (loadModuleDeclarations packagepath modulenames >>= return . Map.fromList)
                maybenameerrors <- liftIO (loadNameErrors packagepath)
                let actualdependencies = concatMap (lookupActualDependencies parsedrepository) dependencies
                insertPackage packagename versionnumber actualdependencies modulemap maybenameerrors)))

insertPackage ::
    (Monad m) =>
    PackageName ->
    VersionNumber ->
    [ActualDependency] ->
    Map ModuleName (Either ModuleError [Declaration]) ->
    Maybe NameErrors ->
    PG (StateT Indices m) ()
insertPackage packagename versionnumber actualdependencies modulemap maybenameerrors = do
    packagenode <- insertPackageNode packagename
    versionnode <- insertVersionNode packagenode versionnumber
    forM actualdependencies (\(depdencypackagename,dependencyversionnumber) -> do
        depedencypackagenode <- insertPackageNode depdencypackagename
        dependencyversionnode <- insertVersionNode depedencypackagenode dependencyversionnumber
        newEdge "DEPENDENCY" versionnode dependencyversionnode)
    let (moduleerrormap,declarationsmap) = splitModuleMap modulemap
    return ()

merge :: (Monad m) => PG m (Maybe a) -> PG m a -> PG m a
merge pgm pg = pgm >>= maybe pg return

insertPackageNode :: (Monad m) => PackageName -> PG (StateT Indices m) Node
insertPackageNode packagename = do
    packageindex <- lift (lift (gets packageIndex))
    let packageproperties = Map.fromList [("packagename",pack packagename)]
    merge (return (Map.lookup packageproperties packageindex)) (do
        pn <- newNode packageproperties
        let packageindex' = Map.insert packageproperties pn packageindex
        lift (lift (modify (\(Indices _ symbolindex) -> Indices packageindex' symbolindex)))
        return pn)

insertVersionNode :: (Monad m) => Node -> VersionNumber -> PG (StateT Indices m) Node
insertVersionNode packagenode versionnumber = do
    let versionproperties = Map.fromList [("versionnumber",pack (display versionnumber))]
    merge (unique (return packagenode >>= suc "VERSION" >>= has (properties >=> strain (==versionproperties)))) (do
        versionnode <- newNode versionproperties
        newEdge "VERSION" packagenode versionnode
        return versionnode)

insertPackageError :: (Monad m) => PackageName -> VersionNumber -> PackageError -> PG (StateT Indices m) ()
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