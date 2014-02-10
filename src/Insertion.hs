{-# LANGUAGE OverloadedStrings #-}
module Insertion where

import Common (
    ParsedRepository,traverseRepository,PackagePath,
    PackageName,VersionNumber,
    PackageInformation(PackageError,PackageInformation),PackageError,loadPackage,
    ModuleInformation(ModuleError,ModuleInformation),ModuleError(..),loadModuleInformation,
    loadDeclarations,Declaration(Declaration),NameErrors(NameErrors),loadNameErrors)

import Web.Neo (NeoT,defaultRunNeoT,cypher)

import Language.Haskell.Names (
    Symbols(Symbols),SymValueInfo(..),SymTypeInfo(..),OrigName(OrigName),GName(GName))

import Distribution.ModuleName (ModuleName)
import Distribution.Package (Dependency(Dependency))
import Distribution.Text (display)

import Data.Aeson (Value,object,(.=),toJSON)
import Data.Aeson.Types (Pair)
import Data.Text (Text)

import Data.Map (Map,traverseWithKey,mapEither)
import qualified Data.Map as Map (fromList)
import qualified Data.Set as Set (toList)

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
insertPackage packagename versionnumber dependencies modulemap maybenameerrors = do
    putStrLn ("Inserting: " ++ packagename ++ " " ++ display versionnumber)
    defaultRunNeoT (do
        insertDependencies packagename versionnumber dependencies
        let (moduleerrormap,declarationsmap) = splitModuleMap modulemap
        traverseWithKey (\modulename declarations -> insertDeclarations packagename versionnumber modulename declarations) declarationsmap
        traverseWithKey (\modulename moduleerror -> insertModuleError packagename versionnumber modulename moduleerror) moduleerrormap
        insertNameErrors packagename versionnumber maybenameerrors
        return ()) >>= print

insertPackageError :: PackageName -> VersionNumber -> PackageError -> IO ()
insertPackageError packagename versionnumber packageerror = do
    defaultRunNeoT (
        cypher
            "MERGE (rootnode:ROOTNODE)\
            \CREATE UNIQUE (rootnode)-[:PACKAGE]->(package:Package {packagename : {packagename}})\
            \CREATE UNIQUE (package)-[:VERSION]->(version:Version {versionnumber : {versionnumber}})\
            \CREATE UNIQUE (version)-[:PACKAGEERROR]->(packageerror:PackageError {packageerror : {packageerrorstring}})"
            (object [
                "packagename" .= packagename,
                "versionnumber" .= display versionnumber,
                "packageerrorstring" .= show packageerror]))
    return ()

insertDependencies :: (Monad m) => PackageName -> VersionNumber -> [Dependency] -> NeoT m ()
insertDependencies packagename versionnumber dependencies = do
    cypher
        "MERGE (rootnode:ROOTNODE)\
        \CREATE UNIQUE (rootnode)-[:PACKAGE]->(package:Package {packagename : {packagename}})\
        \CREATE UNIQUE (package)-[:VERSION]->(version:Version {versionnumber : {versionnumber}})\
        \FOREACH (dependencyname IN {dependencynames} |\
        \    CREATE UNIQUE (rootnode)-[:PACKAGE]->(otherpackage:Package {packagename : dependencyname})\
        \    CREATE UNIQUE (version)-[:DEPENDENCY]->(otherpackage))"
        (object [
            "packagename" .= packagename,
            "versionnumber" .= display versionnumber,
            "dependencynames" .= map (\(Dependency dependencyname _) -> dependencyname) dependencies])
    return ()

splitModuleMap :: Map ModuleName (Either ModuleError [Declaration]) -> (Map ModuleName ModuleError,Map ModuleName [Declaration])
splitModuleMap = mapEither id

insertDeclarations :: (Monad m) => PackageName -> VersionNumber -> ModuleName -> [Declaration] -> NeoT m ()
insertDeclarations packagename versionnumber modulename declarations = do
    cypher
        "MERGE (rootnode:ROOTNODE)\
        \CREATE UNIQUE (rootnode)-[:PACKAGE]->(package:Package {packagename : {packagename}})\
        \CREATE UNIQUE (package)-[:VERSION]->(version:Version {versionnumber : {versionnumber}})\
        \CREATE UNIQUE (version)-[:MODULE]->(module:Module {modulenname : {modulename}})\
        \FOREACH (declarationdata IN {declarations} |\
        \    CREATE UNIQUE (module)-[:DECLARATION]->(declaration:Declaration {\
        \        declarationgenre : declarationdata.declarationgenre,\
        \        declarationast : declarationdata.declarationast})\
        \    FOREACH (declaredsymboldata IN declarationdata.declaredsymbols |\
        \        MERGE (declaredsymbol:Symbol {\
        \            symbolgenre : declaredsymboldata.symbolgenre,\
        \            symbolmodule : declaredsymboldata.symbolmodule,\
        \            symbolname : declaredsymboldata.symbolname})\
        \        CREATE UNIQUE (declaration)-[:DECLAREDSYMBOL]->(declaredsymbol))\
        \    FOREACH (usedsymboldata IN declarationdata.usedsymbols |\
        \        MERGE (usedsymbol:Symbol {\
        \            symbolgenre : usedsymboldata.symbolgenre,\
        \            symbolmodule : usedsymboldata.symbolmodule,\
        \            symbolname : usedsymboldata.symbolname})\
        \        CREATE UNIQUE (declaration)-[:USEDSYMBOL]->(usedsymbol)))"
        (object [
            "packagename" .= packagename,
            "versionnumber" .= display versionnumber,
            "modulename" .= display modulename,
            "declarations" .= map declarationData declarations])
    return ()

declarationData :: Declaration -> Value
declarationData (Declaration genre declarationast declaredsymbols usedsymbols) = object [
    "declarationgenre" .= show genre,
    "declarationast" .= declarationast,
    "declaredsymbols" .= symbolsData declaredsymbols,
    "usedsymbols" .= symbolsData usedsymbols]

symbolsData :: Symbols -> Value
symbolsData (Symbols symvalueinfos symtypeinfos) = toJSON (
    map symValueData (Set.toList symvalueinfos) ++
    map symTypeData (Set.toList symtypeinfos))

symValueData :: SymValueInfo OrigName -> Value
symValueData symvalueinfo = object [
    "symbolgenre" .= symValueGenre symvalueinfo,
    "symbolmodule" .= symModuleData (sv_origName symvalueinfo),
    "symbolname" .= symNameData (sv_origName symvalueinfo)]

symTypeData :: SymTypeInfo OrigName -> Value
symTypeData symtypeinfo = object [
    "symbolgenre" .= symTypeGenre symtypeinfo,
    "symbolmodule" .= symModuleData (st_origName symtypeinfo),
    "symbolname" .= symNameData (st_origName symtypeinfo)]

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

insertModuleError :: (Monad m) => PackageName -> VersionNumber -> ModuleName -> ModuleError -> NeoT m ()
insertModuleError packagename versionnumber modulename moduleerror = do
    cypher
        "MERGE (rootnode:ROOTNODE)\
        \CREATE UNIQUE (rootnode)-[:PACKAGE]->(package:Package {packagename : {packagename}})\
        \CREATE UNIQUE (package)-[:VERSION]->(version:Version {versionnumber : {versionnumber}})\
        \CREATE UNIQUE (version)-[:MODULE]->(module:Module {modulenname : {modulename}})\
        \CREATE UNIQUE (module)-[:MODULEERROR]->(moduleerror:ModuleError {moduleerror : {moduleerrorstring}})"
        (object [
            "packagename" .= packagename,
            "versionnumber" .= display versionnumber,
            "modulename" .= display modulename,
            "moduleerrorstring" .= show moduleerror])
    return ()

insertNameErrors :: (Monad m) => PackageName -> VersionNumber -> Maybe NameErrors -> NeoT m ()
insertNameErrors packagename versionnumber (Just (NameErrors nameerrors)) = do
    cypher
        "MERGE (rootnode:ROOTNODE)\
        \CREATE UNIQUE (rootnode)-[:PACKAGE]->(package:Package {packagename : {packagename}})\
        \CREATE UNIQUE (package)-[:VERSION]->(version:Version {versionnumber : {versionnumber}})\
        \FOREACH (nameerrorstring IN {nameerrors} |\
        \    CREATE UNIQUE (version)-[:NAMEERROR]->(nameerror:NameError {nameerror : nameerrorstring}))"
        (object [
            "packagename" .= packagename,
            "versionnumber" .= display versionnumber,
            "nameerrors" .= nameerrors])
    return ()
insertNameErrors _ _ _ = return ()

