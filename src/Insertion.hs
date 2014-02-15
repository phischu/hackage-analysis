{-# LANGUAGE OverloadedStrings #-}
module Insertion where

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

import Network.HTTP.Client (
    withManager,ManagerSettings(..),defaultManagerSettings,withResponse,
    parseUrl,Request(..),RequestBody(RequestBodyLBS),
    responseStatus,responseBody,brConsume)
import Network.HTTP.Types (statusIsSuccessful)

import Data.Aeson (Value,object,(.=),toJSON,encode)
import Data.Text (Text)
import qualified  Data.ByteString as ByteString (concat)

import Data.Map (Map,mapWithKey,mapEither)
import qualified Data.Map as Map (fromList,toList,lookup,keys)
import qualified Data.Set as Set (toList)

import Data.Foldable (fold)
import Control.Monad (void,forM,forM_,when)

insertAllPackages :: ParsedRepository -> IO ()
insertAllPackages parsedrepository = do
    createIndex
    flip traverseRepository parsedrepository (\packagename versionnumber packagepath -> do
        maybepackageinformation <- loadPackage packagepath
        case maybepackageinformation of
            Nothing -> return ()
            Just (PackageError packageerror) -> insertPackageError packagename versionnumber packageerror
            Just (PackageInformation modulenames dependencies) -> do
                modulemap <- loadModuleDeclarations packagepath modulenames >>= return . Map.fromList
                maybenameerrors <- loadNameErrors packagepath
                let actualdependencies = concatMap (lookupActualDependencies parsedrepository) dependencies
                insertPackage packagename versionnumber actualdependencies modulemap maybenameerrors)
    return ()

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

createIndex :: IO ()
createIndex = batchInsert [
    cypher "CREATE INDEX ON :Symbol(symbolname)" (object [])]

batchInsert :: [Value] -> IO ()
batchInsert apicalls = do
    requestUrl <- parseUrl "http://localhost:7474/db/data/batch"
    let request = requestUrl {
            method = "POST",
            requestHeaders = [
                ("accept","application/json; charset=UTF-8"),
                ("content-type","application/json")],
            requestBody = RequestBodyLBS (encode apicalls),
            responseTimeout = Nothing}
    withManager defaultManagerSettings (\manager ->
        withResponse request manager (\response -> do
            print (responseStatus response)
            responseBodyChunks <- brConsume (responseBody response)
            when (not (statusIsSuccessful (responseStatus response))) (
                print (ByteString.concat responseBodyChunks))))

cypher :: Text -> Value -> Value
cypher querytext parameters = object [
    "to" .= ("/cypher" :: Text),
    "method" .= ("POST" :: Text),
    "body" .= object [
        "query" .= querytext,
        "params" .= parameters]]

insertPackage ::
    PackageName ->
    VersionNumber ->
    [ActualDependency] ->
    Map ModuleName (Either ModuleError [Declaration]) ->
    Maybe NameErrors ->
    IO ()
insertPackage packagename versionnumber dependencies modulemap maybenameerrors = do
    putStrLn ("Inserting: " ++ packagename ++ " " ++ display versionnumber)
    let (moduleerrormap,declarationsmap) = splitModuleMap modulemap
    batchInsert ([cypher
        "MERGE (rootnode:ROOTNODE)\
        \MERGE (rootnode)-[:PACKAGE]->(package:Package {packagename : {packagename}})\
        \MERGE (package)-[:VERSION]->(version:Version {versionnumber : {versionnumber}})\
        \ \
        \FOREACH (dependency IN {dependencies} |\
        \    MERGE (rootnode)-[:PACKAGE]->(otherpackage:Package {packagename : dependency.dependencyname})\
        \    MERGE (otherpackage)-[:VERSION]->(otherversion:Version {versionnumber : dependency.dependencyversion})\
        \    CREATE (version)-[:DEPENDENCY]->(otherversion))\
        \ \
        \FOREACH (moduleerrordata IN {moduleerrormap} |\  
        \    CREATE (version)-[:MODULE]->(module:Module {modulenname : moduleerrordata.modulename})\
        \    CREATE (module)-[:MODULEERROR]->(moduleerror:ModuleError {moduleerror : moduleerrordata.moduleerrorstring}))\
        \ \
        \FOREACH (nameerrorstring IN {nameerrors} |\
        \    CREATE (version)-[:NAMEERROR]->(nameerror:NameError {nameerror : nameerrorstring}))\
        \ \
        \FOREACH (moduledata IN {modulemap} |\
        \    CREATE (version)-[:MODULE]->(module:Module {modulenname : moduledata.modulename})\
        \ \
        \    FOREACH (declarationdata IN moduledata.declarations |\
        \        CREATE (module)-[:DECLARATION]->(declaration:Declaration {\
        \            declarationgenre : declarationdata.declarationgenre,\
        \            declarationast : declarationdata.declarationast})\
        \        FOREACH (declaredsymboldata IN declarationdata.declaredsymbols |\
        \            MERGE (declaredsymbol:Symbol {\
        \                symbolgenre : declaredsymboldata.symbolgenre,\
        \                symbolmodule : declaredsymboldata.symbolmodule,\
        \                symbolname : declaredsymboldata.symbolname})\
        \            CREATE (declaration)-[:DECLAREDSYMBOL]->(declaredsymbol))\
        \        FOREACH (usedsymboldata IN declarationdata.usedsymbols |\
        \            MERGE (usedsymbol:Symbol {\
        \                symbolgenre : usedsymboldata.symbolgenre,\
        \                symbolmodule : usedsymboldata.symbolmodule,\
        \                symbolname : usedsymboldata.symbolname})\
        \            CREATE (declaration)-[:USEDSYMBOL]->(usedsymbol))))"
        (object [
            "packagename" .= packagename,
            "versionnumber" .= display versionnumber,
            "dependencies" .= [object [
                "dependencyname" .= dependencyname,
                "dependencyversion" .= display dependencyversion] | (dependencyname,dependencyversion) <- dependencies],
            "moduleerrormap" .= [object [
                "modulename" .= display modulename,
                "moduleerrorstring" .= show moduleerrorstring] | (modulename,moduleerrorstring) <- Map.toList moduleerrormap],
            "nameerrors" .= case maybenameerrors of
                Just (NameErrors nameerrors) -> nameerrors
                Nothing -> [],
            "modulemap" .= [object [
                "modulename" .= display modulename,
                "declarations" .= map declarationData declarations] | (modulename,declarations) <- Map.toList declarationsmap]])])



insertPackageError :: PackageName -> VersionNumber -> PackageError -> IO ()
insertPackageError packagename versionnumber packageerror = batchInsert ([
        cypher
            "MERGE (rootnode:ROOTNODE)\
            \CREATE UNIQUE (rootnode)-[:PACKAGE]->\
            \              (package:Package {packagename : {packagename}})-[:VERSION]->\
            \              (version:Version {versionnumber : {versionnumber}})-[:PACKAGEERROR]->\
            \              (packageerror:PackageError {packageerror : {packageerrorstring}})"
            (object [
                "packagename" .= packagename,
                "versionnumber" .= display versionnumber,
                "packageerrorstring" .= show packageerror])])

splitModuleMap :: Map ModuleName (Either ModuleError [Declaration]) -> (Map ModuleName ModuleError,Map ModuleName [Declaration])
splitModuleMap = mapEither id

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

type ActualDependency = (PackageName,VersionNumber)

lookupActualDependencies :: ParsedRepository -> Dependency -> [ActualDependency]
lookupActualDependencies parsedrepository (Dependency (Cabal.PackageName packagename) versionrange) =
    case Map.lookup packagename parsedrepository of
        Nothing -> []
        Just versionmap -> [(packagename,versionnumber) |
            versionnumber <- Map.keys versionmap,withinRange versionnumber versionrange]
