{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, OverloadedStrings #-}
module NameResolution where

import Common (
    ParsedRepository,traverseRepository,
    PackageInformation(..),
    ModuleInformation(..),ModuleAST)

import Language.Haskell.Names (computeInterfaces,Symbols,Error)
import Language.Haskell.Names.Interfaces (writeInterface,readInterface)
import Distribution.HaskellSuite.Modules (
    MonadModule(..),convertModuleName,modToString)
import Language.Haskell.Exts.Extension (Language(Haskell2010))
import Language.Haskell.Exts.Annotated (SrcSpanInfo)

import Distribution.Package (Dependency(Dependency),PackageName(PackageName))
import Distribution.ModuleName (ModuleName)
import Distribution.Text (display)
import Distribution.Version (withinRange)

import System.Directory (doesFileExist)

import Data.Aeson (decode,encode,ToJSON(toJSON),object,(.=))

import qualified Data.ByteString.Lazy as ByteString (readFile,writeFile)

import Control.Monad (when,forM_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT,runReaderT,ask)

import Data.Maybe (catMaybes,listToMaybe)
import Data.Set (Set,empty)
import qualified Data.Set as Set (map)
import Data.Map (Map,traverseWithKey)
import qualified Data.Map as Map (lookup,fromList,toAscList)

resolveAndSaveAllPackageNames :: ParsedRepository -> IO ()
resolveAndSaveAllPackageNames parsedrepository = do
    putStrLn "Resolving Names ..."
    flip traverseRepository parsedrepository (\_ _ packagepath -> do
        resolveNamesAndSaveNameErrors parsedrepository packagepath)
    return ()

resolveNamesAndSaveNameErrors :: ParsedRepository ->  FilePath -> IO ()
resolveNamesAndSaveNameErrors parsedrepository packagepath = do
    nameerrorsexist <- doesFileExist (nameerrorspath packagepath) 
    when (not nameerrorsexist) (do
        nameerrors <- resolveNames parsedrepository packagepath
        saveNameErrors packagepath nameerrors)

resolveNames :: ParsedRepository -> FilePath -> IO NameErrors
resolveNames parsedrepository packagepath = do
    maybepackageinformation <- loadPackage packagepath
    case maybepackageinformation of
        Nothing -> return (NameErrors empty)
        Just (PackageError _) -> return (NameErrors empty)
        Just (PackageInformation modulenames dependencies) -> do
            resolveDependencies parsedrepository dependencies
            modules <- recoverModules packagepath modulenames
            nameerrors <- runNameResolution (computeInterfaces Haskell2010 [] modules) (packagepath,parsedrepository,dependencies)
            return (NameErrors nameerrors)

loadPackage :: FilePath -> IO (Maybe PackageInformation)
loadPackage packagepath = ByteString.readFile (packagepath ++ "info.json") >>= return . decode

recoverModules :: FilePath -> [ModuleName] -> IO [ModuleAST]
recoverModules packagepath modulenames = mapM (recoverModule packagepath) modulenames >>= return . catMaybes

recoverModule :: FilePath -> ModuleName -> IO (Maybe ModuleAST)
recoverModule packagepath modulename = do
    maybemoduleinformation <- loadModuleInformation packagepath modulename
    case maybemoduleinformation of
        Nothing -> return Nothing
        Just (ModuleError _) -> return Nothing
        Just (ModuleInformation moduleast) -> return (Just moduleast)

modulepath :: FilePath -> ModuleName -> FilePath
modulepath packagepath modulename = concat [
    packagepath,
    display modulename,
    "/",
    "ast.json"]

loadModuleInformation :: FilePath -> ModuleName -> IO (Maybe ModuleInformation)
loadModuleInformation packagepath modulename = ByteString.readFile (modulepath packagepath modulename) >>= return . decode

saveNameErrors :: FilePath -> NameErrors -> IO ()
saveNameErrors packagepath nameerrors = ByteString.writeFile (nameerrorspath packagepath) (encode nameerrors)

nameerrorspath :: FilePath -> FilePath
nameerrorspath packagepath = packagepath ++ "nameerrors.json"

resolveDependencies :: ParsedRepository -> [Dependency] -> IO ()
resolveDependencies parsedrepository dependencies =
    forM_ dependencies (\(Dependency (PackageName packagename) versionrange) -> do
        case Map.lookup packagename parsedrepository of
            Nothing -> return ()
            Just versionmap -> do
                flip traverseWithKey versionmap (\versionnumber packagepath -> do
                    when (withinRange versionnumber versionrange) (resolveNamesAndSaveNameErrors parsedrepository packagepath))
                return ())

newtype NameResolutionMonad a = NameResolutionMonad {
    unNameResolutionMonad :: ReaderT (FilePath,Map ModuleName FilePath) IO a } deriving (Functor,Monad)

instance MonadModule NameResolutionMonad where
    type ModuleInfo NameResolutionMonad = Symbols
    lookupInCache modulename = NameResolutionMonad (do
        (_,modulemap) <- ask
        case Map.lookup (convertModuleName modulename) modulemap of
            Nothing -> return Nothing
            Just modulefilepath -> lift (readInterface modulefilepath >>= return . Just))
    insertInCache modulename symbols = NameResolutionMonad (do
        (packagepath,_) <- ask
        let modulenamespath = concat [
                packagepath,
                modToString modulename,
                "/",
                "names.json"]
        lift (writeInterface modulenamespath symbols))
    getPackages   = return []
    readModuleInfo filepaths modulename = error
        ("not implemented readModuleInfo: "++show filepaths++" "++modToString modulename)

runNameResolution :: NameResolutionMonad a -> (FilePath,ParsedRepository,[Dependency]) -> IO a
runNameResolution nameresolution (packagepath,parsedrepository,dependencies) = do
    let dependencypaths = map (findDependency parsedrepository) dependencies
    modules <- mapM findModules (catMaybes dependencypaths)
    let modulemap = Map.fromList (concat modules)
    runReaderT (unNameResolutionMonad nameresolution) (packagepath,modulemap)

findDependency :: ParsedRepository -> Dependency -> Maybe FilePath
findDependency parsedrepository (Dependency (PackageName packagename) versionrange) =
    listToMaybe (map snd (filter (\(version,_) -> withinRange version versionrange) (maybe [] Map.toAscList (Map.lookup packagename parsedrepository))))

findModules :: FilePath -> IO [(ModuleName,FilePath)]
findModules packagepath = do
    maybepackageinformation <- loadPackage packagepath
    case maybepackageinformation of
        Nothing -> return []
        (Just (PackageError _)) -> return []
        (Just (PackageInformation modulenames _)) ->
            return (map (\modulename -> (modulename,modulepath packagepath modulename)) modulenames)

data NameErrors = NameErrors (Set (Error SrcSpanInfo))

instance ToJSON NameErrors where
    toJSON (NameErrors nameerrors) = object ["nameerrors" .= toJSON (Set.map show nameerrors)]


