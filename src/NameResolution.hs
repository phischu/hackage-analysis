{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, OverloadedStrings #-}
module NameResolution where

import Common (
    ParsedRepository,traverseRepository,
    PackageInformation(..),
    loadPackage,recoverModules,modulenamespath,
    NameErrors(ResolvingNames,NameErrors),nameerrorspath)

import Language.Haskell.Names (computeInterfaces,Symbols,Error,ppError)
import Language.Haskell.Names.Interfaces (writeInterface,readInterface)
import Distribution.HaskellSuite.Modules (
    MonadModule(..),convertModuleName,modToString)
import Language.Haskell.Exts.Extension (Language(Haskell2010))
import Language.Haskell.Exts.Annotated (SrcSpanInfo)

import Distribution.Package (Dependency(Dependency),PackageName(PackageName))
import Distribution.ModuleName (ModuleName)
import Distribution.Version (withinRange)

import qualified Data.ByteString.Lazy as ByteString (writeFile)
import System.Directory (doesFileExist)

import Data.Aeson (encode,ToJSON(toJSON),object,(.=))

import Control.Monad (when,forM_,filterM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT,runReaderT,ask)

import Data.Maybe (catMaybes,listToMaybe)
import Data.Set (Set,empty)
import qualified Data.Set as Set (map,toList)
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
        saveNameErrors packagepath ResolvingNames
        putStrLn ("Resolving: " ++ packagepath)
        nameerrors <- resolveNames parsedrepository packagepath
        saveNameErrors packagepath nameerrors)

resolveNames :: ParsedRepository -> FilePath -> IO NameErrors
resolveNames parsedrepository packagepath = do
    maybepackageinformation <- loadPackage packagepath
    case maybepackageinformation of
        Nothing -> return (NameErrors [])
        Just (PackageError _) -> return (NameErrors [])
        Just (PackageInformation modulenames dependencies) -> do
            resolveDependencies parsedrepository dependencies
            modules <- recoverModules packagepath modulenames
            nameerrors <- runNameResolution (computeInterfaces Haskell2010 [] modules) (packagepath,parsedrepository,dependencies)
            return (NameErrors (map ppError (Set.toList nameerrors)))

saveNameErrors :: FilePath -> NameErrors -> IO ()
saveNameErrors packagepath nameerrors = ByteString.writeFile (nameerrorspath packagepath) (encode nameerrors)

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
        lift (writeInterface (modulenamespath packagepath (convertModuleName modulename)) symbols))
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
        (Just (PackageInformation modulenames _)) -> do
            let modulelist = map (\modulename -> (modulename,modulenamespath packagepath modulename)) modulenames
            filterM (\(_,path) -> doesFileExist path) modulelist

