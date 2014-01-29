{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving #-}
module NameResolution where

import Common (
    Repository,ParsedRepository,traverseRepository,
    PackageInformation(..),
    ModuleInformation(..),ModuleAST)

import Language.Haskell.Names (computeInterfaces,Symbols,Error)
import Language.Haskell.Names.Interfaces (writeInterface,readInterface)
import Distribution.HaskellSuite.Modules (MonadModule(..),modToString)
import Language.Haskell.Exts.Extension (Language(Haskell2010))
import Language.Haskell.Exts.Annotated (SrcSpanInfo)

import Distribution.Package (Dependency)
import Distribution.ModuleName (ModuleName)
import Distribution.Text (display)

import System.Directory (doesFileExist)

import Data.Aeson (decode)

import qualified Data.ByteString.Lazy as ByteString (readFile)

import Control.Monad (when,forM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT,runReaderT,ask)

import Data.Maybe (catMaybes)
import Data.Set (Set,empty)
import Data.Map (Map)
import qualified Data.Map as Map (lookup)

data NameErrors = NameErrors (Set (Error SrcSpanInfo))

resolveAndSaveAllPackageNames :: ParsedRepository -> IO ()
resolveAndSaveAllPackageNames parsedrepository = do
    putStrLn "Resolving Names ..."
    flip traverseRepository parsedrepository (\_ _ packagepath -> do
        resolveNamesAndSaveNameErrors parsedrepository packagepath)
    return ()

resolveNamesAndSaveNameErrors :: ParsedRepository ->  FilePath -> IO ()
resolveNamesAndSaveNameErrors parsedrepository packagepath = do
    nameerrorsexist <- doesFileExist (packagepath ++ "nameerrors.json") 
    when (not nameerrorsexist) (do
        nameerrors <- resolveNames parsedrepository packagepath
        saveNameErrors packagepath nameerrors)

resolveNames :: ParsedRepository -> FilePath -> IO NameErrors
resolveNames parsedrepository packagepath = do
    maybepackageinformation <- ByteString.readFile (packagepath ++ "info.json") >>= return . decode
    case maybepackageinformation of
        Nothing -> return (NameErrors empty)
        Just (PackageError _) -> return (NameErrors empty)
        Just (PackageInformation modulenames dependencies) -> do
            resolveDependencies parsedrepository dependencies
            modules <- recoverModules packagepath modulenames
            nameerrors <- runNameResolution (computeInterfaces Haskell2010 [] modules) (packagepath,parsedrepository,dependencies)
            return (NameErrors nameerrors)

recoverModules :: FilePath -> [ModuleName] -> IO [ModuleAST]
recoverModules packagepath modulenames = mapM (recoverModule packagepath) modulenames >>= return . catMaybes

recoverModule :: FilePath -> ModuleName -> IO (Maybe ModuleAST)
recoverModule packagepath modulename = do
    let modulepath = concat [
            packagepath,
            display modulename,
            "/",
            "ast.json"]
    maybemoduleinformation <- ByteString.readFile modulepath >>= return . decode
    case maybemoduleinformation of
        Nothing -> return Nothing
        Just (ModuleError _) -> return Nothing
        Just (ModuleInformation moduleast) -> return (Just moduleast)

saveNameErrors :: FilePath -> NameErrors -> IO ()
saveNameErrors packagepath nameerrors = return ()

resolveDependencies :: ParsedRepository -> [Dependency] -> IO ()
resolveDependencies parsedrepository dependencies = return ()

newtype NameResolutionMonad a = NameResolutionMonad {
    unNameResolutionMonad :: ReaderT (FilePath,Map String FilePath) IO a } deriving (Functor,Monad)

instance MonadModule NameResolutionMonad where
    type ModuleInfo NameResolutionMonad = Symbols
    lookupInCache modulename = NameResolutionMonad (do
        (_,modulemap) <- ask
        case Map.lookup (modToString modulename) modulemap of
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
runNameResolution = undefined
