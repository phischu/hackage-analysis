{-# LANGUAGE TypeFamilies #-}
module NameResolution where

import Common (
    Repository,ParsedRepository,traverseRepository,
    PackageInformation(..),
    ModuleInformation(..),ModuleAST)

import Language.Haskell.Names (computeInterfaces,Symbols,Error)
import Distribution.HaskellSuite.Modules (MonadModule(..))
import Language.Haskell.Exts.Extension (Language(Haskell2010))
import Language.Haskell.Exts.Annotated (SrcSpanInfo)

import Distribution.Package (Dependency)
import Distribution.ModuleName (ModuleName)

import System.Directory (doesFileExist)

import Data.Aeson (decode)

import qualified Data.ByteString.Lazy as ByteString (readFile)

import Control.Monad (when,forM)

import Data.Set (Set,empty)

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
            nameerrors <- runNameResolution (computeInterfaces Haskell2010 [] modules) (parsedrepository,dependencies)
            return (NameErrors nameerrors)

recoverModules :: FilePath -> [ModuleName] -> IO [ModuleAST]
recoverModules packagepath modulenames = do
    return undefined

saveNameErrors :: FilePath -> NameErrors -> IO ()
saveNameErrors = undefined

resolveDependencies :: ParsedRepository -> [Dependency] -> IO ()
resolveDependencies parsedrepository dependencies = undefined

data NameResolutionMonad a = NameResolutionMonad

instance Monad NameResolutionMonad where

instance MonadModule NameResolutionMonad where
    type ModuleInfo NameResolutionMonad = Symbols

runNameResolution :: NameResolutionMonad a -> (ParsedRepository,[Dependency]) -> IO a
runNameResolution = undefined
