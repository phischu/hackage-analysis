module Fragmentation where

import Common (
    ParsedRepository,traverseRepository,PackagePath,
    loadPackage,PackageInformation(PackageError,PackageInformation))

import qualified Language.Haskell.Exts.Annotated as HSE (Decl,SrcSpanInfo)

import Language.Haskell.Names (Symbols)

import Distribution.ModuleName (ModuleName)

import Control.Monad (forM_)

splitAndSaveAllDeclarations :: ParsedRepository -> IO ()
splitAndSaveAllDeclarations parsedrepository = do
    flip traverseRepository parsedrepository (\_ _ packagepath -> do
        splitAndSaveDeclarations parsedrepository packagepath)
    return ()

splitAndSaveDeclarations :: ParsedRepository -> PackagePath -> IO ()
splitAndSaveDeclarations parsedrepository packagepath = do
    maybepackageinformation <- loadPackage packagepath
    case maybepackageinformation of
        Nothing -> return ()
        Just (PackageError _) -> return ()
        Just (PackageInformation modulenames dependencies) -> do
            forM_ modulenames (\modulename -> do
                declarations <- splitModule parsedrepository packagepath modulename
                saveDeclarations packagepath modulename declarations)

splitModule :: ParsedRepository -> PackagePath -> ModuleName -> IO [Declaration]
splitModule = undefined

saveDeclarations :: PackagePath -> ModuleName -> [Declaration] -> IO ()
saveDeclarations = undefined

data Declaration = Declaration Genre DeclarationAST Symbols Symbols
data Genre = Genre
type DeclarationAST = HSE.Decl HSE.SrcSpanInfo

