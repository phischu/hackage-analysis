module Fragmentation where

import Common (
    ParsedRepository,traverseRepository,PackagePath,
    loadPackage,PackageInformation(PackageError,PackageInformation),
    loadModuleInformation,ModuleInformation(ModuleError,ModuleInformation))

import NameResolution (runNameResolution)

import Distribution.Package (Dependency)

import qualified Language.Haskell.Exts.Annotated as HSE (Module,Decl,SrcSpanInfo)
import Language.Haskell.Exts.Extension (Language(Haskell2010))
import Language.Haskell.Exts.Pretty (prettyPrint)
import Language.Haskell.Names (Symbols(Symbols),annotateModule,Scoped)
import Language.Haskell.Names.SyntaxUtils (getModuleDecls)
import Language.Haskell.Names.ModuleSymbols (getTopDeclSymbols)

import Distribution.ModuleName (ModuleName)

import Control.Monad (forM_)
import Data.Either (partitionEithers)
import qualified Data.Set as Set (fromList)

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
                declarations <- splitModule parsedrepository packagepath dependencies modulename
                saveDeclarations packagepath modulename declarations)

splitModule :: ParsedRepository -> PackagePath -> [Dependency] -> ModuleName -> IO [Declaration]
splitModule parsedrepository packagepath dependencies modulename = do
    maybemoduleinformation <- loadModuleInformation packagepath modulename
    case maybemoduleinformation of
        Nothing -> return []
        Just (ModuleError _) -> return []
        Just (ModuleInformation moduleast) -> do
            annoatedmoduleast <- runNameResolution
                (annotateModule Haskell2010 [] moduleast)
                (packagepath,parsedrepository,dependencies)
            return (splitAnnotatedModule annoatedmoduleast)

splitAnnotatedModule :: HSE.Module (Scoped HSE.SrcSpanInfo) -> [Declaration]
splitAnnotatedModule annotatedmoduleast = map declToDeclaration (getModuleDecls annotatedmoduleast)

declToDeclaration :: HSE.Decl (Scoped (HSE.SrcSpanInfo)) -> Declaration
declToDeclaration annotatedmoduleast = Declaration
    Genre
    (prettyPrint annotatedmoduleast)
    (declaredSymbols annotatedmoduleast)
    undefined

declaredSymbols :: HSE.Decl (Scoped (HSE.SrcSpanInfo)) -> Symbols
declaredSymbols annotatedmoduleast = Symbols (Set.fromList valuesymbols) (Set.fromList typesymbols) where
    (valuesymbols,typesymbols) = partitionEithers (getTopDeclSymbols undefined undefined annotatedmoduleast)

saveDeclarations :: PackagePath -> ModuleName -> [Declaration] -> IO ()
saveDeclarations = undefined

data Declaration = Declaration Genre DeclarationAST Symbols Symbols deriving (Show,Eq)
data Genre = Genre deriving (Show,Eq,Read)
type DeclarationAST = String

