{-# LANGUAGE OverloadedStrings #-}
module Fragmentation where

import Common (
    ParsedRepository,traverseRepository,PackagePath,
    loadPackage,PackageInformation(PackageError,PackageInformation),
    loadModuleInformation,ModuleInformation(ModuleError,ModuleInformation),
    Declaration(Declaration),declarationsFilePath,Genre(..))

import NameResolution (runNameResolution)

import Distribution.Package (Dependency)

import qualified Language.Haskell.Exts.Annotated as HSE (Module,Decl,SrcSpanInfo,ModuleName)
import Language.Haskell.Exts.Annotated (Decl(..))
import Language.Haskell.Exts.Extension (Language(Haskell2010))
import Language.Haskell.Exts.Pretty (prettyPrint)

import Language.Haskell.Names (
    Symbols(Symbols),annotateModule,Scoped(Scoped),SymValueInfo,SymTypeInfo,OrigName,
    NameInfo(GlobalValue,GlobalType))
import Language.Haskell.Names.SyntaxUtils (getModuleDecls,getModuleName)
import Language.Haskell.Names.ModuleSymbols (getTopDeclSymbols)
import qualified Language.Haskell.Names.GlobalSymbolTable as GlobalTable (empty)

import Distribution.ModuleName (ModuleName)
import Distribution.Text (display)

import System.Directory (doesFileExist)

import Data.Aeson (encode,ToJSON(toJSON),object,(.=))

import qualified Data.ByteString.Lazy as ByteString (writeFile)
import Control.Monad (forM_,when)
import Data.Either (partitionEithers)
import qualified Data.Set as Set (fromList)
import Data.Foldable (foldMap)

splitAndSaveAllDeclarations :: ParsedRepository -> IO ()
splitAndSaveAllDeclarations parsedrepository = do
    flip traverseRepository parsedrepository (\_ _ packagepath -> do
        splitAndSaveDeclarations parsedrepository packagepath)
    return ()

splitAndSaveDeclarations :: ParsedRepository -> PackagePath -> IO ()
splitAndSaveDeclarations parsedrepository packagepath = do
    putStrLn ("Fragmenting: " ++ packagepath)
    maybepackageinformation <- loadPackage packagepath
    case maybepackageinformation of
        Nothing -> return ()
        Just (PackageError _) -> return ()
        Just (PackageInformation modulenames dependencies) -> do
            forM_ modulenames (\modulename -> do
                declarationsexist <- doesFileExist (declarationsFilePath packagepath modulename) 
                when (not declarationsexist) (do
                    declarations <- splitModule parsedrepository packagepath dependencies modulename
                    saveDeclarations packagepath modulename declarations))

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
            let modulenameast = getModuleName annoatedmoduleast
            return (splitAnnotatedModule modulenameast annoatedmoduleast)

splitAnnotatedModule :: HSE.ModuleName (Scoped HSE.SrcSpanInfo) -> HSE.Module (Scoped HSE.SrcSpanInfo) -> [Declaration]
splitAnnotatedModule modulenameast annotatedmoduleast = map (declToDeclaration modulenameast) (getModuleDecls annotatedmoduleast)

declToDeclaration :: HSE.ModuleName (Scoped HSE.SrcSpanInfo) -> HSE.Decl (Scoped HSE.SrcSpanInfo) -> Declaration
declToDeclaration modulenameast annotatedmoduleast = Declaration
    (declGenre annotatedmoduleast)
    (prettyPrint annotatedmoduleast)
    (declaredSymbols modulenameast annotatedmoduleast)
    (usedSymbols annotatedmoduleast)

declGenre :: HSE.Decl (Scoped HSE.SrcSpanInfo) -> Genre
declGenre (TypeDecl _ _ _) = Type
declGenre (TypeFamDecl _ _ _) = Type
declGenre (DataDecl _ _ _ _ _ _) = Type
declGenre (GDataDecl _ _ _ _ _ _ _) = Type
declGenre (DataFamDecl _ _ _ _) = Type
declGenre (TypeInsDecl _ _ _) = Type
declGenre (DataInsDecl _ _ _ _ _) = Type
declGenre (GDataInsDecl _ _ _ _ _ _) = Type
declGenre (ClassDecl _ _ _ _ _) = TypeClass
declGenre (InstDecl _ _ _ _) = ClassInstance
declGenre (DerivDecl _ _ _) = ClassInstance
declGenre (TypeSig _ _ _) = TypeSignature
declGenre (FunBind _ _) = Value
declGenre (PatBind _ _ _ _ _) = Value
declGenre (ForImp _ _ _ _ _ _) = Value
declGenre _ = Other

declaredSymbols :: HSE.ModuleName (Scoped HSE.SrcSpanInfo) -> HSE.Decl (Scoped HSE.SrcSpanInfo) -> Symbols
declaredSymbols modulenameast annotatedmoduleast = Symbols (Set.fromList valuesymbols) (Set.fromList typesymbols) where
    (valuesymbols,typesymbols) = partitionEithers (getTopDeclSymbols GlobalTable.empty modulenameast annotatedmoduleast)

usedSymbols :: HSE.Decl (Scoped HSE.SrcSpanInfo) -> Symbols
usedSymbols annotatedmoduleast = Symbols (Set.fromList valuesymbols) (Set.fromList typesymbols) where
    (valuesymbols,typesymbols) = partitionEithers (foldMap externalSymbol annotatedmoduleast)

externalSymbol :: Scoped HSE.SrcSpanInfo -> [Either (SymValueInfo OrigName) (SymTypeInfo OrigName)]
externalSymbol (Scoped (GlobalValue symvalueinfo) _) = [Left symvalueinfo]
externalSymbol (Scoped (GlobalType symtypeinfo) _) = [Right symtypeinfo]
externalSymbol _ = []

saveDeclarations :: PackagePath -> ModuleName -> [Declaration] -> IO ()
saveDeclarations packagepath modulename declarations =
    ByteString.writeFile (declarationsFilePath packagepath modulename) (encode declarations)

