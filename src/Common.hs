{-# LANGUAGE OverloadedStrings #-}
module Common where

import Data.Version (Version)
import Distribution.Package (Dependency)
import Distribution.ModuleName (ModuleName)
import Distribution.Text (display,simpleParse)

import qualified Language.Haskell.Exts.Annotated as HSE (Module,SrcSpanInfo)
import Language.Haskell.Exts.Annotated (parseModule,ParseResult(ParseOk,ParseFailed))
import Language.Haskell.Exts.Pretty (prettyPrint)

import Data.Aeson (
    ToJSON(toJSON),object,(.=),
    FromJSON(parseJSON),Value(Object),(.:))
import Data.Aeson.Types (Parser)

import Data.Map (Map,traverseWithKey)

import Control.Monad (mzero,mplus)

type Repository a = Map PackageName (Map VersionNumber a)
type SourceRepository = Repository FilePath
type ParsedRepository = Repository FilePath
type PackageName   = String
type VersionNumber = Version

type ModuleAST = HSE.Module HSE.SrcSpanInfo

traverseRepository :: (PackageName -> VersionNumber -> a -> IO b) -> Repository a -> IO (Repository b)
traverseRepository f =
    traverseWithKey (\packagename ->
        traverseWithKey (\versionnumber a ->
            f packagename versionnumber a))

data PackageError =
    PackageReadingError String |
    PackageFinalizationError [Dependency] |
    PackageNoLibrary deriving (Eq,Show,Read)

data ModuleError =
    ModuleFilteringError String |
    ModuleFileNotFound |
    MultipleModuleFilesFound |
    PreprocessorError String |
    ParserError String deriving (Eq,Show,Read)

data PackageInformation =
    PackageError PackageError |
    PackageInformation [ModuleName] [Dependency] deriving (Eq,Show)

instance ToJSON PackageInformation where
    toJSON (PackageError packageerror) = object ["packageerror" .= show packageerror]
    toJSON (PackageInformation modulenames dependencies) = object [
        "modulenames" .= map display modulenames,
        "dependencies" .= map display dependencies]

instance FromJSON PackageInformation where
    parseJSON value = parsePackageError value `mplus` parsePackageInformation value

parsePackageError :: Value -> Parser PackageInformation
parsePackageError (Object o) = do
    packageerrorvalue <- o .: "packageerror"
    return (PackageError (read packageerrorvalue))
parsePackageError _ = mzero

parsePackageInformation :: Value -> Parser PackageInformation
parsePackageInformation (Object o) = do
    modulenamevalues <- o .: "modulenames"
    dependencyvalues <- o .: "dependencies"
    modulenames <- maybe mzero return (mapM simpleParse modulenamevalues)
    dependencies <- maybe mzero return (mapM simpleParse dependencyvalues)
    return (PackageInformation modulenames dependencies)
parsePackageInformation _ = mzero

data ModuleInformation =
    ModuleError ModuleError |
    ModuleInformation ModuleAST deriving (Eq,Show)

instance ToJSON ModuleInformation where
    toJSON (ModuleError moduleerror) = object ["moduleerror" .= show moduleerror]
    toJSON (ModuleInformation moduleast) = object ["moduleast" .= prettyPrint moduleast]

instance FromJSON ModuleInformation where
    parseJSON value = parseModuleError value `mplus` parseModuleInformation value

parseModuleError :: Value -> Parser ModuleInformation
parseModuleError (Object o) = do
    moduleerrorvalue <- o .: "moduleerror"
    return (ModuleError (read moduleerrorvalue))
parseModuleError _ = mzero

parseModuleInformation :: Value -> Parser ModuleInformation
parseModuleInformation (Object o) = do
    moduleastvalue <- o .: "moduleast"
    case parseModule moduleastvalue of
        ParseOk moduleast -> return (ModuleInformation moduleast)
        ParseFailed _ _ -> mzero
parseModuleInformation _ = mzero
