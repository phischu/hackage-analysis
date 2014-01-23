{-# LANGUAGE OverloadedStrings #-}
module Common where

import Data.Version (Version)
import Distribution.Package (Dependency)
import Distribution.ModuleName (ModuleName)
import Distribution.Text (display)

import qualified Language.Haskell.Exts.Annotated as HSE (Module,SrcSpanInfo)
import Language.Haskell.Exts.Pretty (prettyPrint)

import Data.Aeson (
    ToJSON(toJSON),object,(.=),
    FromJSON(parseJSON))

import Data.Map (Map,traverseWithKey)

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
    parseJSON = undefined

data ModuleInformation =
    ModuleError ModuleError |
    ModuleInformation ModuleAST deriving (Eq,Show)

instance ToJSON ModuleInformation where
    toJSON (ModuleError moduleerror) = object ["moduleerror" .= show moduleerror]
    toJSON (ModuleInformation moduleast) = object ["moduleast" .= prettyPrint moduleast]

instance FromJSON ModuleInformation where
    parseJSON = undefined
