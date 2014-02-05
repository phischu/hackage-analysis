{-# LANGUAGE OverloadedStrings #-}
module Common where

import Data.Version (Version)
import Distribution.Package (Dependency)
import Distribution.ModuleName (ModuleName)
import Distribution.Text (display,simpleParse)

import qualified Language.Haskell.Exts.Annotated as HSE (Module,SrcSpanInfo)
import Language.Haskell.Exts.Annotated (parseModuleWithMode,ParseResult(ParseOk,ParseFailed))
import Language.Haskell.Exts.Annotated.Fixity (baseFixities)
import Language.Haskell.Exts.Parser (ParseMode(fixities),defaultParseMode)
import Language.Haskell.Exts.Pretty (prettyPrint)

import Language.Haskell.Names (Symbols,Error)
import Language.Haskell.Names.Interfaces ()

import Data.Aeson (
    ToJSON(toJSON),object,(.=),
    decode,FromJSON(parseJSON),Value(Object),(.:))
import Data.Aeson.Types (Parser)

import qualified Data.ByteString.Lazy as ByteString (readFile)

import Data.Maybe (catMaybes)
import Data.Map (Map,traverseWithKey)
import Data.Set (Set)
import qualified Data.Set as Set (map)

import Control.Monad (mzero,mplus,msum)

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
    ParserError String |
    ModuleInformationFileError |
    DeclarationsFileError deriving (Eq,Show,Read)

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
    let mode = defaultParseMode {fixities = Just baseFixities}
    case parseModuleWithMode mode moduleastvalue of
        ParseOk moduleast -> return (ModuleInformation moduleast)
        ParseFailed _ _ -> mzero
parseModuleInformation _ = mzero

type PackagePath = FilePath

loadPackage :: PackagePath -> IO (Maybe PackageInformation)
loadPackage packagepath = ByteString.readFile (packagepath ++ "info.json") >>= return . decode

recoverModules :: PackagePath -> [ModuleName] -> IO [ModuleAST]
recoverModules packagepath modulenames = mapM (recoverModule packagepath) modulenames >>= return . catMaybes

recoverModule :: PackagePath -> ModuleName -> IO (Maybe ModuleAST)
recoverModule packagepath modulename = do
    maybemoduleinformation <- loadModuleInformation packagepath modulename
    case maybemoduleinformation of
        Nothing -> return Nothing
        Just (ModuleError _) -> return Nothing
        Just (ModuleInformation moduleast) -> return (Just moduleast)

moduleastpath :: PackagePath -> ModuleName -> FilePath
moduleastpath packagepath modulename = concat [
    packagepath,
    display modulename,
    "/",
    "ast.json"]

modulenamespath :: PackagePath -> ModuleName -> FilePath
modulenamespath packagepath modulename = concat [
    packagepath,
    display modulename,
    "/",
    "names.json"]   

loadModuleInformation :: PackagePath -> ModuleName -> IO (Maybe ModuleInformation)
loadModuleInformation packagepath modulename = ByteString.readFile (moduleastpath packagepath modulename) >>= return . decode

data Declaration = Declaration Genre DeclarationAST DeclaredSymbols UsedSymbols deriving (Show,Eq)
data Genre = Value | TypeSignature | Type | TypeClass | ClassInstance | Other deriving (Show,Eq,Read)
type DeclarationAST = String
type DeclaredSymbols = Symbols
type UsedSymbols = Symbols

instance ToJSON Declaration where
    toJSON (Declaration genre declarationast declaredsymbols usedsymbols) = object [
        "genre" .= show genre,
        "declarationast" .= declarationast,
        "declaredsymbols" .= declaredsymbols,
        "usedsymbols" .= usedsymbols]

instance FromJSON Declaration where
    parseJSON (Object o) = do
        genre <- o .: "genre" >>= return . read
        declarationast <- o .: "declarationast"
        declaredsymbols <- o .: "declaredsymbols"
        usedsymbols <- o .: "usedsymbols"
        return (Declaration genre declarationast declaredsymbols usedsymbols)
    parseJSON _ = mzero

declarationsFilePath :: PackagePath -> ModuleName -> FilePath
declarationsFilePath packagepath modulename = concat [
    packagepath,
    display modulename,
    "/",
    "declarations.json"]

loadDeclarations :: PackagePath -> ModuleName -> IO (Maybe [Declaration])
loadDeclarations packagepath modulename = ByteString.readFile (declarationsFilePath packagepath modulename) >>= return . decode

data NameErrors =
    ResolvingNames |
    NameErrors [String]

instance ToJSON NameErrors where
    toJSON ResolvingNames = object ["resolvingnames" .= True]
    toJSON (NameErrors nameerrors) = object ["nameerrors" .= nameerrors]

instance FromJSON NameErrors where
    parseJSON (Object o) = msum [
        o .: "resolvingnames" >>= (\True -> return ResolvingNames),
        o .: "nameerrors" >>= return . NameErrors]
    parseJSON _ = mzero

nameerrorspath :: FilePath -> FilePath
nameerrorspath packagepath = packagepath ++ "nameerrors.json"

loadNameErrors :: PackagePath -> IO (Maybe NameErrors)
loadNameErrors packagepath = ByteString.readFile (nameerrorspath packagepath) >>= return . decode
