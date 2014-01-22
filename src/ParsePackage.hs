{-# LANGUAGE OverloadedStrings #-}
module ParsePackage where

import Types (
    Repository,PackageName,VersionNumber,ModuleAST)

import Distribution.PackageDescription (
    GenericPackageDescription,PackageDescription,FlagAssignment,
    library,libModules,libBuildInfo,hsSourceDirs)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.PackageDescription.Configuration (finalizePackageDescription)
import Distribution.Verbosity (silent)
import Distribution.System (Platform(Platform),Arch(I386),OS(Linux))
import Distribution.Compiler (CompilerId(CompilerId),CompilerFlavor(GHC))
import Data.Version (Version(Version))
import Distribution.Package (Dependency)
import Distribution.ModuleName (ModuleName,toFilePath)
import Distribution.Text (display)

import Language.Preprocessor.Cpphs (
    runCpphs,
    defaultCpphsOptions,CpphsOptions(boolopts),
    defaultBoolOptions,BoolOptions(warnings))

import Language.Haskell.Exts.Annotated (parseFileContentsWithMode)
import Language.Haskell.Exts.Annotated.Fixity (baseFixities)
import Language.Haskell.Exts.Parser (
    ParseMode(..),defaultParseMode,ParseResult(ParseOk,ParseFailed))
import Language.Haskell.Exts.Pretty (prettyPrint)

import Control.Exception (evaluate)
import Control.DeepSeq (force)

import Control.Error (
    EitherT,runEitherT,hoistEither,fmapLT,scriptIO,note,left)

import Data.Aeson (ToJSON(toJSON),encode,object,(.=))

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString (writeFile)

import System.Directory (doesFileExist,createDirectoryIfMissing)
import System.FilePath (dropFileName)

import Control.Monad (forM,filterM,void)

import Data.Map (Map,traverseWithKey,fromList,empty,keys)
import qualified Data.Map as Map (map)

type PackageResult = Either PackageError (Map ModuleName (Either ModuleError ModuleAST))

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

parseAllPackages :: Repository -> IO (Map PackageName (Map VersionNumber PackageResult))
parseAllPackages = traverseWithKey (\packagename ->
    traverseWithKey (\_ -> parsePackage packagename))

parseAndSaveAllPackages :: Repository -> IO ()
parseAndSaveAllPackages = void . (traverseWithKey (\packagename ->
    traverseWithKey (\versionnumber packagepath -> do
        parsePackage packagename packagepath >>= savePackage packagename versionnumber)))

parsePackage :: PackageName -> FilePath -> IO PackageResult
parsePackage packagename packagepath = runEitherT (do

    let cabalfilepath = packagepath ++ packagename ++ ".cabal"

    genericpackagedescription <- scriptIO (readPackageDescription silent cabalfilepath)
        `onFailure` PackageReadingError

    (packagedescription,_) <- hoistEither (simpleConfigure genericpackagedescription)
        `onFailure` PackageFinalizationError

    librarysection <- hoistEither (note () (library packagedescription))
        `onFailure` (const PackageNoLibrary)

    modules <- forM (libModules librarysection) (\modulename -> do

        eithermoduleast <- runEitherT (do

            let sourcedirs = hsSourceDirs (libBuildInfo librarysection)
                potentialpaths = do
                    directory <- sourcedirs
                    extension <- [".hs",".lhs"]
                    return (packagepath ++ directory ++ "/" ++ toFilePath modulename ++ extension)

            existingpaths <- scriptIO (filterM doesFileExist potentialpaths)
                `onFailure` ModuleFilteringError

            modulepath <- case existingpaths of
                [] -> left ModuleFileNotFound
                [modulepath] -> return modulepath
                _ -> left MultipleModuleFilesFound

            let cpphsoptions = defaultCpphsOptions {boolopts = booloptions }
                booloptions = defaultBoolOptions {warnings = False}

            modulefile <- scriptIO (do
                rawsource <- readFile modulepath
                sourcecode <- runCpphs cpphsoptions modulepath rawsource
                evaluate (force sourcecode))
                    `onFailure` PreprocessorError

            let mode = defaultParseMode {parseFilename = modulepath, fixities = Just baseFixities}

            eitherast <- scriptIO (do
                parseresult <- return (parseFileContentsWithMode mode modulefile)
                case parseresult of
                    ParseFailed _ message -> return (Left (ParserError message))
                    ParseOk ast -> return (Right ast))
                        `onFailure` ParserError

            hoistEither eitherast)

        return (modulename,eithermoduleast))
    return (fromList modules))

defaultPlatform :: Platform
defaultPlatform = Platform I386 Linux

defaultCompiler :: CompilerId
defaultCompiler = CompilerId GHC (Version [7,6,3] [])

simpleConfigure :: GenericPackageDescription -> Either [Dependency] (PackageDescription,FlagAssignment)
simpleConfigure = finalizePackageDescription [] (const True) defaultPlatform defaultCompiler []

onFailure :: Monad m => EitherT a m r -> (a -> b) -> EitherT b m r
onFailure = flip fmapLT

savePackage :: PackageName -> VersionNumber -> PackageResult -> IO ()
savePackage packagename versionnumber packageresult = do

    let (packageinformation,modulemap) = interpretResult packageresult

    createDirectoryWriteFile (instanceFilePath packagename versionnumber) (encode packageinformation)

    traverseWithKey (\modulename moduleinformation -> do
        createDirectoryWriteFile (moduleFilePath packagename versionnumber modulename) (encode moduleinformation)) modulemap
    return ()

createDirectoryWriteFile :: FilePath -> ByteString -> IO ()
createDirectoryWriteFile filepath content = do
    createDirectoryIfMissing True (dropFileName filepath)
    ByteString.writeFile filepath content

interpretResult :: PackageResult -> (PackageInformation,Map ModuleName ModuleInformation)
interpretResult (Left packageerror) = (PackageError packageerror,empty)
interpretResult (Right modulemap) = (PackageInformation (keys modulemap),Map.map interpretModuleResult modulemap)

interpretModuleResult :: Either ModuleError ModuleAST -> ModuleInformation
interpretModuleResult (Left moduleerror) = ModuleError moduleerror
interpretModuleResult (Right moduleast) = ModuleInformation moduleast

instanceFilePath :: PackageName -> VersionNumber -> FilePath
instanceFilePath packagename versionnumber = concat [
    "data/instances/",
    packagePath packagename versionnumber,
    "instance.json"]

moduleFilePath :: PackageName -> VersionNumber -> ModuleName -> FilePath
moduleFilePath packagename versionnumber modulename = concat [
    "data/instances/",
    packagePath packagename versionnumber,
    display modulename,
    "/",
    "ast.json"]

packagePath :: PackageName -> VersionNumber -> FilePath
packagePath packagename versionnumber = concat [
    packagename,
    "/",
    display versionnumber,
    "/"]

data PackageInformation =
    PackageError PackageError |
    PackageInformation [ModuleName] deriving (Eq,Show)

instance ToJSON PackageInformation where
    toJSON (PackageError packageerror) = object ["packageerror" .= show packageerror]
    toJSON (PackageInformation modulenames) = object ["modulenames" .= map display modulenames]

data ModuleInformation =
    ModuleError ModuleError |
    ModuleInformation ModuleAST deriving (Eq,Show)

instance ToJSON ModuleInformation where
    toJSON (ModuleError moduleerror) = object ["moduleerror" .= show moduleerror]
    toJSON (ModuleInformation moduleast) = object ["moduleast" .= prettyPrint moduleast]
