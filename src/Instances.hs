module Instances where

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

import Language.Preprocessor.Cpphs (
    runCpphs,
    defaultCpphsOptions,CpphsOptions(boolopts),
    defaultBoolOptions,BoolOptions(warnings))

import Language.Haskell.Exts.Annotated (parseFileContentsWithMode)
import Language.Haskell.Exts.Annotated.Fixity (baseFixities)
import Language.Haskell.Exts.Parser (
    ParseMode(..),defaultParseMode,ParseResult(ParseOk,ParseFailed))

import Control.Exception (evaluate)
import Control.DeepSeq (force)

import Control.Error (
    EitherT,runEitherT,hoistEither,fmapLT,scriptIO,note,left)

import System.Directory (doesFileExist)

import Control.Monad (forM,filterM)

import Data.Map (Map,traverseWithKey,fromList)

data PackageError =
    PackageReadingError String |
    PackageFinalizationError [Dependency] |
    PackageNoLibrary

data ModuleError =
    ModuleFilteringError String |
    ModuleFileNotFound |
    MultipleModuleFilesFound |
    PreprocessorError String |
    ParserError String

parseAllPackages ::
    Repository ->
    IO (
    Map PackageName (
    Map VersionNumber (
        Either PackageError (
        Map ModuleName (
            Either ModuleError
            ModuleAST)))))
parseAllPackages = traverseWithKey (\packagename ->
    traverseWithKey (\_ -> parsePackage packagename))

parsePackage :: PackageName -> FilePath -> IO (Either PackageError (Map ModuleName (Either ModuleError ModuleAST)))
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
