module ParsePackage where

import Common (
    Repository,SourceRepository,ParsedRepository,PackageName,VersionNumber,ModuleAST,
    traverseRepository,
    PackageInformation(..),ModuleInformation(..),PackageError(..),ModuleError(..))

import Distribution.PackageDescription (
    GenericPackageDescription,PackageDescription,FlagAssignment,
    library,libModules,libBuildInfo,hsSourceDirs,
    targetBuildDepends,buildDepends)
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

import Control.Exception (evaluate)
import Control.DeepSeq (force)

import Control.Error (
    EitherT,runEitherT,hoistEither,fmapLT,scriptIO,note,left)

import Data.Aeson (encode)

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString (writeFile)

import System.Directory (doesFileExist,createDirectoryIfMissing)
import System.FilePath (dropFileName)

import Data.List (nub)

import Control.Monad (forM,filterM,when)

import Data.Map (Map,traverseWithKey,fromList,empty,keys)
import qualified Data.Map as Map (map)

type PackageResult = Either PackageError (Map ModuleName (Either ModuleError ModuleAST),[Dependency])

parseAllPackages :: SourceRepository -> IO (Repository PackageResult)
parseAllPackages = traverseRepository (\packagename _ packagepath -> parsePackage packagename packagepath)

parseAndSaveAllPackages :: SourceRepository -> IO ParsedRepository
parseAndSaveAllPackages repository = do
    putStrLn "Parsing Packages ..."
    flip traverseRepository repository (\packagename versionnumber packagepath -> do
        packageinformationexists <- doesFileExist (infoFilePath packagename versionnumber)
        when (not packageinformationexists) (do
            putStrLn ("Parsing: " ++ packagepath)
            packageresult <- parsePackage packagename packagepath
            savePackage packagename versionnumber packageresult)
        return (dropFileName (infoFilePath packagename versionnumber)))

parsePackage :: PackageName -> FilePath -> IO PackageResult
parsePackage packagename packagepath = runEitherT (do

    let cabalfilepath = packagepath ++ packagename ++ ".cabal"

    genericpackagedescription <- scriptIO (readPackageDescription silent cabalfilepath)
        `onFailure` PackageReadingError

    (packagedescription,_) <- hoistEither (simpleConfigure genericpackagedescription)
        `onFailure` PackageFinalizationError

    librarysection <- hoistEither (note () (library packagedescription))
        `onFailure` (const PackageNoLibrary)

    let dependencies = nub (targetBuildDepends (libBuildInfo librarysection) ++ buildDepends packagedescription)

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
    return (fromList modules,dependencies))

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

    createDirectoryWriteFile (infoFilePath packagename versionnumber) (encode packageinformation)

    flip traverseWithKey modulemap (\modulename moduleinformation -> do
        createDirectoryWriteFile (moduleFilePath packagename versionnumber modulename) (encode moduleinformation))
    return ()

createDirectoryWriteFile :: FilePath -> ByteString -> IO ()
createDirectoryWriteFile filepath content = do
    createDirectoryIfMissing True (dropFileName filepath)
    ByteString.writeFile filepath content

interpretResult :: PackageResult -> (PackageInformation,Map ModuleName ModuleInformation)
interpretResult (Left packageerror) = (PackageError packageerror,empty)
interpretResult (Right (modulemap,dependencies)) =
    (PackageInformation (keys modulemap) dependencies,Map.map interpretModuleResult modulemap)

interpretModuleResult :: Either ModuleError ModuleAST -> ModuleInformation
interpretModuleResult (Left moduleerror) = ModuleError moduleerror
interpretModuleResult (Right moduleast) = ModuleInformation moduleast

infoFilePath :: PackageName -> VersionNumber -> FilePath
infoFilePath packagename versionnumber = concat [
    "data/info/",
    packagePath packagename versionnumber,
    "info.json"]

moduleFilePath :: PackageName -> VersionNumber -> ModuleName -> FilePath
moduleFilePath packagename versionnumber modulename = concat [
    "data/info/",
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


