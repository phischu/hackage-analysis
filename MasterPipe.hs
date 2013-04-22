{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}
module MasterPipe where

import Control.Proxy
import Control.Proxy.Safe
import Control.Proxy.Safe.Prelude

import Control.Monad (filterM,when,forM_,void)
import System.Directory (doesFileExist,createDirectoryIfMissing)
import System.FilePath
import Control.Exception (ErrorCall(ErrorCall),SomeException(SomeException),IOException,evaluate)
import Control.DeepSeq (force)

import Distribution.PackageDescription
    (PackageDescription(..),Library(..),libModules,BuildInfo(..),
     FlagAssignment,GenericPackageDescription)
import Distribution.PackageDescription.Configuration (finalizePackageDescription)
import Distribution.Package (Dependency)
import Distribution.Verbosity (silent)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.System (Platform(Platform),Arch(I386),OS(Linux))
import Distribution.Compiler (CompilerId(CompilerId),CompilerFlavor(GHC))
import Distribution.Text (disp)
import Distribution.ModuleName (toFilePath)
import qualified Data.Version as V (Version(Version))

import Language.Preprocessor.Cpphs (runCpphs,defaultCpphsOptions)
import Data.List (isPrefixOf)

import Language.Haskell.Exts (parseFileContentsWithMode)
import Language.Haskell.Exts.Fixity (baseFixities)
import Language.Haskell.Exts.Parser (ParseMode(..),defaultParseMode,ParseResult(ParseOk,ParseFailed))
import qualified Language.Haskell.Exts.Syntax as AST

import GHC.Generics
import qualified Data.ByteString.Lazy as BS
import Data.Aeson.Generic

import Control.Monad.State.Strict (execStateT,modify,get)

import Data.Map.Strict (Map,empty,insertWith,toList,delete)
import qualified Data.Map.Strict as M (map)
import Data.Set (Set,union,singleton,size)
import Data.List (foldl')
import Visualization (cppflags)

data Stats = Stats {
    numberOfPackagesFound :: !Integer,
    numberOfPackagesConsidered :: !Integer,
    numberOfModulesFound :: !Integer,
    numberOfModulesParsed :: !Integer,
    cppOptionsUsed :: ![(Package,Module,String)]
    } deriving (Read,Show)

emptyStats = Stats 0 0 0 0 []

masterpipe :: IO ()
masterpipe = do
    result <- trySafeIO $ flip execStateT emptyStats $ runProxy $ runEitherK $
        raiseK packages >-> countPackagesFound >->
        raiseK (tryK (memoPipe loadConfigurations configurations saveConfigurations)) >->
        countPackagesConsidered >->
        raiseK (tryK modules) >-> countModulesFound >->
        preprocess >->
        raiseK (mapP (memoPipe loadASTs asts saveASTs)) >-> countModulesParsed >-> printProgress
    writeFile "result.txt" (show result)
    writeOutCPPFlagData result
    cppflags

memoPipe :: (Proxy p,ListT p,Monad m) =>
            (() -> Pipe p a (Either a b) m ()) ->
            (() -> Pipe p a b m ()) ->
            (() -> Pipe p b b m ()) ->
            (() -> Pipe p a b m ())
memoPipe load work save = load >-> leftD work >-> mapD (either id id) >-> save

data Package = Package Name Version FilePath deriving (Show,Read,Eq,Ord)
type Name = String
type Version = String

packages :: (Proxy p) => () -> Producer (ExceptionP p) Package SafeIO ()
packages = readFileS "packages.list" >-> mapD read

data Configuration = Configuration (Either NoModulesReason ([Module],CPPOptions)) deriving (Show,Read)
data NoModulesReason = ConfigureFailure | NoLibrary deriving (Show,Read)

loadConfigurations :: (Proxy p,CheckP p) => () -> Pipe p Package (Either Package (Package,Configuration)) IO ()
loadConfigurations () = runIdentityP $ forever $ do
    package <- request ()
    let path = configurationpath package
    exists <- lift (doesFileExist path)
    if exists
        then do
                configuration <- fmap read (lift (readFile path))
                respond (Right (package,configuration))
        else respond (Left package)

configurationpath :: Package -> FilePath
configurationpath (Package name version _) = "Configurations/" ++ name ++ "-" ++ version ++ ".configuration"

saveConfigurations :: (Proxy p,CheckP p) => () -> Pipe p (Package,Configuration) (Package,Configuration) IO ()
saveConfigurations () = runIdentityP $ forever $ do
    (package,configuration) <- request ()
    let path = configurationpath package
    exists <- lift (doesFileExist path)
    when (not exists) (do
        lift (createDirectoryIfMissing True (dropFileName path))
        lift (writeFile path (show configuration)))
    respond (package,configuration)

configurations :: (Proxy p,CheckP p) => () -> Pipe p Package (Package,Configuration) IO ()
configurations () = runIdentityP $ forever $ do
    package <- request ()
    let Package packagename version packagepath = package
    let cabalfile = packagepath ++ packagename ++ ".cabal"
    genericpackagedescription <- lift (readPackageDescription silent cabalfile)
    case configure genericpackagedescription of
        Left _ -> respond (package,Configuration (Left ConfigureFailure))
        Right (packagedescription,_) -> do
            case library packagedescription of
                Nothing -> respond (package,Configuration (Left NoLibrary))
                Just librarysection -> do
                    let modulenames = libModules librarysection
                        sourcedirs = hsSourceDirs (libBuildInfo librarysection)
                        potentialModules = do
                            modulename <- modulenames
                            directory <- sourcedirs
                            extension <- [".hs",".lhs"]
                            return (Module (show (disp modulename)) (packagepath ++ directory ++ "/" ++ toFilePath modulename ++ extension))
                        valid (Module _ path) = doesFileExist path
                        cppoptions = CPPOptions (cppOptions (libBuildInfo librarysection))
                    modules <- lift (filterM valid potentialModules)
                    respond (package,Configuration (Right (modules,cppoptions)))

configure :: GenericPackageDescription -> Either [Dependency] (PackageDescription,FlagAssignment)
configure = finalizePackageDescription [] (const True) (Platform I386 Linux) (CompilerId GHC (V.Version [7,6,2] [])) []

data Module = Module Name FilePath deriving (Show,Read)
data CPPOptions = CPPOptions [String] deriving (Show,Read)

modules :: (Proxy p,CheckP p,Monad m) => () -> Pipe p (Package,Configuration) (Package,Module,CPPOptions) m ()
modules () = runIdentityP $ forever $ do
    (package,Configuration configuration) <- request ()
    case configuration of
        Left _ -> return ()
        Right (modules,cppoptions) -> forM_ modules (\modul->respond (package,modul,cppoptions))

preprocess :: (Proxy p) => () -> Pipe (ExceptionP p) (Package,Module,CPPOptions) (Package,Module,String) (StateT Stats SafeIO) r
preprocess () = forever $ (do
    (package,modul,_) <- request ()
    let Module modulename modulepath = modul
    rawsource <- hoist lift (tryIO (readFile modulepath >>= evaluate . force))
    let optionsFound = filter isPreprocessorLine (lines rawsource)
        isPreprocessorLine x = "#ifdef" `isPrefixOf` x ||
                               "#ifndef" `isPrefixOf` x ||
                               "#if" `isPrefixOf` x ||
                               "#elif" `isPrefixOf` x
        addCppOption x stats = stats {cppOptionsUsed = x:cppOptionsUsed stats}
    forM_ optionsFound (\option -> lift (modify (addCppOption (package,modul,option))))
    sourcecode <- hoist lift (tryIO (runCpphs defaultCpphsOptions modulepath rawsource))
    respond (package,modul,rawsource)) `catch`
        (\e -> hoist lift (tryIO (print (e :: IOException)))) `catch`
        (\e -> hoist lift (tryIO (print (e :: ErrorCall))))

loadASTs :: (Proxy p,CheckP p) => () -> Pipe p (Package,Module,String) (Either (Package,Module,String) (Package,Module,AST.Module)) SafeIO ()
loadASTs () = runIdentityP $ forever $ void $ runEitherP $  do
    (package,modul,sourcecode) <- request ()
    let path = astpath package modul
    exists <- tryIO (doesFileExist path)
    if exists
        then do
                maybeast <- fmap decode (tryIO (BS.readFile path))
                case maybeast of
                    Nothing -> respond (Left (package,modul,sourcecode))
                    Just ast -> respond (Right (package,modul,ast))
        else respond (Left (package,modul,sourcecode))

asts :: (Proxy p,CheckP p) => () -> Pipe p (Package,Module,String) (Package,Module,AST.Module) SafeIO ()
asts () = runIdentityP $ forever $ void $ runEitherP $ do
    (package,modul,sourcecode) <- request ()
    let Module modulename modulepath = modul
        mode = defaultParseMode {parseFilename = modulepath, fixities = Just baseFixities}
    maybeast <- tryIO (do
        parseresult <- return (parseFileContentsWithMode mode sourcecode)
        case parseresult of
            ParseFailed _ _ -> return Nothing
            ParseOk ast -> return (Just ast)) `catch`
                (\(ErrorCall err)->do
                    return Nothing)
    case maybeast of
        Nothing -> return ()
        Just ast -> respond (package,modul,ast)

astpath :: Package -> Module -> FilePath
astpath (Package packagename packageversion packagepath) (Module modulename modulepath) = concat
    ["ASTs/",packagename,"/",packageversion,"/",modulename,"/",packagename,"-",packageversion,"_",modulename,".ast.json"]

saveASTs :: (Proxy p,CheckP p) => () -> Pipe p (Package,Module,AST.Module) (Package,Module,AST.Module) SafeIO ()
saveASTs () = runIdentityP $ forever $ void $ runEitherP $ do
    (package,modul,ast) <- request ()
    let path = astpath package modul
    exists <- tryIO (doesFileExist path)
    when (not exists) (do
        tryIO (createDirectoryIfMissing True (dropFileName path))
        tryIO (BS.writeFile path (encode ast)))
    respond (package,modul,ast)

countPackagesFound :: (Proxy p) => () -> Pipe (ExceptionP p) Package Package (StateT Stats SafeIO) r
countPackagesFound () = forever $ do
    x <- request ()
    lift (modify (\stats->stats {numberOfPackagesFound = numberOfPackagesFound stats + 1}))
    respond x

countPackagesConsidered :: (Proxy p) => () -> Pipe (ExceptionP p) (Package,Configuration) (Package,Configuration) (StateT Stats SafeIO) r
countPackagesConsidered () = forever $ do
    x <- request ()
    case x of
        (_,Configuration (Left _)) -> return ()
        (_,Configuration (Right _)) -> lift (modify (\stats->stats {numberOfPackagesConsidered = numberOfPackagesConsidered stats + 1}))
    respond x

countModulesFound :: (Proxy p) => () -> Pipe (ExceptionP p) (Package,Module,CPPOptions) (Package,Module,CPPOptions) (StateT Stats SafeIO) r
countModulesFound () = forever $ do
    x <- request ()
    lift (modify (\stats->stats {numberOfModulesFound = numberOfModulesFound stats + 1}))
    respond x

countModulesParsed :: (Proxy p) => () -> Pipe (ExceptionP p) (Package,Module,AST.Module) (Package,Module,AST.Module) (StateT Stats SafeIO) r
countModulesParsed () = forever $ do
    x <- request ()
    lift (modify (\stats->stats {numberOfModulesParsed = numberOfModulesParsed stats + 1}))
    respond x

printProgress :: (Proxy p) => () -> Pipe (ExceptionP p) x x (StateT Stats SafeIO) r
printProgress () = forever $ do
    x <- request ()
    stats <- lift get
    if (numberOfModulesParsed stats `mod` 1000 == 0)
        then raise (tryIO (print (stats {cppOptionsUsed = []})))
        else return ()
    respond x

writeOutCPPFlagData :: Stats -> IO ()
writeOutCPPFlagData stats = writeFile "CPPFlags.data" (flagmapToCsv flagmap) where
    flagmap = foldl' insertFlag empty (cppOptionsUsed stats)
    insertFlag :: Map String (Set Package) -> (Package,Module,String) -> Map String (Set Package)
    insertFlag accu (package,_,flag) = insertWith union (stripDefined (stripExclamationMark (extractFlag flag))) (singleton package) accu
    extractFlag flag = if length (words flag) > 1 then words flag !! 1 else "error finding flag"
    stripExclamationMark ('!':flag) = flag
    stripExclamationMark flag = flag
    stripDefined flag = if "defined(" `isPrefixOf` flag then takeWhile (/= ')') (drop 8 flag) else flag
    flagmapToCsv = unlines . map (\(k,v)->k++" "++show v) . toList . delete "" . M.map size


