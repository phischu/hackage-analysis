{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}
module MasterPipe where

import Control.Proxy
import Control.Proxy.Safe
import Control.Proxy.Safe.Prelude

import Control.Monad (filterM,when,forM_,void)
import System.Directory (doesFileExist,createDirectoryIfMissing)
import System.FilePath
import Control.Exception (ErrorCall(ErrorCall))

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

import Language.Haskell.Exts (parseFileWithMode)
import Language.Haskell.Exts.Fixity (baseFixities)
import Language.Haskell.Exts.Parser (ParseMode(..),defaultParseMode,ParseResult(ParseOk,ParseFailed))
import qualified Language.Haskell.Exts.Syntax as AST

import GHC.Generics
import qualified Data.ByteString.Lazy as BS
import Data.Aeson.Generic


masterpipe :: IO ()
masterpipe = runSafeIO $ runProxy $ runEitherK $
    packages >->
    tryK (memoPipe loadConfigurations configurations saveConfigurations) >->
    tryK modules >->
    mapP (memoPipe loadASTs asts saveASTs)

memoPipe :: (Proxy p,ListT p,Monad m) =>
            (() -> Pipe p a (Either a b) m ()) ->
            (() -> Pipe p a b m ()) ->
            (() -> Pipe p b b m ()) ->
            (() -> Pipe p a b m ())
memoPipe load work save = load >-> leftD work >-> mapD (either id id) >-> save

data Package = Package Name Version FilePath deriving (Show,Read,Eq)
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
{-
deriving instance Generic AST.Module
deriving instance Generic AST.Decl
deriving instance Generic AST.ImportDecl
deriving instance Generic AST.Annotation
deriving instance Generic AST.ExportSpec
deriving instance Generic AST.ImportSpec
deriving instance Generic AST.Activation
deriving instance Generic AST.Exp
deriving instance Generic AST.WarningText
deriving instance Generic AST.Rule
deriving instance Generic AST.ModuleName
deriving instance Generic AST.CName
deriving instance Generic AST.XAttr
instance FromJSON AST.Module
instance FromJSON AST.Decl
instance FromJSON AST.ImportDecl
instance FromJSON AST.Annotation
instance FromJSON AST.ExportSpec
instance FromJSON AST.ImportSpec
instance FromJSON AST.Activation
instance FromJSON AST.Exp
instance FromJSON AST.WarningText
instance FromJSON AST.Rule
instance FromJSON AST.ModuleName
instance FromJSON AST.CName
instance FromJSON AST.XAttr
-}
loadASTs :: (Proxy p,CheckP p) => () -> Pipe p (Package,Module,CPPOptions) (Either (Package,Module,CPPOptions) (Package,Module,AST.Module)) SafeIO ()
loadASTs () = runIdentityP $ forever $ void $ runEitherP $  do
    (package,modul,cppoptions) <- request ()
    let path = astpath package modul
    exists <- tryIO (doesFileExist path)
    if exists
        then do
                maybeast <- fmap decode (tryIO (BS.readFile path))
                case maybeast of
                    Nothing -> respond (Left (package,modul,cppoptions))
                    Just ast -> respond (Right (package,modul,ast))
        else respond (Left (package,modul,cppoptions))

asts :: (Proxy p,CheckP p) => () -> Pipe p (Package,Module,CPPOptions) (Package,Module,AST.Module) SafeIO ()
asts () = runIdentityP $ forever $ void $ runEitherP $ do
    (package,modul,cppoptions) <- request ()
    let Package packagename packageversion packagepath = package
        Module modulename modulepath = modul
        mode = defaultParseMode {parseFilename = modulepath, fixities = Just baseFixities}
    maybeast <- tryIO (do
        parseresult <- parseFileWithMode mode modulepath
        case parseresult of
            ParseFailed _ _ -> return Nothing
            ParseOk ast -> return (Just ast)) `catch`
                (\(ErrorCall err)->do
                    --tryIO (putStrLn (packagename++"-"++modulename++": " ++err))
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



