module MasterPipe where

import Control.Proxy
import Control.Proxy.Safe
import Control.Proxy.Safe.Prelude

import Control.Monad (filterM,when)
import System.Directory (doesFileExist,createDirectoryIfMissing)
import System.FilePath

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


masterpipe :: IO ()
masterpipe = runSafeIO $ runProxy $ runEitherK $
    packages >->
    tryK (loadConfigurations >-> leftD configurations >-> mapD (either id id) >-> saveConfigurations)

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
modules = undefined

data AST = AST

asts :: (Proxy p,CheckP p) => () -> Pipe p (Package,Module,CPPOptions) (Package,Module,AST) IO ()
asts = undefined


