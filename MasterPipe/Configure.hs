{-# LANGUAGE DeriveDataTypeable #-}
module MasterPipe.Configure where

import MasterPipe.Types

import Control.Proxy (Proxy,Pipe,request,respond)
import Control.Proxy.Safe (ExceptionP,SafeIO,tryIO,left,catch)
import Control.Monad (forever)

import Distribution.PackageDescription
    (PackageDescription(..),
     FlagAssignment,GenericPackageDescription)
import Distribution.PackageDescription.Configuration (finalizePackageDescription)
import Distribution.Package (Dependency)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Verbosity (silent)
import Distribution.System (Platform(Platform),Arch(I386),OS(Linux))
import Distribution.Compiler (CompilerId(CompilerId),CompilerFlavor(GHC))
import qualified Data.Version as V (Version(Version))

import Control.Exception (Exception,SomeException,toException)
import Data.Typeable (Typeable)


configureD :: (Proxy p) => () -> Pipe (ExceptionP p) PackageVersion (PackageVersion,Configuration) SafeIO r
configureD () = forever ((do

    package <- request ()

    let PackageVersion packagename version packagepath = package
        cabalfile = packagepath ++ packagename ++ ".cabal"

    genericpackagedescription <- tryIO (readPackageDescription silent cabalfile)
    (packagedescription,flagassignment) <- either
        (left.toException.CouldNotSatisfyDependencies package)
        return
        (configure genericpackagedescription)

    respond (package,Configuration flagassignment defaultPlatform defaultCompiler packagedescription))

        `catch`

    (\e -> tryIO (print (e :: SomeException))))

defaultPlatform :: Platform
defaultPlatform = Platform I386 Linux

defaultCompiler :: CompilerId
defaultCompiler = CompilerId GHC (V.Version [7,6,2] [])

configure :: GenericPackageDescription -> Either [Dependency] (PackageDescription,FlagAssignment)
configure = finalizePackageDescription [] (const True) defaultPlatform defaultCompiler []

data ConfigureException = CouldNotSatisfyDependencies PackageVersion [Dependency] deriving (Read,Show,Typeable)

instance Exception ConfigureException


{-
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


-}
