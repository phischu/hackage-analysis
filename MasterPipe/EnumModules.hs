{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module MasterPipe.EnumModules where

import MasterPipe.Types
import MasterPipe.Database

import Database.PropertyGraph (PropertyGraph,VertexId)

import Control.Proxy (Proxy,Pipe,request,respond,liftP)
import Control.Proxy.Safe (ExceptionP,SafeIO,throw,tryIO,catch)
import Control.Proxy.Trans.State (StateP,modify)
import Control.Monad (forever,filterM,forM_,when,guard)

import Distribution.PackageDescription
    (PackageDescription(library),
    libModules,Library(libBuildInfo),
    BuildInfo(hsSourceDirs))
import Distribution.Text (disp)
import Distribution.ModuleName (ModuleName,toFilePath)

import System.Directory (doesFileExist)

import Control.Exception (Exception,SomeException,toException)
import Data.Typeable (Typeable)
import Data.Text (Text,pack)

enummodulesD :: (Proxy p) => () -> Pipe (ExceptionP (StateP (PropertyGraph VertexId) p)) (PackageVersion,Configuration) (PackageVersion,Configuration,Module) SafeIO r
enummodulesD () = forever ((do

    (package,configuration) <- request ()

    let PackageVersion packagename version packagepath = package
        Configuration _ _ _ packagedescription = configuration

    librarysection <- maybe
        (throw (toException NoLibrary))
        return
        (library packagedescription)

    let modulenames = libModules librarysection
        sourcedirs = hsSourceDirs (libBuildInfo librarysection)
        potentialModules = do
            modulename <- modulenames
            directory <- sourcedirs
            extension <- [".hs",".lhs"]
            return (Module (show (disp modulename)) (packagepath ++ directory ++ "/" ++ toFilePath modulename ++ extension))
        valid (Module _ path) = doesFileExist path
    modules <- tryIO (filterM valid potentialModules)

    when (length modulenames /= length modules) (do
        let modulesnotfound = filter (notfound modules) modulenames
            notfound modules modulename = null (do
                (Module foundname _) <- modules
                guard (foundname == show (disp modulename)))
        tryIO (print (toException (NotAllModuleFilesFound package modulesnotfound))))

    forM_ modules (\m@(Module modulename modulepath) -> (do
        liftP (modify (>>= insertModule (pack modulename)))
        respond (package,configuration,m))))

        `catch`

    (\e -> tryIO (print (e :: SomeException))))

data EnumModulesException = NoLibrary
                          | NotAllModuleFilesFound PackageVersion [Distribution.ModuleName.ModuleName] deriving (Read,Show,Typeable)

instance Exception EnumModulesException

insertModule :: Text -> VertexId -> PropertyGraph VertexId
insertModule = insertVertex "MODULE" "modulename"

{-
modules :: (Proxy p,CheckP p,Monad m) => () -> Pipe p (PackageVersion,Configuration) (PackageVersion,Module,CPPOptions) m ()
modules () = runIdentityP $ forever $ do
    (package,Configuration configuration) <- request ()
    case configuration of
        Left _ -> return ()
        Right (modules,cppoptions) -> forM_ modules (\modul->respond (package,modul,cppoptions))
-}
