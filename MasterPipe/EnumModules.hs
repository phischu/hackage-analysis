{-# LANGUAGE DeriveDataTypeable,OverloadedStrings #-}
module MasterPipe.EnumModules where

import MasterPipe.Types
import MasterPipe.Database (myCreateNode,myCreateRelationship)

import Control.Proxy (Proxy,Pipe,request,respond)
import Control.Proxy.Safe (ExceptionP,SafeIO,throw,tryIO,catch)
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

import Data.Text (pack)

enummodulesD :: (Proxy p) => () -> Pipe (ExceptionP p) (Package,Configuration,PackageNode,VersionNode,VariantNode) (Package,Configuration,Module,PackageNode,VersionNode,VariantNode,ModuleNode) SafeIO r
enummodulesD () = forever ((do

    (package,configuration,packagenode,versionnode,variantnode) <- request ()

    let Package packagename version packagepath = package
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

    forM_ modules (\m -> (do
        let Module modulename _ = m
        modulenode <- myCreateNode "modulename" (pack modulename)
        myCreateRelationship variantnode modulenode "MODULE"
        respond (package,configuration,m,packagenode,versionnode,variantnode,modulenode))))

        `catch`

    (\e -> tryIO (print (e :: SomeException))))

data EnumModulesException = NoLibrary
                          | NotAllModuleFilesFound Package [ModuleName] deriving (Read,Show,Typeable)

instance Exception EnumModulesException

{-
modules :: (Proxy p,CheckP p,Monad m) => () -> Pipe p (Package,Configuration) (Package,Module,CPPOptions) m ()
modules () = runIdentityP $ forever $ do
    (package,Configuration configuration) <- request ()
    case configuration of
        Left _ -> return ()
        Right (modules,cppoptions) -> forM_ modules (\modul->respond (package,modul,cppoptions))
-}
