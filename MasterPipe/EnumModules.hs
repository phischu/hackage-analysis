{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, StandaloneDeriving #-}
module MasterPipe.EnumModules where

import MasterPipe.Types
import MasterPipe.Database

import Database.PropertyGraph (PropertyGraphT,VertexId)

import Control.Proxy (Proxy,Pipe,request,respond,liftP,lift)
import Control.Proxy.Safe (ExceptionP,SafeIO,throw,tryIO,catch)
import Control.Proxy.Trans.State (StateP,get,put)
import Control.Monad (forever,filterM,forM_,void)
import Control.Monad.Morph (hoist)

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

enummodulesD :: (Proxy p) => () -> Pipe
    (ExceptionP (StateP VertexId p))
    (PackageVersion,Configuration)
    (PackageVersion,Configuration,Module)
    (PropertyGraphT SafeIO)
    r
enummodulesD () = forever ((do

    (package,configuration) <- request ()
    variantvertex <- liftP get

    let PackageVersion _ _ packagepath = package
        Configuration _ _ _ packagedescription = configuration

    librarysection <- maybe
        (throw (toException NoLibrary))
        return
        (library packagedescription)

    let modulenames = libModules librarysection
        sourcedirs = hsSourceDirs (libBuildInfo librarysection)

        potentialPaths modulename = do
            directory <- sourcedirs
            extension <- [".hs",".lhs"]
            return (packagepath ++ directory ++ "/" ++ toFilePath modulename ++ extension)

        valid path = doesFileExist path

        findPaths modulename = hoist lift (tryIO (filterM valid (potentialPaths modulename)))

    forM_ modulenames (\modulename -> do

        paths <- findPaths modulename
        let modulenamestring = (show (disp modulename))
        modulevertex <- lift (insertModule (pack modulenamestring) variantvertex)

        case paths of

            [] -> lift (insertException (pack "module not found") modulevertex) >> return ()

            [modulepath] -> do
                liftP (put modulevertex)
                respond (package,configuration,Module modulenamestring modulepath)

            _ -> lift (insertException (pack "module found at multiple paths") modulevertex >> return ())))

        `catch`

    (\NoLibrary -> do
        variantvertex <- liftP get
        void (lift (insertVertex "VARIANTEXCEPTION" "exception" "no library" variantvertex)))

        `catch`

    (\e -> hoist lift (tryIO (print (e :: SomeException)))))

data EnumModulesException = NoLibrary deriving (Show,Typeable)

instance Exception EnumModulesException

insertModule :: (Monad m) => Text -> VertexId -> PropertyGraphT m VertexId
insertModule = insertVertex "MODULE" "modulename"

insertException :: (Monad m) => Text -> VertexId -> PropertyGraphT m VertexId
insertException = insertVertex "MODULEEXCEPTION" "exception"

