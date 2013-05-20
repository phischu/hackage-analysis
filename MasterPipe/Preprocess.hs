{-# LANGUAGE OverloadedStrings #-}
module MasterPipe.Preprocess where

import MasterPipe.Types

import MasterPipe.Database (insertVertex)

import Database.PropertyGraph (PropertyGraphT,VertexId)

import Control.Proxy (Proxy,Pipe,request,respond,lift,liftP)
import Control.Proxy.Safe (ExceptionP,SafeIO,tryIO,catch)
import Control.Proxy.Trans.State (StateP,get)
import Control.Monad (forever,void)
import Control.Monad.Morph (hoist)

import Control.Exception (SomeException,evaluate)
import Control.DeepSeq (force)
import Data.Text (pack)

import Language.Preprocessor.Cpphs
    (runCpphs,
    defaultCpphsOptions,CpphsOptions(boolopts),
    defaultBoolOptions,BoolOptions(warnings))

preprocessD :: (Proxy p) => () -> Pipe
    (ExceptionP (StateP VertexId p))
    (PackageVersion,Configuration,Module)
    (PackageVersion,Configuration,Module,String)
    (PropertyGraphT SafeIO)
    r
preprocessD () = forever ((do

    (package,configuration,modul) <- request ()

    let Module _ modulepath = modul
        cpphsoptions = defaultCpphsOptions {boolopts = booloptions}
        booloptions = defaultBoolOptions {warnings = False}

    rawsource <- hoist lift (tryIO (readFile modulepath))
    sourcecode <- hoist lift (tryIO (runCpphs cpphsoptions modulepath rawsource))
    hoist lift (tryIO (evaluate (force sourcecode)))

    respond (package,configuration,modul,sourcecode))

        `catch`

    (\e -> do
        modulevertex <- liftP get
        void (lift (insertVertex "PREPROCESSOREXCEPTION" "exception" (pack (show (e :: SomeException))) modulevertex))))

{-
preprocess :: (Proxy p) => () -> Pipe (ExceptionP p) (PackageVersion,Module,CPPOptions) (PackageVersion,Module,String) (StateT Stats SafeIO) r
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
-}
