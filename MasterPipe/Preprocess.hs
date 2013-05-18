module MasterPipe.Preprocess where

import MasterPipe.Types

import Database.PropertyGraph (PropertyGraphT,VertexId)

import Control.Proxy (Proxy,Pipe,request,respond,lift)
import Control.Proxy.Safe (ExceptionP,SafeIO,tryIO,catch)
import Control.Monad (forever)
import Control.Monad.Morph (hoist)

import Control.Exception (SomeException,evaluate)
import Control.DeepSeq (force)

import Language.Preprocessor.Cpphs (runCpphs,defaultCpphsOptions)

preprocessD :: (Proxy p) => () -> Pipe
    (ExceptionP p)
    (PackageVersion,Configuration,Module)
    (PackageVersion,Configuration,Module,String)
    (PropertyGraphT SafeIO)
    r
preprocessD () = forever ((do

    (package,configuration,modul) <- request ()

    let Module modulename modulepath = modul

    rawsource <- hoist lift (tryIO (readFile modulepath))
    sourcecode <- hoist lift (tryIO (runCpphs defaultCpphsOptions modulepath rawsource))
    hoist lift (tryIO (evaluate (force sourcecode)))

    respond (package,configuration,modul,sourcecode))

        `catch`

    (\e -> hoist lift (tryIO (print (e :: SomeException)))))

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
