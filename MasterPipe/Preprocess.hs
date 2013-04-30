module MasterPipe.Preprocess where

import MasterPipe.Types

import Control.Proxy (Proxy,Pipe,request,respond)
import Control.Proxy.Safe (ExceptionP,SafeIO,tryIO,catch)
import Control.Monad (forever)

import Control.Exception (SomeException,evaluate)
import Control.DeepSeq (force)

import Language.Preprocessor.Cpphs (runCpphs,defaultCpphsOptions)

preprocessD :: (Proxy p) => () -> Pipe (ExceptionP p) (Package,Configuration,Module,ModuleNode) (Package,Configuration,Module,String,ModuleNode) SafeIO r
preprocessD () = forever ((do

    (package,configuration,modul,modulenode) <- request ()

    let Module modulename modulepath = modul

    rawsource <- tryIO (readFile modulepath)
    sourcecode <- tryIO (runCpphs defaultCpphsOptions modulepath rawsource)
    tryIO (evaluate (force sourcecode))

    respond (package,configuration,modul,sourcecode,modulenode))

        `catch`

    (\e -> tryIO (print (e :: SomeException))))

{-
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
-}
