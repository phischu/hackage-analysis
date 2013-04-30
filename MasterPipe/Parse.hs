{-# LANGUAGE DeriveDataTypeable #-}
module MasterPipe.Parse where

import MasterPipe.Types

import Control.Proxy (Proxy,Pipe,request,respond)
import Control.Proxy.Safe (ExceptionP,SafeIO,tryIO,throw,catch)
import Control.Monad (forever)

import Control.Exception (Exception,SomeException,toException)
import Data.Typeable (Typeable)

import Language.Haskell.Exts (parseFileContentsWithMode,SrcLoc)
import Language.Haskell.Exts.Fixity (baseFixities)
import Language.Haskell.Exts.Parser (ParseMode(..),defaultParseMode,ParseResult(ParseOk,ParseFailed))

parseD :: (Proxy p) => () -> Pipe (ExceptionP p) (Package,Configuration,Module,String,ModuleNode) (Package,Configuration,Module,AST,ModuleNode) SafeIO r
parseD () = forever ((do

    (package,configuration,modul,sourcecode,modulenode) <- request ()

    let Module modulename modulepath = modul
        mode = defaultParseMode {parseFilename = modulepath, fixities = Just baseFixities}
        parseresult = parseFileContentsWithMode mode sourcecode

    ast <- case parseresult of
        ParseFailed sourcelocation message -> throw (toException (ParserException sourcelocation message))
        ParseOk ast -> return ast

    respond (package,configuration,modul,ast,modulenode))

        `catch`

    (\e -> tryIO (print (e :: SomeException))))

data ParserException = ParserException SrcLoc String deriving (Show,Typeable)

instance Exception ParserException

{-
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
-}

