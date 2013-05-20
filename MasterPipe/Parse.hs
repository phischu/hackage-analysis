{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module MasterPipe.Parse where

import MasterPipe.Types

import Database.PropertyGraph (PropertyGraphT,VertexId)

import MasterPipe.Database (insertVertex)

import Control.Proxy (Proxy,Pipe,request,respond,lift,liftP)
import Control.Proxy.Safe (ExceptionP,SafeIO,tryIO,throw,catch)
import Control.Proxy.Trans.State (StateP,get)
import Control.Monad (forever,void)
import Control.Monad.Morph (hoist)

import Control.Exception (Exception,SomeException,toException)
import Data.Typeable (Typeable)
import Data.Text (pack)

import Language.Haskell.Exts (parseFileContentsWithMode,SrcLoc)
import Language.Haskell.Exts.Fixity (baseFixities)
import Language.Haskell.Exts.Parser (ParseMode(..),defaultParseMode,ParseResult(ParseOk,ParseFailed))

parseD :: (Proxy p) => () -> Pipe
    (ExceptionP (StateP VertexId p))
    (PackageVersion,Configuration,Module,String)
    (PackageVersion,Configuration,Module,AST)
    (PropertyGraphT SafeIO)
    r
parseD () = forever ((do

    (package,configuration,modul,sourcecode) <- request ()

    let Module modulename modulepath = modul
        mode = defaultParseMode {parseFilename = modulepath, fixities = Just baseFixities}
        parseresult = parseFileContentsWithMode mode sourcecode

    ast <- case parseresult of
        ParseFailed sourcelocation message -> throw (toException (ParserException sourcelocation message))
        ParseOk ast -> return ast

    respond (package,configuration,modul,ast))

        `catch`

    (\e -> do
        modulevertex <- liftP get
        void (lift (insertVertex "PARSEREXCEPTION" "exception" (pack (show (e :: SomeException))) modulevertex))))

data ParserException = ParserException SrcLoc String deriving (Show,Typeable)

instance Exception ParserException
