{-# LANGUAGE DeriveDataTypeable,OverloadedStrings #-}
module MasterPipe.EnumPackages where

import MasterPipe.Types

import Control.Proxy (Proxy,Producer,Pipe,mapD,filterD,(>->),request,respond)
import Control.Proxy.Safe (ExceptionP,SafeIO,tryIO,readFileS,catch,throw)

import Control.Monad (forever)

import Control.Exception (Exception,SomeException,toException)
import Data.Typeable (Typeable)

import Database.Neo4j (Node,createNode,defaultClient,createRelationship)

import Data.Text (Text,pack)
import Data.Aeson (toJSON)


enumpackagesS :: (Proxy p) => () -> Producer (ExceptionP p) (Package,Node) SafeIO ()
enumpackagesS () = ((do

    basenode <- myCreateNode "packagename" "base"

    (readFileS "packages.list" >->
     mapD read >->
     filterD (\(Package name _ _)->name == "base") >->
     createVersion basenode) ())

        `catch`

    (\e -> tryIO (print (e :: SomeException))))

createVersion :: (Proxy p) => Node -> () -> Pipe (ExceptionP p) Package (Package,Node) SafeIO ()
createVersion basenode () = forever (do

    package <- request ()

    let Package _ version _ = package

    versionnode <- myCreateNode "versionname" (pack version)
    tryIO (createRelationship defaultClient basenode versionnode "VERSION" []) >>= either (throw . toException . Neo4jException) return

    respond (package,versionnode))

myCreateNode :: (Proxy p) => Text -> Text -> (ExceptionP p) a' a b' b SafeIO Node
myCreateNode propertyname propertyvalue =
    tryIO (createNode defaultClient [(propertyname,toJSON propertyvalue)]) >>=
    either (throw . toException . Neo4jException) return

data Neo4jException = Neo4jException String deriving (Read,Show,Typeable)

instance Exception Neo4jException


