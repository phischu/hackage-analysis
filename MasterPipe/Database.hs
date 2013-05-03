{-# LANGUAGE DeriveDataTypeable #-}
module MasterPipe.Database where

import MasterPipe.Types

import Control.Proxy (Proxy)
import Control.Proxy.Safe (ExceptionP,SafeIO,tryIO,throw)

import Control.Exception (Exception,SomeException,toException)
import Data.Typeable (Typeable)

import Data.Text (Text,pack)
import Data.Aeson (toJSON)

import Database.Neo4j (Node,createNode,defaultClient,createRelationship,Relationship)

data Neo4jException = Neo4jException String deriving (Read,Show,Typeable)

instance Exception Neo4jException

myCreateNode :: (Proxy p) => Text -> Text -> (ExceptionP p) a' a b' b SafeIO Node
myCreateNode propertyname propertyvalue =
    tryIO (createNode defaultClient [(propertyname,toJSON propertyvalue)]) >>=
    either (throw . toException . Neo4jException) return

myCreateRelationship :: (Proxy p) => Node -> Node -> String -> (ExceptionP p) a' a b' b SafeIO Relationship
myCreateRelationship from to name =
    tryIO (createRelationship defaultClient from to name []) >>=
    either (throw . toException . Neo4jException) return

