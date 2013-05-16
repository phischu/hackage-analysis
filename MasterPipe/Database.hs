module MasterPipe.Database where

import MasterPipe.Types

import Database.PropertyGraph (PropertyGraph,VertexId,newVertex,newEdge)

import Control.Proxy (Proxy,Consumer,request,liftP)
import Control.Proxy.Safe (ExceptionP,SafeIO)
import Control.Monad (forever,forM_,when)

import Data.Map (empty,singleton)
import Data.Text (Text)

insertVertex :: Text -> Text -> Text -> VertexId -> PropertyGraph VertexId
insertVertex edgelabel propertyname propertyvalue parentvertex = do
	childvertex <- newVertex (singleton propertyname propertyvalue)
	newEdge empty edgelabel parentvertex childvertex
	return childvertex
