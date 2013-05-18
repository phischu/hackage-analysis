module MasterPipe.Database where

import MasterPipe.Types

import Database.PropertyGraph (PropertyGraphT,VertexId,newVertex,newEdge)

import Data.Map (empty,singleton)
import Data.Text (Text)

insertVertex :: (Monad m) => Text -> Text -> Text -> VertexId -> PropertyGraphT m VertexId
insertVertex edgelabel propertyname propertyvalue parentvertex = do
	childvertex <- newVertex (singleton propertyname propertyvalue)
	newEdge empty edgelabel parentvertex childvertex
	return childvertex
