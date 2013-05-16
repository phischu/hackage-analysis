{-# LANGUAGE OverloadedStrings #-}
module MasterPipe.EnumPackages where

import MasterPipe.Types
import MasterPipe.Database

import Database.PropertyGraph (PropertyGraph,VertexId)

import Control.Proxy
import Control.Proxy.Safe
import Control.Proxy.Trans.State (StateP,modify)


enumpackagesS :: (Proxy p) => () -> Producer (ExceptionP (StateP (PropertyGraph VertexId) p)) PackageName SafeIO ()
enumpackagesS () = do
	liftP (modify (>>=(insertVertex "PACKAGE" "packagename" "base")))
	respond "base"


