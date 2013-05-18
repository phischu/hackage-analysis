{-# LANGUAGE OverloadedStrings #-}
module MasterPipe.EnumPackages where

import MasterPipe.Types
import MasterPipe.Database

import Database.PropertyGraph (PropertyGraphT,VertexId,newVertex)

import Control.Proxy (Proxy,Producer,lift,liftP,respond)
import Control.Proxy.Safe (ExceptionP,SafeIO)
import Control.Proxy.Trans.State (StateP,put)

import Data.Map (empty)


enumpackagesS :: (Proxy p) => () -> Producer (ExceptionP (StateP VertexId p)) PackageName (PropertyGraphT SafeIO) ()
enumpackagesS () = do

	rootvertex <- lift (newVertex empty)

	packagevertex <- lift (insertVertex "PACKAGE" "packagename" "base" rootvertex)

	liftP (put packagevertex)

	respond "base"


