{-# LANGUAGE OverloadedStrings #-}
module MasterPipe.EnumPackages where

import MasterPipe.Types
import MasterPipe.Database

import Database.PropertyGraph (PropertyGraphT,VertexId,newVertex)

import Control.Proxy (Proxy,Producer,lift,liftP,respond)
import Control.Proxy.Safe (ExceptionP,SafeIO,tryIO,catch)
import Control.Proxy.Trans.State (StateP,put)
import Control.Monad.Morph (hoist)

import Control.Monad (forM_)
import Control.Exception (SomeException)

import Data.Text (pack)
import Data.Map (empty)
import Data.List (nub)


enumpackagesS :: (Proxy p) => () -> Producer (ExceptionP (StateP VertexId p)) PackageName (PropertyGraphT SafeIO) ()
enumpackagesS () = (do

	rootvertex <- lift (newVertex empty)

	packagelist <- hoist lift (tryIO (readFile "packages.list" >>= return . map read . lines))

	let packagenames = nub (map (\(PackageVersion packagename _ _) -> packagename) packagelist)

	forM_ packagenames (\packagename -> do
	    packagevertex <- lift (insertVertex "PACKAGE" "packagename" (pack packagename) rootvertex)
	    liftP (put packagevertex)
	    respond packagename)

        `catch`

    (\e -> hoist lift (tryIO (print (e :: SomeException)))))


