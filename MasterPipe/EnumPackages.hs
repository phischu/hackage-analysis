{-# LANGUAGE OverloadedStrings #-}
module MasterPipe.EnumPackages where

import MasterPipe.Types
import MasterPipe.Database (myCreateNode,myCreateRelationship)

import Control.Proxy (Proxy,Producer,Pipe,mapD,filterD,(>->),request,respond)
import Control.Proxy.Safe (ExceptionP,SafeIO,tryIO,readFileS,catch,throw)

import Control.Monad (forever)

import Control.Exception (SomeException)

import Data.Text (pack)

import Database.Neo4j (Node)


enumpackagesS :: (Proxy p) => () -> Producer (ExceptionP p) (Package,PackageNode,VersionNode) SafeIO ()
enumpackagesS () = ((do

    basenode <- myCreateNode "packagename" "base"

    (readFileS "packages.list" >->
     mapD read >->
     filterD (\(Package name _ _)->name == "base") >->
     createVersion basenode) ())

        `catch`

    (\e -> tryIO (print (e :: SomeException))))

createVersion :: (Proxy p) => Node -> () -> Pipe (ExceptionP p) Package (Package,PackageNode,VersionNode) SafeIO ()
createVersion basenode () = forever (do

    package <- request ()

    let Package _ version _ = package

    versionnode <- myCreateNode "versionname" (pack version)
    myCreateRelationship basenode versionnode "VERSION"

    respond (package,basenode,versionnode))



