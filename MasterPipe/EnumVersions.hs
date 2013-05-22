{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module MasterPipe.EnumVersions where

import MasterPipe.Types

import Database.PropertyGraph (PropertyGraphT,VertexId,newVertex,newEdge)

import Control.Proxy (Proxy,Pipe,request,respond,lift,liftP)
import Control.Proxy.Safe (ExceptionP,SafeIO,tryIO,catch)
import Control.Proxy.Trans.State (StateP,get,put)
import Control.Monad.Morph (hoist)

import Control.Monad (forever,forM,forM_,guard)
import Control.Exception (SomeException)
import Data.Text (Text,pack)
import Data.Map (empty,fromList)
import Data.List (sortBy)
import Data.Function (on)

import Data.Version (Version(Version),parseVersion,showVersion)
import Text.ParserCombinators.ReadP (readP_to_S)

enumVersionsD :: (Proxy p) => () -> Pipe (ExceptionP (StateP VertexId p)) PackageName PackageVersion (PropertyGraphT SafeIO) r
enumVersionsD () = forever ((do

    packagename <- request ()
    packagevertex <- liftP get

    packagelist <- hoist lift (tryIO (readFile "packages.list" >>= return . map read . lines))

    let versions = sortBy (compare `on` snd) (do
            packageversion@(PackageVersion name versionname _) <- packagelist
            guard (name == packagename)
            (version,"") <- readP_to_S parseVersion versionname
            return (packageversion,version))

    versionvertices <- forM versions (\(packageversion,version) -> do

        let majorversion = case version of
                Version (v1:v2:_)    _  -> showVersion (Version [v1,v2] [])
                Version versionparts _  -> showVersion (Version versionparts [])
            minorversion = case version of
                Version (_:_:rest) tags -> showVersion (Version rest tags)
                Version _          tags -> showVersion (Version [] tags)

        versionvertex <- lift (insertVersion
            (pack (showVersion version))
            (pack majorversion)
            (pack minorversion)
            packagevertex)

        liftP (put versionvertex)
        respond packageversion
        return versionvertex)

    forM_ (pairs versionvertices) (\(versionvertex1,versionvertex2) -> 
        lift (newEdge empty "NEXTVERSION" versionvertex1 versionvertex2)))

        `catch`

    (\e -> hoist lift (tryIO (print (e::SomeException)))))


insertVersion :: (Monad m) => Text -> Text -> Text -> VertexId -> PropertyGraphT m VertexId
insertVersion versionname majorversion minorversion packagevertex = do
    versionvertex <- newVertex (fromList [
        ("versionname" ,versionname),
        ("majorversion",majorversion),
        ("minorversion",minorversion)])
    newEdge empty "VERSION" packagevertex versionvertex
    return versionvertex

pairs :: [a] -> [(a,a)]
pairs [] = []
pairs [_] = []
pairs (x1:x2:xs) = (x1,x2):pairs (x2:xs)
