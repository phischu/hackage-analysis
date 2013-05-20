{-# LANGUAGE OverloadedStrings #-}
module MasterPipe.EnumVersions where

import MasterPipe.Types
import MasterPipe.Database

import Database.PropertyGraph (PropertyGraphT,VertexId,newVertex,newEdge)

import Control.Proxy (Proxy,Pipe,request,respond,lift,liftP)
import Control.Proxy.Safe (ExceptionP,SafeIO,tryIO,catch)
import Control.Proxy.Trans.State (StateP,get,put)
import Control.Monad.Morph (hoist)

import Control.Monad (forever,forM_,void)
import Control.Exception (SomeException)
import Data.Text (Text,pack)
import Data.Map (empty,fromList)

import Data.Version (Version(Version),parseVersion,showVersion)
import Text.ParserCombinators.ReadP (readP_to_S)

enumVersionsD :: (Proxy p) => () -> Pipe (ExceptionP (StateP VertexId p)) PackageName PackageVersion (PropertyGraphT SafeIO) r
enumVersionsD () = forever ((do

    packagename <- request ()
    packagevertex <- liftP get

    packagelist <- hoist lift (tryIO (readFile "packages.list" >>= return . map read . lines))
    let packageversions = filter (\(PackageVersion name _ _) -> name == packagename) packagelist

    forM_ packageversions (\packageversion -> (do

        let PackageVersion _ versionname _ = packageversion
            versionparses = readP_to_S parseVersion versionname

        case filter (null.snd) versionparses of

            [(Version (v1:v2:rest) tags,"")] -> do

                let majorversion = showVersion (Version [v1,v2] [])
                    minorversion = showVersion (Version rest tags)

                versionvertex <- lift (insertVersion
                    (pack versionname)
                    (pack majorversion)
                    (pack minorversion)
                    packagevertex)
                liftP (put versionvertex)
                respond packageversion

            _ -> do
                void (lift (insertVertex
                    "VERSIONEXCEPTION"
                    "exception"
                    (pack ("version parse failed: "++versionname))
                    packagevertex)))))

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
