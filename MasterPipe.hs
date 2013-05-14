{-# LANGUAGE StandaloneDeriving, DeriveGeneric, OverloadedStrings #-}
module MasterPipe where

import Control.Proxy (Proxy,(>->),runProxy)
import Control.Proxy.Safe (trySafeIO)
import Control.Proxy.Trans.Either (runEitherK)
import Control.Proxy.Trans.Writer (runWriterK)

import Data.Map.Strict (Map,empty,singleton,elems,traverseWithKey)
import Control.Monad (forM_)
import Data.Text (pack)

import Database.PropertyGraph (PropertyGraph,newVertex,newEdge,VertexId,Label,Key,Value,Properties)
import Database.PropertyGraph.Neo4jBatch (add,convertPropertyGraphToNeo4jBatch)

import MasterPipe.Types (PackageTree(PackageTree),PackageName,Version,ModuleName,Fragment(FunctionFragment))
import MasterPipe.EnumPackages (enumpackagesS)
import MasterPipe.EnumVersions (enumVersionsD)
import MasterPipe.Configure (configureD)
import MasterPipe.EnumModules (enummodulesD)
import MasterPipe.Preprocess (preprocessD)
import MasterPipe.Parse (parseD)
import MasterPipe.Fragment (fragmentD)
import MasterPipe.Database (databaseC)

masterpipe :: IO ()
masterpipe = do
    result <- trySafeIO $ runProxy $ runWriterK $ runEitherK $
        enumpackagesS >->
        enumVersionsD >->
        configureD >->
        enummodulesD >->
        preprocessD >->
        parseD >->
        fragmentD >->
        databaseC
    case result of
        (Left e,_) -> print e
        (Right (),packagetree) ->
            add "localhost" 7474 (packageTreeToPropertyGraph packagetree) >>= either print (const (return ()))

packageTreeToPropertyGraph :: PackageTree -> PropertyGraph [VertexId]
packageTreeToPropertyGraph (PackageTree packagetree) = insertPackages packagetree

insertPackages :: Map PackageName (Map Version (Map String (Map ModuleName [Fragment]))) -> PropertyGraph [VertexId]
insertPackages = insertWhatever
    (\packagename -> singleton "packagename" (pack packagename))
    insertVersions
    "VERSION"

insertVersions :: Map Version (Map String (Map ModuleName [Fragment])) -> PropertyGraph [VertexId]
insertVersions = insertWhatever
    (\versionname -> singleton "versionname" (pack versionname))
    insertVariants
    "VARIANT"

insertVariants :: Map String (Map ModuleName [Fragment]) -> PropertyGraph [VertexId]
insertVariants = insertWhatever
    (\configuration -> singleton "configuration" (pack configuration))
    insertModules
    "MODULE"

insertModules :: Map ModuleName [Fragment] -> PropertyGraph [VertexId]
insertModules = insertWhatever
    (\modulename -> singleton "modulename" (pack modulename))
    insertFragments
    "FRAGMENT"

insertFragments :: [Fragment] -> PropertyGraph [VertexId]
insertFragments = mapM (\(FunctionFragment functionname) ->
    newVertex (singleton "functionname" (pack functionname)))

insertWhatever :: (k -> Properties) -> (v -> PropertyGraph [VertexId]) -> Label -> Map k v -> PropertyGraph [VertexId]
insertWhatever f g label = fmap elems . (traverseWithKey (\key value -> do
    vertexid  <- newVertex (f key)
    childrenids <- g value
    forM_ childrenids (\childid -> do
        newEdge empty label vertexid childid)
    return vertexid))





