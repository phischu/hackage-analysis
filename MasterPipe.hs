{-# LANGUAGE StandaloneDeriving, DeriveGeneric, OverloadedStrings #-}
module MasterPipe where

import Control.Proxy (Proxy,(>->),runProxy)
import Control.Proxy.Safe (trySafeIO)
import Control.Proxy.Trans.Either (runEitherK)
import Control.Proxy.Trans.State (runStateK)

import Data.Map.Strict (empty)

import Database.PropertyGraph (newVertex)
import Database.PropertyGraph.Neo4jBatch (add)

import MasterPipe.Types (PackageTree(PackageTree),PackageName,Version,ModuleName,Fragment(FunctionFragment))
import MasterPipe.EnumPackages (enumpackagesS)
import MasterPipe.EnumVersions (enumVersionsD)
import MasterPipe.Configure (configureD)
import MasterPipe.EnumModules (enummodulesD)
import MasterPipe.Preprocess (preprocessD)
import MasterPipe.Parse (parseD)
import MasterPipe.Fragment (fragmentD)

masterpipe :: IO ()
masterpipe = do
    result <- trySafeIO $ runProxy $ runStateK (newVertex empty) $ runEitherK $
        enumpackagesS >->
        enumVersionsD >->
        configureD >->
        enummodulesD >->
        preprocessD >->
        parseD >->
        fragmentD
    case result of
        (Left e,_) -> print e
        (Right (),propertygraph) ->
            add "localhost" 7474 propertygraph >>= either print (const (return ()))
