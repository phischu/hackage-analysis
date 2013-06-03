{-# LANGUAGE StandaloneDeriving, DeriveGeneric, OverloadedStrings #-}
module MasterPipe where

import Control.Proxy (Proxy,(>->),runProxy)
import Control.Proxy.Safe (SafeIO,trySafeIO,tryIO)
import Control.Proxy.Trans.Either (runEitherK)
import Control.Proxy.Trans.State (evalStateK)
import Control.Error (EitherT(EitherT),runEitherT)
import Control.Monad.Morph (MFunctor,hoist)
import Control.Monad.Trans.Free (FreeT(FreeT),runFreeT)
import Control.Monad.IO.Class (MonadIO,liftIO)
import Control.Exception (throw)

import Data.Map.Strict (empty)

import Database.PropertyGraph (newVertex)
import Database.PropertyGraph.Neo4jSingle (runPropertyGraphT)
import Database.Neo4j (defaultClient)

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
    result <- runEitherT $ runPropertyGraphT defaultClient $ hoist trySafeIO $ runProxy $ evalStateK undefined $ runEitherK $
        enumpackagesS >->
        enumVersionsD >->
        configureD >->
        enummodulesD >->
        preprocessD >->
        parseD >->
        fragmentD
    print result

instance (Functor f) => MFunctor (FreeT f) where
    hoist nat freet = FreeT (nat (do
        next <- runFreeT freet
        return (fmap (hoist nat) next)))

instance MonadIO SafeIO where
    liftIO m = runProxy (runEitherK (const (tryIO m))) >>= either throw return