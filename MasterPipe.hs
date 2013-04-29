{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}
module MasterPipe where

import Control.Proxy
import Control.Proxy.Safe
import Control.Proxy.Safe.Prelude

import Control.Monad (filterM,when,forM_,void)
import System.Directory (doesFileExist,createDirectoryIfMissing)
import System.FilePath
import Control.Exception (ErrorCall(ErrorCall),SomeException(SomeException),IOException,evaluate)
import Control.DeepSeq (force)

import Distribution.PackageDescription
    (PackageDescription(..),Library(..),libModules,BuildInfo(..),
     FlagAssignment,GenericPackageDescription)
import Distribution.PackageDescription.Configuration (finalizePackageDescription)
import Distribution.Package (Dependency)
import Distribution.Verbosity (silent)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.System (Platform(Platform),Arch(I386),OS(Linux))
import Distribution.Compiler (CompilerId(CompilerId),CompilerFlavor(GHC))
import Distribution.Text (disp)
import Distribution.ModuleName (toFilePath)
import qualified Data.Version as V (Version(Version))

import Language.Preprocessor.Cpphs (runCpphs,defaultCpphsOptions)
import Data.List (isPrefixOf)

import Language.Haskell.Exts (parseFileContentsWithMode)
import Language.Haskell.Exts.Fixity (baseFixities)
import Language.Haskell.Exts.Parser (ParseMode(..),defaultParseMode,ParseResult(ParseOk,ParseFailed))
import qualified Language.Haskell.Exts.Syntax as AST

import GHC.Generics
import qualified Data.ByteString.Lazy as BS
import Data.Aeson.Generic

import Control.Monad.State.Strict (execStateT,modify,get)

import Data.Map.Strict (Map,empty,insertWith,toList,delete)
import qualified Data.Map.Strict as M (map)
import Data.Set (Set,union,singleton,size)
import Data.List (foldl')
import Visualization (cppflags)

import MasterPipe.EnumPackages (enumpackagesS)
import MasterPipe.Configure (configureD)
import MasterPipe.EnumModules (enummodulesD)
import MasterPipe.Preprocess (preprocessD)
import MasterPipe.Parse (parseD)
import MasterPipe.Fragment (fragmentD)
import MasterPipe.Database (databaseC)

masterpipe :: IO ()
masterpipe = do
    runSafeIO $ runProxy $ runEitherK $
        enumpackagesS >->
        configureD >->
        enummodulesD >->
        preprocessD >->
        parseD >->
        fragmentD

