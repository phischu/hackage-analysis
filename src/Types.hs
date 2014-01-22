module Types where

import Data.Map (Map)
import Data.Version (Version)
import qualified Language.Haskell.Exts.Annotated as HSE (Module,SrcSpanInfo)

type Repository = Map PackageName (Map VersionNumber FilePath)
type PackageName   = String
type VersionNumber = Version

type ModuleAST = HSE.Module HSE.SrcSpanInfo


