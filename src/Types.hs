module Types where

import Data.Map (Map)
import Data.Version (Version)
import qualified Language.Haskell.Exts.Annotated as HSE (Module,SrcSpanInfo)

type Repository a = Map PackageName (Map VersionNumber a)
type SourceRepository = Repository FilePath
type ParsedRepository = Repository FilePath
type PackageName   = String
type VersionNumber = Version

type ModuleAST = HSE.Module HSE.SrcSpanInfo


