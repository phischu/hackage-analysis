module Common where

import Data.Map (Map,traverseWithKey)
import Data.Version (Version)
import qualified Language.Haskell.Exts.Annotated as HSE (Module,SrcSpanInfo)

type Repository a = Map PackageName (Map VersionNumber a)
type SourceRepository = Repository FilePath
type ParsedRepository = Repository FilePath
type PackageName   = String
type VersionNumber = Version

type ModuleAST = HSE.Module HSE.SrcSpanInfo

traverseRepository :: (PackageName -> VersionNumber -> a -> IO b) -> Repository a -> IO (Repository b)
traverseRepository f =
    traverseWithKey (\packagename ->
        traverseWithKey (\versionnumber a ->
            f packagename versionnumber a))
