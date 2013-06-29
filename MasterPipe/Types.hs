module MasterPipe.Types where

import Distribution.PackageDescription (PackageDescription,FlagAssignment)
import Distribution.System (Platform)
import Distribution.Compiler (CompilerId)

import qualified Language.Haskell.Exts.Syntax as AST

import Data.Map.Strict (Map,empty,unionWith)
import Data.Monoid (Monoid(mempty,mappend))

type PackageName = String

data PackageVersion = PackageVersion PackageName Version FilePath deriving (Show,Read,Eq,Ord)

type Version = String

data Configuration = Configuration FlagAssignment Platform CompilerId PackageDescription deriving (Show,Read)

type ModuleName = String
data Module = Module ModuleName FilePath deriving (Show,Read)

type AST = AST.Module

type FragmentName = String
data Fragment = ValueFragment FragmentName
              | TypeFragment FragmentName
              | ClassFragment FragmentName deriving (Eq,Show,Read)

newtype PackageTree = PackageTree (Map PackageName (Map Version (Map String (Map ModuleName [Fragment])))) deriving (Eq,Show,Read)

instance Monoid PackageTree where
	mempty = PackageTree empty
	(PackageTree p1) `mappend` (PackageTree p2) = PackageTree (unionWith (unionWith (unionWith (unionWith mappend))) p1 p2)
