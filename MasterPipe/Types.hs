module MasterPipe.Types where

import Distribution.PackageDescription (PackageDescription,FlagAssignment)
import Distribution.System (Platform)
import Distribution.Compiler (CompilerId)


data Package = Package Name Version FilePath deriving (Show,Read,Eq,Ord)
type Name = String
type Version = String

data Configuration = Configuration FlagAssignment Platform CompilerId PackageDescription deriving (Show,Read)

data Module = Module Name FilePath deriving (Show,Read)

data AST = AST

