module MasterPipe.EnumPackages where

import MasterPipe.Types

import Control.Proxy
import Control.Proxy.Safe

enumpackagesS :: (Proxy p) => () -> Producer (ExceptionP p) Package SafeIO ()
enumpackagesS = readFileS "packages.list" >-> mapD read >-> filterD (\(Package name _ _)->name == "base")


