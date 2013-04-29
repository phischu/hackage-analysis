module MasterPipe.EnumPackages where

import MasterPipe.Types

import Control.Proxy (Proxy,Producer,mapD,filterD,(>->))
import Control.Proxy.Safe (ExceptionP,SafeIO,readFileS)


enumpackagesS :: (Proxy p) => () -> Producer (ExceptionP p) Package SafeIO ()
enumpackagesS = readFileS "packages.list" >-> mapD read >-> filterD (\(Package name _ _)->name == "base")


