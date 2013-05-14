module MasterPipe.EnumPackages where

import MasterPipe.Types

import Control.Proxy
import Control.Proxy.Safe

enumpackagesS :: (Proxy p) => () -> Producer (ExceptionP p) PackageName SafeIO ()
enumpackagesS () = respond "base"


