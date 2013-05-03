module MasterPipe.Database where

import MasterPipe.Types

import Control.Proxy (Proxy,Consumer)
import Control.Proxy.Safe (ExceptionP,SafeIO)
import Control.Monad (forever,forM_,when)

databaseC :: (Proxy p) => () -> Consumer (ExceptionP p) (Package,Configuration,Module,Fragment) SafeIO ()
databaseC = undefined