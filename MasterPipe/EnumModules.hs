module MasterPipe.EnumModules where

import MasterPipe.Types

import Control.Proxy
import Control.Proxy.Safe

enummodulesD :: (Proxy p) => () -> Pipe (ExceptionP p) (Package,Configuration) (Package,Configuration,Module) SafeIO r
enummodulesD = undefined

{-
modules :: (Proxy p,CheckP p,Monad m) => () -> Pipe p (Package,Configuration) (Package,Module,CPPOptions) m ()
modules () = runIdentityP $ forever $ do
    (package,Configuration configuration) <- request ()
    case configuration of
        Left _ -> return ()
        Right (modules,cppoptions) -> forM_ modules (\modul->respond (package,modul,cppoptions))
-}
