module MasterPipe.Database where

import MasterPipe.Types

import Control.Proxy (Proxy,Consumer,request,liftP)
import Control.Proxy.Safe (ExceptionP,SafeIO)
import Control.Monad (forever,forM_,when)
import Control.Proxy.Trans.Writer (WriterP,tell)
import Data.Map (Map,singleton)

databaseC :: (Proxy p) => () -> Consumer (ExceptionP (WriterP PackageTree p)) (PackageVersion,Configuration,Module,Fragment) SafeIO r
databaseC () = forever $ do
	(package,configuration,modul,fragment) <- request ()
	let PackageVersion packagename version _ = package
	    Configuration flagassignment platform compilerid _ = configuration
	    Module modulename _ = modul
	    fragmentmap = PackageTree (
	    	singleton packagename 
	        (singleton version 
	        (singleton (show (flagassignment,platform,compilerid))
	        (singleton modulename [fragment]))))
	liftP (tell fragmentmap)