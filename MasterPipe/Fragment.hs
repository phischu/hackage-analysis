module MasterPipe.Fragment where

import MasterPipe.Types
import MasterPipe.Database (myCreateNode,myCreateRelationship)

import Control.Proxy (Proxy,Pipe,request,respond)
import Control.Proxy.Safe (ExceptionP,SafeIO,tryIO)
import Control.Monad (forever,forM_,when)

import Data.Text (pack)

import qualified Language.Haskell.Exts as AST (
    Module(Module),ModuleName(ModuleName),Decl(FunBind),
    Match(Match),Name(Ident,Symbol))

fragmentD :: (Proxy p) => () -> Pipe (ExceptionP p) (Package,Configuration,Module,AST,ModuleNode) (Package,Configuration,Module,Fragment) SafeIO r
fragmentD () = forever (do

    (package,configuration,modul,ast,modulenode) <- request ()

    let Module modulename1 _ = modul
        AST.Module srcloc (AST.ModuleName modulename2) pragmas warning exports imports declarations = ast

        fragments = do
            declaration <- declarations
            case declaration of
                AST.FunBind [] -> []
                AST.FunBind (AST.Match _ astname _ _ _ _:_) -> case astname of
                    AST.Ident fragmentname -> [FunctionFragment fragmentname]
                    AST.Symbol fragmentname -> [FunctionFragment fragmentname]
                _ -> []

    when (modulename1 /= modulename2)
        (tryIO (print ("Modulenames do not match: " ++ modulename1 ++ " /= " ++ modulename2)))

    forM_ fragments (\fragment -> (do
        let FunctionFragment functionname = fragment
        fragmentnode <- myCreateNode (pack "functionname") (pack functionname)
        myCreateRelationship modulenode fragmentnode "FRAGMENT"
        respond (package,configuration,modul,fragment))))



