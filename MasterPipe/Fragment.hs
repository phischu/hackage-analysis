{-# LANGUAGE OverloadedStrings #-}
module MasterPipe.Fragment where

import MasterPipe.Types
import MasterPipe.Database

import Database.PropertyGraph (PropertyGraph,VertexId)

import Control.Proxy (Proxy,Pipe,request,respond,liftP)
import Control.Proxy.Safe (ExceptionP,SafeIO,tryIO)
import Control.Proxy.Trans.State (StateP,modify)
import Control.Monad (forever,forM_,when)

import Data.Text (Text,pack)

import qualified Language.Haskell.Exts as AST (
    Module(Module),ModuleName(ModuleName),Decl(FunBind),
    Match(Match),Name(Ident,Symbol))

fragmentD :: (Proxy p) => () -> Pipe (ExceptionP (StateP (PropertyGraph VertexId) p)) (PackageVersion,Configuration,Module,AST) (PackageVersion,Configuration,Module,Fragment) SafeIO r
fragmentD () = forever (do

    (package,configuration,modul,ast) <- request ()

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

    forM_ fragments (\fragment@(FunctionFragment functionname) -> (do
        liftP (modify (>>= insertFragment (pack functionname)))
        respond (package,configuration,modul,fragment))))

insertFragment :: Text -> VertexId -> PropertyGraph VertexId
insertFragment = insertVertex "FRAGMENT" "functionname"

