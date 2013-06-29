{-# LANGUAGE OverloadedStrings #-}
module MasterPipe.Fragment where

import MasterPipe.Types
import MasterPipe.Database

import Database.PropertyGraph (PropertyGraphT,VertexId,newVertex,newEdge)

import Control.Proxy (Proxy,Pipe,request,respond,liftP,lift)
import Control.Proxy.Safe (ExceptionP,SafeIO,tryIO)
import Control.Proxy.Trans.State (StateP,get)
import Control.Monad (forever,forM_,when)
import Control.Monad.Morph (hoist)

import Data.Text (Text,pack)
import Data.Map (empty,singleton)

import Data.Aeson.Generic (toJSON)

import qualified Language.Haskell.Exts as AST (
    Module(Module),ModuleName(ModuleName),Decl(FunBind),
    ExportSpec,
    Match(Match),Name(Ident,Symbol),
    prettyPrint)

fragmentD :: (Proxy p) => () -> Pipe
    (ExceptionP (StateP VertexId p))
    (PackageVersion,Configuration,Module,AST)
    (PackageVersion,Configuration,Module,Fragment)
    (PropertyGraphT SafeIO)
    r
fragmentD () = forever (do

    (package,configuration,modul,ast) <- request ()
    modulevertex <- liftP get

    let Module modulename1 _ = modul
        AST.Module srcloc (AST.ModuleName modulename2) pragmas warning maybeExports imports declarations = ast

        fragments = do
            declaration <- declarations
            case declaration of
                AST.FunBind [] -> []
                AST.FunBind (AST.Match _ astname _ _ _ _:_) -> case astname of
                    AST.Ident fragmentname -> [FunctionFragment fragmentname]
                    AST.Symbol fragmentname -> [FunctionFragment fragmentname]
                _ -> []

    lift (insertExportList modulevertex maybeExports)

    when (modulename1 /= modulename2)
        (hoist lift ((tryIO (print ("Modulenames do not match: " ++ modulename1 ++ " /= " ++ modulename2)))))

    forM_ fragments (\fragment@(FunctionFragment functionname) -> (do
        lift (insertFragment (pack functionname) modulevertex)
        respond (package,configuration,modul,fragment))))

insertFragment :: (Monad m) => Text -> VertexId -> PropertyGraphT m VertexId
insertFragment = insertVertex "FRAGMENT" "functionname"

insertExportList :: (Monad m) => VertexId -> Maybe [AST.ExportSpec] -> PropertyGraphT m ()
insertExportList _            Nothing = return ()
insertExportList modulevertex (Just exports) = do
    exportlistvertex <- newVertex empty
    newEdge empty "EXPORTLIST" modulevertex exportlistvertex
    forM_ exports (\export -> do
        exportvertex <- newVertex (singleton "exportname" (toJSON (AST.prettyPrint export)))
        newEdge empty "EXPORT" exportlistvertex exportvertex)
