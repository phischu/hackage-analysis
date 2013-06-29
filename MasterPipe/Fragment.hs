{-# LANGUAGE OverloadedStrings #-}
module MasterPipe.Fragment where

import MasterPipe.Types
import MasterPipe.Database

import Database.PropertyGraph (PropertyGraphT,VertexId,newVertex,newEdge)

import Control.Proxy (Proxy,Pipe,request,respond,liftP,lift)
import Control.Proxy.Safe (ExceptionP,SafeIO,tryIO)
import Control.Proxy.Trans.State (StateP,get,put)
import Control.Monad (forever,forM_,when)
import Control.Monad.Morph (hoist)

import Data.Text (Text,pack)
import Data.Map (empty,singleton,fromList)
import Data.Maybe (mapMaybe)

import Data.Aeson.Generic (toJSON)

import qualified Language.Haskell.Exts as AST (
    Module(Module),ModuleName(ModuleName),
    ExportSpec,
    Match(Match),Name(Ident,Symbol),
    Decl(TypeDecl,TypeFamDecl,DataDecl,GDataDecl,ClassDecl,FunBind,PatBind,ForImp),
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

        fragments = mapMaybe extractFragment declarations

    lift (insertExportList modulevertex maybeExports)

    when (modulename1 /= modulename2)
        (hoist lift ((tryIO (print ("Modulenames do not match: " ++ modulename1 ++ " /= " ++ modulename2)))))

    forM_ fragments (\fragment -> (do
        fragmentvertex <- lift (insertFragment fragment modulevertex)
        liftP (put fragmentvertex)
        respond (package,configuration,modul,fragment))))

extractFragment :: AST.Decl -> Maybe Fragment
extractFragment (AST.TypeDecl _ name _ _) = Just (TypeFragment (AST.prettyPrint name))
extractFragment (AST.TypeFamDecl _ name _ _) = Just (TypeFragment (AST.prettyPrint name))
extractFragment (AST.DataDecl _ _ _ name _ _ _) = Just (TypeFragment (AST.prettyPrint name))
extractFragment (AST.GDataDecl _ _ _ name _ _ _ _) = Just (TypeFragment (AST.prettyPrint name))
extractFragment (AST.ClassDecl _ _ name _ _ _) = Just (ClassFragment (AST.prettyPrint name))
extractFragment (AST.FunBind ((AST.Match _ name _ _ _ _):_)) = Just (ValueFragment (AST.prettyPrint name))
extractFragment (AST.PatBind _ _ _ _ _) = Nothing -- TODO: find all names bound by a pattern
extractFragment (AST.ForImp _ _ _ _ name _) = Just (ValueFragment (AST.prettyPrint name))
extractFragment _ = Nothing

insertFragment :: (Monad m) => Fragment -> VertexId -> PropertyGraphT m VertexId
insertFragment (ValueFragment fragmentname) = insertFragmentWithGenreAndName "Value" (pack fragmentname)
insertFragment (TypeFragment fragmentname)  = insertFragmentWithGenreAndName "Type" (pack fragmentname)
insertFragment (ClassFragment fragmentname) = insertFragmentWithGenreAndName "Class" (pack fragmentname)

insertFragmentWithGenreAndName :: (Monad m) => Text -> Text -> VertexId -> PropertyGraphT m VertexId
insertFragmentWithGenreAndName genre fragmentname modulevertex = do
    fragmentvertex <- newVertex (fromList [
        ("genre",toJSON genre),
        ("fragmentname",toJSON fragmentname)])
    newEdge empty "FRAGMENT" modulevertex fragmentvertex
    return fragmentvertex

insertExportList :: (Monad m) => VertexId -> Maybe [AST.ExportSpec] -> PropertyGraphT m ()
insertExportList _            Nothing = return ()
insertExportList modulevertex (Just exports) = do
    exportlistvertex <- newVertex empty
    newEdge empty "EXPORTLIST" modulevertex exportlistvertex
    forM_ exports (\export -> do
        exportvertex <- newVertex (singleton "exportname" (toJSON (AST.prettyPrint export)))
        newEdge empty "EXPORT" exportlistvertex exportvertex)
