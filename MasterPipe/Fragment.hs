{-# LANGUAGE OverloadedStrings #-}
module MasterPipe.Fragment where

import MasterPipe.Types
import MasterPipe.Database

import Database.PropertyGraph (PropertyGraphT,VertexId,newVertex,newEdge)

import Control.Proxy (Proxy,Pipe,request,respond,liftP,lift)
import Control.Proxy.Safe (ExceptionP,SafeIO,tryIO)
import Control.Proxy.Trans.State (StateP,get,put)
import Control.Monad (forever,forM_,when,(>=>))
import Control.Monad.Morph (hoist)

import Data.Text (Text,pack)
import Data.Map (empty,singleton,fromList)
import Data.Maybe (mapMaybe)

import Data.Aeson.Generic (toJSON)
import Data.Generics.Uniplate.Data (universe,transform)

import qualified Language.Haskell.Exts as AST (
    Module(Module),ModuleName(ModuleName),
    ExportSpec,
    Match(Match),Name(Ident,Symbol),
    Decl(TypeDecl,TypeFamDecl,DataDecl,GDataDecl,ClassDecl,FunBind,PatBind,ForImp,TypeSig),
    QualConDecl(QualConDecl),ConDecl(ConDecl,InfixConDecl,RecDecl),
    GadtDecl(GadtDecl),
    ClassDecl(ClsDecl),
    Pat(PVar,PAsPat,PViewPat),
    ExportSpec(EVar,EAbs,EThingAll,EThingWith,EModuleContents),
    QName(Qual,UnQual,Special),CName,
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
extractFragment (AST.TypeDecl _ name _ _) = Just (TypeFragment (AST.prettyPrint name) [])
extractFragment (AST.TypeFamDecl _ name _ _) = Just (TypeFragment (AST.prettyPrint name) [])
extractFragment (AST.DataDecl _ _ _ name _ constructors _) =
    Just (TypeFragment
        (AST.prettyPrint name)
        (concatMap extractDataDefinitions constructors))
extractFragment (AST.GDataDecl _ _ _ name _ _ gadtdecls _) =
    Just (TypeFragment
        (AST.prettyPrint name)
        (concatMap extractGDataDefinitions gadtdecls))
extractFragment (AST.ClassDecl _ _ name _ _ classdecls) =
    Just (ClassFragment
        (AST.prettyPrint name)
        (concatMap extractClassDefinitions classdecls))
extractFragment (AST.FunBind ((AST.Match _ name _ _ _ _):_)) = Just (ValueFragment (AST.prettyPrint name))
extractFragment (AST.PatBind _ pattern _ _ _) = Just (PatternFragment (extractPatternDefinitions pattern))
extractFragment (AST.ForImp _ _ _ _ name _) = Just (ValueFragment (AST.prettyPrint name))
extractFragment _ = Nothing

extractDataDefinitions :: AST.QualConDecl -> [DefinitionName]
extractDataDefinitions (AST.QualConDecl _ _ _ (AST.ConDecl name _)) = [AST.prettyPrint name]
extractDataDefinitions (AST.QualConDecl _ _ _ (AST.InfixConDecl _ name _)) = [AST.prettyPrint name]
extractDataDefinitions (AST.QualConDecl _ _ _ (AST.RecDecl name fields)) = AST.prettyPrint name : do
    (fieldnames,_) <- fields
    map AST.prettyPrint fieldnames

extractGDataDefinitions :: AST.GadtDecl -> [DefinitionName]
extractGDataDefinitions (AST.GadtDecl _ name _) = [AST.prettyPrint name]

extractClassDefinitions :: AST.ClassDecl -> [DefinitionName]
extractClassDefinitions (AST.ClsDecl (AST.TypeSig _ names _)) = map AST.prettyPrint names
extractClassDefinitions _ = []

extractPatternDefinitions :: AST.Pat -> [DefinitionName]
extractPatternDefinitions pattern = do
    let removeViewPattern pattern = case pattern of
            (AST.PViewPat _ childpattern) -> childpattern
            actualpattern -> actualpattern
    singlepattern <- universe (transform removeViewPattern pattern)
    case singlepattern of
        AST.PVar name -> [AST.prettyPrint name]
        AST.PAsPat name _ -> [AST.prettyPrint name]
        _ -> []

insertFragment :: (Monad m) => Fragment -> VertexId -> PropertyGraphT m VertexId
insertFragment (ValueFragment fragmentname) =
    insertFragmentWithGenreAndName "Value" (pack fragmentname)
insertFragment (TypeFragment fragmentname definitions)  =
    insertFragmentWithGenreAndName "Type" (pack fragmentname) >=>
    insertDefinitions definitions
insertFragment (ClassFragment fragmentname definitions) =
    insertFragmentWithGenreAndName "Class" (pack fragmentname) >=>
    insertDefinitions definitions
insertFragment (PatternFragment []) =
    insertVertex "FRAGMENTEXCEPTION" "exception" "empty pattern" ["Fragmentexception"]
insertFragment (PatternFragment (definition:definitions)) =
    insertVertex "FRAGMENT" "fragmentname" (pack definition) ["Fragment","Patternfragment"] >=>
    insertDefinitions (definition:definitions)

insertDefinitions :: (Monad m) => [DefinitionName] -> VertexId -> PropertyGraphT m VertexId
insertDefinitions definitions fragmentvertex = do
    forM_ definitions (\definition -> insertVertex
        "DEFINES"
        "definitionname"
        (pack definition)
        ["Definition"]
        fragmentvertex)
    return fragmentvertex

insertFragmentWithGenreAndName :: (Monad m) => Text -> Text -> VertexId -> PropertyGraphT m VertexId
insertFragmentWithGenreAndName genre fragmentname modulevertex = do
    fragmentvertex <- newVertex
        (fromList [
            ("genre",toJSON genre),
            ("fragmentname",toJSON fragmentname)])
        ["Fragment"]
    newEdge empty "FRAGMENT" modulevertex fragmentvertex
    return fragmentvertex

insertExportList :: (Monad m) => VertexId -> Maybe [AST.ExportSpec] -> PropertyGraphT m ()
insertExportList _            Nothing = return ()
insertExportList modulevertex (Just exports) = do
    exportlistvertex <- newVertex empty ["Exportlist"]
    newEdge empty "EXPORTLIST" modulevertex exportlistvertex
    forM_ exports (\export -> do
        case export of
            AST.EVar qname ->
                insertExportSpec "Valueexport" (getQualification qname) (getName qname) exportlistvertex
            AST.EAbs qname -> 
                insertExportSpec "Abstractexport" (getQualification qname) (getName qname) exportlistvertex
            AST.EThingAll qname ->
                insertExportSpec "Thingallexport" (getQualification qname) (getName qname) exportlistvertex
            AST.EThingWith qname exportparts -> do
                exportspecvertex <- insertExportSpec
                    "Partialexport"
                    (getQualification qname)
                    (getName qname)
                    exportlistvertex
                insertExportParts exportparts exportspecvertex
                return exportspecvertex
            AST.EModuleContents (AST.ModuleName reexportname) -> do
                exportspecvertex <- newVertex
                    (singleton "reexportname" (toJSON reexportname))
                    ["Exportspec","Reexport"]
                newEdge empty "EXPORTSPEC" exportlistvertex exportspecvertex
                return exportspecvertex)

getQualification :: AST.QName -> Maybe String
getQualification (AST.Qual qualificaton _) = Just (AST.prettyPrint qualificaton)
getQualification _                     = Nothing

getName :: AST.QName -> String
getName (AST.Qual _ name)     = AST.prettyPrint name
getName (AST.UnQual name)     = AST.prettyPrint name
getName (AST.Special special) = AST.prettyPrint special

insertExportSpec :: (Monad m) => Text -> Maybe String -> String -> VertexId -> PropertyGraphT m VertexId
insertExportSpec label maybeexportqualification exportname exportlistvertex = do
    exportspecvertex <- newVertex
        (singleton "exportname" (toJSON exportname))
        ["Exportspec","Directexport",label]
    newEdge empty "EXPORTSPEC" exportlistvertex exportspecvertex
    case maybeexportqualification of
        Nothing -> return ()
        Just exportqualification -> do
            exportqualificationvertex <- newVertex
                (singleton "exportqualification" (toJSON exportqualification))
                ["Exportqualification"]
            newEdge empty "EXPORTQUALIFICATION" exportspecvertex exportqualificationvertex
    return exportspecvertex

insertExportParts :: (Monad m) => [AST.CName] -> VertexId -> PropertyGraphT m ()
insertExportParts names exportspecvertex = do
    forM_ names (\name -> do
        exportpartvetex <- newVertex
            (singleton "exportpartname" (toJSON (AST.prettyPrint name)))
            ["Exportpart"]
        newEdge empty "EXPORTPART" exportspecvertex exportpartvetex)
