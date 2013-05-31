{-# LANGUAGE NoMonomorphismRestriction,OverloadedStrings #-}
module Main where

import Database.PipesGremlin

import Control.Proxy
import Control.Proxy.Safe
import Control.Proxy.Trans.Writer

import Control.Monad
import Data.List
import qualified Data.ByteString.Lazy as BS

import Data.Aeson
import Text.Blaze.Svg.Renderer.Utf8

import Database.Neo4j

import Diagrams.Prelude hiding (runQuery,Proxy)
import Diagrams.Backend.SVG
import Diagrams.Backend.SVG.CmdLine




main :: IO ()
main = do
    results <- runQuery changeQuery
    forM_ results (\(packagenamevalue,changes) -> do
    	case fromJSON packagenamevalue of
    		Success packagename -> do
    			print (packagename :: String,numberOfChanges changes)
    			when (length changes > 1) (do
    			    let dia = renderDia SVG (SVGOptions (Width 400)) (changeDiagram (numberOfChanges changes))
    			    BS.writeFile ("ChangeDiagrams/"++packagename++".svg") (renderSvg dia))
    		Error e -> print e)


numberOfChanges :: [(Versionname,[(Modulename,Functionname)])] -> [(Double,Double,Double)]
numberOfChanges [] = []
numberOfChanges [_] = []
numberOfChanges (version1:version2:rest) = (kepts,addeds,removeds):numberOfChanges (version2:rest) where
    (_,names1) = version1
    (_,names2) = version2
    kepts = fromIntegral (length (intersect names1 names2))
    addeds = fromIntegral (length (names2 \\ names1))
    removeds = fromIntegral (length (names1 \\ names2))


runQuery :: (Show a) => ProduceT (ExceptionP ProxyFast) SafeIO a -> IO [a]
runQuery test = do
    result <- trySafeIO $ runProxy $ runEitherK $ runWriterK $ const (liftP (runRespondT test)) >-> toListD
    either (print>=>const (return [])) (return . snd) result

type Packagename = Value
type Versionname = Value
type Modulename = Value
type Functionname = Value

changeQuery :: ProduceT (ExceptionP ProxyFast) SafeIO (Packagename,[(Versionname,[(Modulename,Functionname)])])
changeQuery = do
    package <- nodeById 1 >>= nextLabeled "PACKAGE"
    packagename <- property "packagename" package
    versions <- firstVersion package >>= versionChain
    versionfragments <- forM versions (\version -> do
        versionname <- property "versionname" version
        fragments <- gather (do
            variant <- return version >>= nextLabeled "VARIANT"
            modul <- return variant >>= nextLabeled "MODULE"
            fragment <- return modul >>= nextLabeled "FRAGMENT"
            modulename <- property "modulename" modul
            functionname <- property "functionname" fragment
            return (modulename,functionname))
        return (versionname,fragments))
    return (packagename,versionfragments)

firstVersion :: (Proxy p) => Node -> ProduceT (ExceptionP p) SafeIO Node
firstVersion package = do
    version <- nextLabeled "VERSION" package
    ensurenot (inEdgeLabeled "NEXTVERSION" version)
    return version

versionChain :: (Proxy p) => Node -> ProduceT (ExceptionP p) SafeIO [Node]
versionChain version1 = do
    possibleVersion2 <- gather (nextLabeled "NEXTVERSION" version1)
    case possibleVersion2 of
        [] -> return []
        [version2] -> versionChain version2 >>= return . (version2:)
        _ -> error "two next versions"

changeDiagram :: [(Double,Double,Double)] -> Diagram SVG R2
changeDiagram values = hcat (map (\(x,y,z) -> bar x y z) values) # pad 1.2

addeds = [2,0,2,8,0,0]
kepts  = [8,13,4,9,20,20]
removeds = [1,1,2,0,0,5]

bar kept added removed = removedRect `below` (addedRect `above` keptRect `above` strutX 1) where
    removedRect = minibar red removed
    keptRect    = minibar blue kept
    addedRect   = minibar green added

minibar color height
    | height > 0 = unitSquare # scaleY height # fc color
    | otherwise  = mempty

x `below` y = beside (r2 (0,-1)) y x

x `above` y = beside (r2 (0,1)) y x

