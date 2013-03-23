{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Main where

import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Classes

import qualified System.Directory as IO
import qualified Data.Map as M
import Data.Maybe
import Control.Monad
import Control.DeepSeq
import Distribution.Text
import qualified Data.Version as V

import Distribution.Hackage.DB (readHackage')
import Distribution.PackageDescription.Parse
import Distribution.PackageDescription
import Distribution.ModuleName hiding (main)
import Distribution.Verbosity
import Distribution.PackageDescription.Configuration
import Distribution.System
import Distribution.Compiler


newtype Package = Package (Name,Version) deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
type Name = String
type Version = String

newtype Module = Module (Name,Location) deriving (Show,Typeable,Eq,Hashable,Binary,NFData,Read)
type Location = FilePath

newtype ExtractedPackage = ExtractedPackage Package deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype PackageArchive = PackageArchive Package deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype GetPackageList = GetPackageList () deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype PackageList = PackageList [Package] deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype GetModulesInPackage = GetModulesInPackage Package deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype ModuleList = ModuleList [Module] deriving (Show,Typeable,Eq,Hashable,Binary,NFData,Read)
newtype CreateModuleListFile = CreateModuleListFile Package deriving (Show,Typeable,Eq,Hashable,Binary,NFData)

instance Rule ExtractedPackage () where
    storedValue (ExtractedPackage (Package (name,version))) = do
        exists <- IO.doesDirectoryExist (extractedDirectory++name++"-"++version)
        if exists then return (Just ()) else return Nothing

instance Rule PackageArchive () where
    storedValue (PackageArchive package) = do
        exists <- IO.doesFileExist (archiveDirectory package)
        if exists then return (Just ()) else return Nothing

instance Rule GetPackageList PackageList where
    storedValue (GetPackageList ()) = return Nothing

instance Rule GetModulesInPackage ModuleList where
    storedValue (GetModulesInPackage package) = return Nothing

instance Rule CreateModuleListFile () where
    storedValue (CreateModuleListFile package) = do
        exists <- IO.doesFileExist (moduleListFile package)
        if exists then return (Just ()) else return Nothing

packageIdentifier :: Package -> String
packageIdentifier (Package (name,version)) = name++"-"++version

extractedDirectory :: FilePath
extractedDirectory = "Packages/"

archiveDirectory :: Package -> FilePath
archiveDirectory package = "Archives/"++packageIdentifier package++".tar.gz"

moduleListFile :: Package -> FilePath
moduleListFile package = "ModuleLists/"++packageIdentifier package++".modulelist"

packageUrl :: Package -> String
packageUrl (Package (name,version)) = concat ["hackage.haskell.org/packages/archive/",name,"/",version,"/",name,"-",version,".tar.gz"]

main :: IO ()
main = shakeArgs shakeOptions {shakeThreads = 4} $ do

    action (do
        PackageList packages <- apply1 (GetPackageList ())
        apply (map CreateModuleListFile packages) :: Action [ModuleList])

    rule (\(GetPackageList ()) -> Just (do
        need ["00-index.tar"]
        hackage <- liftIO (readHackage' "00-index.tar")
        let packages = [Package (name,renderVersion version)| name <- M.keys hackage, version <- M.keys (hackage M.! name)]
        return (PackageList (every 1 packages))))

    rule (\(ExtractedPackage package) -> Just $ do
        liftIO (IO.createDirectoryIfMissing True extractedDirectory)
        () <- apply1 (PackageArchive package)
        system' "tar" ["xzf",archiveDirectory package,"-C",extractedDirectory])

    rule (\(PackageArchive package) -> Just $ do
        liftIO (IO.createDirectoryIfMissing True (takeDirectory (archiveDirectory package)))
        system' "wget" ["-nv","-O",archiveDirectory package,packageUrl package])

    "00-index.tar" *> (\out -> do
        system' "wget" ["-nv","hackage.haskell.org/packages/archive/00-index.tar.gz"]
        system' "gunzip" ["-f","00-index.tar.gz"])

    rule (\(GetModulesInPackage package@(Package (name,version))) -> Just $ do
        let packagedirectory = extractedDirectory++packageIdentifier package++"/"
            cabalfile = packagedirectory++name++".cabal"
        need [cabalfile]
        genericpackagedescription <- liftIO (readPackageDescription silent cabalfile)
        let eitherPackagedescription = finalizePackageDescription
                [] (const True) (Platform I386 Linux) (CompilerId GHC (V.Version [7,6,2] [])) [] genericpackagedescription
            packagedescription = either (const []) ((:[]).fst) eitherPackagedescription
            modulenames = packagedescription >>= maybeToList . library >>= libModules
            sourcedirs = packagedescription >>= maybeToList . library >>= hsSourceDirs . libBuildInfo
            potentialModules = do
                name <- modulenames
                directory <- sourcedirs
                extension <- [".hs",".lhs"]
                return (Module (show (disp name),packagedirectory++directory++"/"++toFilePath name++extension))
            valid (Module (_,path)) = doesFileExist path
        modules <- filterM valid potentialModules
        return (ModuleList modules))

    rule (\(CreateModuleListFile package) -> Just $ do
        liftIO (IO.createDirectoryIfMissing True (takeDirectory (moduleListFile package)))
        modulelist <- apply1 (GetModulesInPackage package) :: Action ModuleList
        writeFile' (moduleListFile package) (show modulelist))

    return ()


every :: Int -> [a] -> [a]
every nth [] = []
every nth xs = head xs : every nth (drop nth xs)

renderVersion :: V.Version -> String
renderVersion version@(V.Version branch tags) = addtags tags (show (disp version))

addtags :: [String] -> String -> String
addtags [] s = s
addtags (x:xs) s = addtags xs (s++"-"++x)









