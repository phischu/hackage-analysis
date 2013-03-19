{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Main where

import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Classes

import qualified System.Directory as IO
import qualified Data.Map as M
import Distribution.Text

import Distribution.Hackage.DB (readHackage')


newtype Package = Package (Name,Version) deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
type Name = String
type Version = String

newtype ExtractedPackage = ExtractedPackage Package deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype PackageArchive = PackageArchive Package deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype PackageList = PackageList [Package] deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype GetPackageList = GetPackageList () deriving (Show,Typeable,Eq,Hashable,Binary,NFData)

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

extractedDirectory :: FilePath
extractedDirectory = "Packages/"

archiveDirectory :: Package -> FilePath
archiveDirectory (Package (name,version)) = "Archives/"++name++"-"++version++".tar.gz"

packageUrl :: Package -> String
packageUrl (Package (name,version)) = concat ["hackage.haskell.org/packages/archive/",name,"/",version,"/",name,"-",version,".tar.gz"]

main :: IO ()
main = shakeArgs shakeOptions {shakeThreads = 10} $ do

    action (do
        PackageList packages <- apply1 (GetPackageList ())
        apply (map ExtractedPackage packages) :: Action [()])

    rule (\(GetPackageList ()) -> Just (do
        need ["00-index.tar"]
        hackage <- liftIO (readHackage' "00-index.tar")
        let packages = [Package (name,show (disp version))| name <- M.keys hackage, version <- M.keys (hackage M.! name)]
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

    return ()


every :: Int -> [a] -> [a]
every nth [] = []
every nth xs = head xs : every nth (drop nth xs)











