{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Main where

import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Classes

import qualified System.Directory as IO

import Paths

newtype Package = Package (Name,Version) deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
type Name = String
type Version = String

newtype ExtractedPackage = ExtractedPackage Package deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype PackageArchive = PackageArchive Package deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype PackageList = PackageList [Package] deriving (Show,Typeable,Eq,Hashable,Binary,NFData)

instance Rule ExtractedPackage () where
    storedValue (ExtractedPackage (Package (name,version))) = do
        exists <- IO.doesDirectoryExist (extractedDirectory++name++"-"++version)
        if exists then return (Just ()) else return Nothing

instance Rule PackageArchive () where
    storedValue (PackageArchive package) = do
        exists <- IO.doesFileExist (archiveDirectory package)
        if exists then return (Just ()) else return Nothing

instance Rule () PackageList where
    storedValue () = return Nothing

extractedDirectory :: FilePath
extractedDirectory = "Packages/"

archiveDirectory :: Package -> FilePath
archiveDirectory (Package (name,version)) = "Archives/"++name++"-"++version++".tar.gz"

packageUrl :: Package -> String
packageUrl (Package (name,version)) = concat ["hackage.haskell.org/packages/archive/",name,"/",version,"/",name,"-",version,".tar.gz"]

main :: IO ()
main = shake shakeOptions {shakeThreads = 4} $ do

    action (do
        PackageList packages <- apply1 ()
        apply (map ExtractedPackage packages) :: Action [()])

    rule (\() -> Just (return (PackageList [Package ("aeson-lens","0.1.0.2")])))

    rule (\(ExtractedPackage package) -> Just $ do
        liftIO (IO.createDirectoryIfMissing True extractedDirectory)
        () <- apply1 (PackageArchive package)
        system' "tar" ["xzf",archiveDirectory package,"-C",extractedDirectory])

    rule (\(PackageArchive package) -> Just $ do
        liftIO (IO.createDirectoryIfMissing True (takeDirectory (archiveDirectory package)))
        system' "wget" ["-O",archiveDirectory package,packageUrl package])

    return ()














