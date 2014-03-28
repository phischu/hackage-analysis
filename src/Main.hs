module Main where

import Repository (loadRepository)
import ParsePackage (parseAndSaveAllPackages)
import NameResolution (resolveAndSaveAllPackageNames)
import Fragmentation (splitAndSaveAllDeclarations)
import Insertion (insertAllPackages)

import Data.Map.Strict (Map,toList)
import Data.Text (Text,unpack)
import Data.List (intercalate)

newtype ShowProperties = ShowProperties {unShowProperties :: Map Text Text}
newtype ShowLabel = ShowLabel {unShowLabel :: Text}

instance Show ShowProperties where
    show = intercalate "\n" . (map (\(key,value) -> unpack key ++ " = " ++ unpack value)) . toList . unShowProperties

instance Show ShowLabel where
    show = unpack . unShowLabel

main :: IO ()
main = do
    sourcerepository <- loadRepository
    parsedrepository <- parseAndSaveAllPackages sourcerepository
    resolveAndSaveAllPackageNames parsedrepository
    splitAndSaveAllDeclarations parsedrepository
    propertygraph <- insertAllPackages parsedrepository
    putStrLn "done"

