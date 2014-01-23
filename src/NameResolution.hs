module NameResolution where

import Common (Repository,ParsedRepository,traverseRepository)

data NameErrors = NameErrors

resolvePackageNames :: FilePath -> IO (Repository NameErrors)
resolvePackageNames = undefined

resolveAndSaveAllPackageNames :: ParsedRepository -> IO (Repository NameErrors)
resolveAndSaveAllPackageNames parsedrepository = do
    putStrLn "Resolving Names ..."
    flip traverseRepository parsedrepository (\_ _ packagepath -> do
        resolveNames packagepath
        return undefined)

resolveNames :: FilePath -> IO NameErrors
resolveNames packagepath = do
    packageinformation <- undefined
    return undefined

