module NameResolution where

import Common (Repository,ParsedRepository)

data NameError = NameError

resolvePackageNames :: FilePath -> IO (Repository NameError)
resolvePackageNames = undefined

resolveAndSaveAllPackageNames :: ParsedRepository -> IO (Repository NameError)
resolveAndSaveAllPackageNames = undefined
