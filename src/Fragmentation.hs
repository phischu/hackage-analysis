module Fragmentation where

import Common (ParsedRepository,traverseRepository)

splitAndSaveAllDeclarations :: ParsedRepository -> IO ()
splitAndSaveAllDeclarations parsedrepository = do
    flip traverseRepository parsedrepository (\_ _ packagepath -> do
        return ())
    return ()



