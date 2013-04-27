module Visualization where

import System.Cmd (system)
import System.Exit (ExitCode(ExitSuccess,ExitFailure))

cppflags :: IO ()
cppflags = do
    exitcode <- system ("R CMD Rscript CPPFlags.R")
    case exitcode of
        ExitSuccess -> return ()
        ExitFailure _ -> putStrLn "Failed to run CPPFlags.R"
{-
writeOutCPPFlagData :: Stats -> IO ()
writeOutCPPFlagData stats = writeFile "CPPFlags.data" (flagmapToCsv flagmap) where
    flagmap = foldl' insertFlag empty (cppOptionsUsed stats)
    insertFlag :: Map String (Set Package) -> (Package,Module,String) -> Map String (Set Package)
    insertFlag accu (package,_,flag) = insertWith union (stripDefined (stripExclamationMark (extractFlag flag))) (singleton package) accu
    extractFlag flag = if length (words flag) > 1 then words flag !! 1 else "error finding flag"
    stripExclamationMark ('!':flag) = flag
    stripExclamationMark flag = flag
    stripDefined flag = if "defined(" `isPrefixOf` flag then takeWhile (/= ')') (drop 8 flag) else flag
    flagmapToCsv = unlines . map (\(k,v)->k++" "++show v) . toList . delete "" . M.map size
-}


