module Visualization where

import System.Cmd (system)
import System.Exit (ExitCode(ExitSuccess,ExitFailure))

cppflags :: IO ()
cppflags = do
    exitcode <- system ("R CMD Rscript CPPFlags.R")
    case exitcode of
        ExitSuccess -> return ()
        ExitFailure _ -> putStrLn "Failed to run CPPFlags.R"

