module Main where

import Control.Monad (forM_,liftM)
import Control.Proxy
import System.Directory (doesDirectoryExist,getDirectoryContents)
import System.Environment (getArgs)
import System.FilePath ((</>),takeExtension)

import Language.Preprocessor.Cpphs (runCpphs,defaultCpphsOptions,CpphsOptions(defines,includes))

import Language.Haskell.Exts
    (parseFileContentsWithMode,ParseMode(parseFilename),defaultParseMode)
import Language.Haskell.Exts.Parser
    (ParseResult(ParseOk,ParseFailed))

import Data.Time (getCurrentTime,UTCTime)
import Network.URI (parseURI)

import Schema
import TransactAST
import Web.Datomic.REST
    (createDatabase,transact,
     ServerAddress,StorageName,DatabaseName,
     TransactionError)
import Web.Datomic.TransactionDSL (Transaction)
import Data.EDN (ToEDN(toEDN))

main :: IO ()
main = do
    time <- getCurrentTime
    let databasename = timeToName time
    createDatabase uri "master" databasename >>= print
    transact uri "master" databasename (toEDN masterSchema) >>= print
    runProxy $
            getRecursiveContents "./packages/"
        >-> filterD (\filename ->
                takeExtension filename == ".hs")
        >-> mapMD (\filename -> do
                content <- readFile filename
                return (filename,content))
        >-> mapMD (\(filename,content) -> do
                sourceAfterCPP <- runCpphs cpphsOptions filename content
                return (filename,sourceAfterCPP))
        >-> mapD (\(filename,content) ->
                parseFileContentsWithMode (defaultParseMode {parseFilename = filename}) content)
        >-> foreverK (\() -> do
                result <- request ()
                case result of
                    ParseOk m -> respond m
                    ParseFailed _ _ -> return ())
        >-> mapD transactModule
        >-> transactionConsumer uri "master" databasename

transactionConsumer :: (Proxy p) => ServerAddress -> StorageName -> DatabaseName -> () -> Consumer p (Transaction a) IO ()
transactionConsumer uri storagename databasename = runIdentityK $ foreverK (\() -> do
    transaction <- request ()
    lift (transact uri storagename databasename (toEDN transaction))
    return ())

Just uri = parseURI "http://127.0.0.1:8834"

timeToName :: UTCTime -> String
timeToName = ("attempt"++) . replace '.' '-' . replace ':' '-' . replace ' ' '_' . show

replace :: (Eq a) => a -> a -> [a] -> [a]
replace a b = map (\ x -> if x==a then b else x)

getRecursiveContents :: (Proxy p) => FilePath -> () -> Producer p FilePath IO ()
getRecursiveContents topPath () = runIdentityP $ do
  names <- lift $ getDirectoryContents topPath
  let properNames = filter (`notElem` [".", ".."]) names
  forM_ properNames $ \name -> do
    let path = topPath </> name
    isDirectory <- lift $ doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path ()
      else respond path

cpphsOptions :: CpphsOptions
cpphsOptions = defaultCpphsOptions {defines = cpphsDefines,includes = ["./packages/base-4.6.0.1/include/"]}

cpphsDefines :: [(String,String)]
cpphsDefines = [("linux_HOST_OS","1"),
                ("i386_BUILD_ARCH","1"),
                ("__GLASGOW_HASKELL__","706"),
                ("__STDC_HOSTED__","1"),
                ("i386_HOST_ARCH","1"),
                ("linux_BUILD_OS","1")]






