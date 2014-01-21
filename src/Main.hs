module Main where

import Repository (loadRepository)


main :: IO ()
main = do
    loadRepository
    print "hallo"
