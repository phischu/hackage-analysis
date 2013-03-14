{-# LANGUAGE OverloadedStrings #-}
module TransactAST where

import Data.Text

import Language.Haskell.Exts

import Web.Datomic.TransactionDSL


transactModule :: Module -> Transaction ()
transactModule (Module _ _ _ _ _ _ decls) = mapM_ transactDecl decls

transactDecl :: Decl -> Transaction ()
transactDecl (TypeSig _ names _) = mapM_ transactName names
transactDecl _ = return ()

transactName :: Name -> Transaction ()
transactName (Ident string) = do
    tempid <- newTempId (key "db.part" "user")
    add tempid (key "name" "case") (key "name.case" "ident")
    add tempid (key "name" "string") (pack string)
transactNAme (Symbol string) = do
    tempid <- newTempId (key "db.part" "user")
    add tempid (key "name" "case") (key "name.case" "symbol")
    add tempid (key "name" "string") (pack string)





