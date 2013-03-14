{-# LANGUAGE OverloadedStrings #-}
module Schema where

import Web.Datomic.SchemaDSL
import Web.Datomic.TransactionDSL

masterSchema :: Transaction ()
masterSchema = do
    nameSchema
    return ()

nameSchema :: Transaction TempId
nameSchema = do
    enum (key "db.part" "user")
         (key "name" "case")
         [key "name.case" "ident",key "name.case" "symbol"]
    schema (key "name" "string") TypeString One


