{-# LANGUAGE OverloadedStrings #-}
module Main where

import Database.PostgreSQL.Simple
import qualified Data.Text as T
import Hasxiom.Core
import Hasxiom.Database.Postgres

main :: IO ()
main = do
    -- Establish connection to the nixlakehouse (laptop)
    conn <- connectPostgreSQL "host=192.168.68.56 dbname=hasxiom_db user=nixdude"
    putStrLn ">> [Hasxiom] Connected to Lakehouse at 192.168.68.56"

    let queryExpr = FilterByDepth 117 (DependsOn "aeson" Identity)
    let sql = compileQuery queryExpr

    putStrLn $ ">> [Hasxiom] Executing DSL: " ++ T.unpack sql
    
    -- Hydrate real records from the Postgres Lakehouse
    results <- query_ conn (Query sql) :: IO [Package]

    putStrLn $ ">> [Hasxiom] Hydration Complete. Found " ++ show (length results) ++ " packages."
    mapM_ (print . attrName) results

    close conn
