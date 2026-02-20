{-# LANGUAGE OverloadedStrings #-}
module Main where

import Database.PostgreSQL.Simple
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Hasxiom.Core
import Hasxiom.Database.Postgres

main :: IO ()
main = do
    -- Establish connection to the nixlakehouse (laptop)
    conn <- connectPostgreSQL "host=192.168.68.56 dbname=hasxiom_db user=nixdude"
    putStrLn ">> [Hasxiom] Connected to Lakehouse at 192.168.68.56"

    let queryExpr = FilterByDepth 117 (DependsOn "aeson" Identity)
    let sqlText = compileQuery queryExpr

    putStrLn $ ">> [Hasxiom] Executing DSL: " ++ T.unpack sqlText
    
    -- fromString is polymorphic; because query_ expects a Query, 
    -- it will convert the String (from Text) into a Query type automatically.
    let sqlQuery = fromString (T.unpack sqlText)

    results <- query_ conn sqlQuery :: IO [Package]

    putStrLn $ ">> [Hasxiom] Hydration Complete. Found " ++ show (length results) ++ " packages."
    mapM_ (TIO.putStrLn . attrName) results

    close conn
