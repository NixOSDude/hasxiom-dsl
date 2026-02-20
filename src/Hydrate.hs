{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.PostgreSQL.Simple
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Hasxiom.Core
import Hasxiom.Database.Postgres

-- | Tell postgresql-simple how to turn a DB row into a 'Package'
instance FromRow Package where
    fromRow = Package <$> field <*> field <*> (field >>= return . parseDeps)
      where
        -- A helper to handle the array-to-list conversion safely
        parseDeps :: Maybe [T.Text] -> [T.Text]
        parseDeps (Just xs) = xs
        parseDeps Nothing   = []

main :: IO ()
main = do
    -- 1. Establish the LAN connection to the Lakehouse
    -- Ensure your Postgres 'pg_hba.conf' allows the Ultra 7 IP!
    conn <- connectPostgreSQL "host=192.168.68.56 dbname=hasxiom_db user=nixdude"
    
    putStrLn ">> [Hasxiom] Connected to Lakehouse (192.168.68.56)"

    -- 2. Define our DSL Expression
    let queryExpr = FilterByDepth 117 (DependsOn "aeson" Identity)
    let sql = compileQuery queryExpr

    putStrLn $ ">> [Hasxiom] Executing DSL: " ++ T.unpack sql

    -- 3. Execute and Hydrate
    -- This maps the SQL results directly into our Pure Haskell Records
    results <- query_ conn (Query sql) :: IO [Package]

    putStrLn ">> [Hasxiom] Hydration Complete. Results:"
    mapM_ (print . attrName) results

    close conn
