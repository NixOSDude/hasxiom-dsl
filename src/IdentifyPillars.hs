{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.PostgreSQL.Simple
import qualified Data.Text as T
import Text.Printf (printf)

main :: IO ()
main = do
    conn <- connectPostgreSQL "dbname=hasxiom_db user=nixdude"
    putStrLn ">> [Hasxiom] Identifying System Pillars (High-Depth Packages)..."

    -- We add 'attribute_name IS NOT NULL' to satisfy the 1st Tenet
    rows <- query_ conn "SELECT attribute_name, depth FROM nix_packages \
                        \WHERE depth > 0 \
                        \AND attribute_name IS NOT NULL \
                        \ORDER BY depth DESC LIMIT 10" :: IO [(T.Text, Int)]

    if null rows 
        then putStrLn "!! Warning: No pillars found. Is the Lakehouse hydrated?"
        else do
            putStrLn "----------------------------------------------------"
            putStrLn " RANK | DEPTH | PACKAGE ATTRIBUTE NAME"
            putStrLn "----------------------------------------------------"
            let printRow (rank, (name, d)) = printf " #%2d  | %5d | %s\n" (rank :: Int) d (T.unpack name)
            mapM_ printRow (zip [1..] rows)
    
    close conn
