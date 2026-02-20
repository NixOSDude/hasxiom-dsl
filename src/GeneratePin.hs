{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.PostgreSQL.Simple
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
    conn <- connectPostgreSQL "dbname=hasxiom_db user=nixdude"
    putStrLn ">> [Hasxiom] Generating High-Depth Surgical Pins..."

    -- We include depth in the SELECT so we can ORDER BY it
    rows <- query_ conn "SELECT attribute_name, depth FROM nix_packages \
                       \WHERE depth >= 110 \
                       \AND attribute_name IS NOT NULL \
                       \GROUP BY attribute_name, depth \
                       \ORDER BY depth DESC;" :: IO [(T.Text, Int)]

    let header = "# High-Depth Pillars (Levels 110-119)\n{ pkgs ? import <nixpkgs> {} }:\n[\n"
    -- We map only the first element of the tuple (the name) to the Nix list
    let body = T.unlines $ map (\(name, _) -> "  \"" <> name <> "\"") rows
    let footer = "]"
    
    TIO.writeFile "pinned-packages.nix" (header <> body <> footer)
    putStrLn $ "Success: " ++ show (length rows) ++ " surgical pins generated."
    close conn
