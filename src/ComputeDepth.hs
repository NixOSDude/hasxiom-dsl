{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.PostgreSQL.Simple
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.Environment (getEnv)

main :: IO ()
main = do
    connStr <- getEnv "HASXIOM_DB_CONN"
    conn <- connectPostgreSQL (T.encodeUtf8 $ T.pack connStr)
    
    putStrLn ">> [Hasxiom] Computing Depth via Level-Set Iteration (Safe Mode)..."
    
    -- Tenet: Always start from a clean, known state
    _ <- execute_ conn "UPDATE nix_packages SET depth = 0"
    
    -- Step 1: Initialize Leaf Nodes (Depth 1)
    -- A leaf has no children (no one depends on it in this context)
    putStrLn ">> Initializing Leaf Nodes (Level 1)..."
    _ <- execute_ conn 
        "UPDATE nix_packages SET depth = 1 \
        \WHERE id NOT IN (SELECT parent_id FROM package_dependencies)"

    -- Step 2: Iteratively find the next level
    -- A package's depth = 1 + (max depth of its children)
    let loop iteration = do
            putStrLn $ ">> Calculating Level: " ++ show iteration
            updatedCount <- execute_ conn 
                "UPDATE nix_packages p \
                \SET depth = sub.new_depth \
                \FROM ( \
                \  SELECT p2.id, MAX(c.depth) + 1 as new_depth \
                \  FROM nix_packages p2 \
                \  JOIN package_dependencies pd ON p2.id = pd.parent_id \
                \  JOIN nix_packages c ON pd.child_id = c.id \
                \  WHERE p2.depth = 0 \
                \  AND c.depth > 0 \
                \  GROUP BY p2.id \
                \) sub \
                \WHERE p.id = sub.id"
            
            if updatedCount > 0 
                then loop (iteration + 1)
                else return iteration

    finalDepth <- loop 2
    putStrLn $ ">> [Hasxiom] Success! Max Graph Depth: " ++ show (finalDepth - 1)
    close conn
