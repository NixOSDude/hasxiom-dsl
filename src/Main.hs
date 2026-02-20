{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.PostgreSQL.Simple
import qualified Data.Text as T
import qualified Data.Set as Set
import System.Environment (getArgs)
import Control.Monad (foldM)

-- Directory: ~/projects/hasxiom-dsl
-- Tenet: Explicit Type Signatures & O(n) Recursion
-- Goal: Recursive Graphviz generation from local Ultra 7 Lakehouse

main :: IO ()
main = do
    args <- getArgs
    conn <- connectPostgreSQL "dbname=hasxiom_db user=nixdude"
    case args of
        ["dot", target] -> do
            putStrLn "digraph G {"
            putStrLn "  rankdir=LR;"
            putStrLn "  node [shape=box, style=filled, color=lightblue, fontname=\"Courier\"];"
            
            -- Explicitly type the return to (Int, T.Text) to fix GHC-39999
            pkgs <- query conn "SELECT id, package_name FROM nix_packages WHERE package_name LIKE ? LIMIT 1" (Only $ "%" <> target <> "%") :: IO [(Int, T.Text)]
            
            case pkgs of
                [(pid, _)] -> do
                    _ <- traverseGraph conn Set.empty pid
                    return ()
                [] -> putStrLn "// No package found matching that name."
                _  -> putStrLn "// Ambiguous result."
            putStrLn "}"
        _ -> putStrLn "// Usage: cabal run hasxiom-mcp -- dot <package_substring>"
    close conn

-- Recursive function using foldM to pass the Visited Set state
traverseGraph :: Connection -> Set.Set Int -> Int -> IO (Set.Set Int)
traverseGraph conn visited currentId = 
    if Set.member currentId visited
    then return visited
    else do
        -- Explicitly type the result of the edge query
        edges <- query conn "SELECT p.package_name, c.package_name, c.id \
                            \FROM package_dependencies pd \
                            \JOIN nix_packages p ON pd.parent_id = p.id \
                            \JOIN nix_packages c ON pd.child_id = c.id \
                            \WHERE p.id = ?" (Only currentId) :: IO [(T.Text, T.Text, Int)]
        
        let newVisited = Set.insert currentId visited
        
        -- Recursively visit children
        foldM (\vAcc (pName, cName, cid) -> do
            putStrLn $ "  \"" <> T.unpack pName <> "\" -> \"" <> T.unpack cName <> "\";"
            traverseGraph conn vAcc cid
            ) newVisited edges
