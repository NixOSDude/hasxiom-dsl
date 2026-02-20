{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Database.PostgreSQL.Simple
import qualified Data.Map.Strict as Map
import System.CPUTime (getCPUTime)
import Text.Printf (printf)
import Data.List (unfoldr, foldl')
import Control.Monad (forM_)

type NodeId = Int
type Graph = Map.Map NodeId [NodeId]
type Memo = Map.Map NodeId Int

calculateDepth :: Graph -> Memo -> NodeId -> (Int, Memo)
calculateDepth graph memo node =
    case Map.lookup node memo of
        Just d -> (d, memo)
        Nothing ->
            let children = Map.findWithDefault [] node graph
            in if null children
               then (1, Map.insert node 1 memo)
               else
                 let (depths, updatedMemo) = foldl' 
                        (\(ds, m) child -> 
                           let (d, m') = calculateDepth graph m child 
                           in (d:ds, m')) 
                        ([], memo) children
                     !maxD = 1 + (if null depths then 0 else maximum depths)
                 in (maxD, Map.insert node maxD updatedMemo)

chunks :: Int -> [a] -> [[a]]
chunks n = unfoldr (\xs -> if null xs then Nothing else Just (splitAt n xs))

main :: IO ()
main = do
    start <- getCPUTime
    conn <- connectPostgreSQL "dbname=hasxiom_db user=nixdude"
    putStrLn ">> [1/4] Loading edges..."
    edges <- query_ conn "SELECT parent_id, child_id FROM package_dependencies" :: IO [(NodeId, NodeId)]
    let !graph = foldl' (\acc (p, c) -> Map.insertWith (++) p [c] acc) Map.empty edges
    putStrLn ">> [2/4] Loading nodes..."
    nodes <- query_ conn "SELECT id FROM nix_packages" :: IO [Only NodeId]
    putStrLn ">> [3/4] Computing depths (Strict)..."
    let !finalMemo = foldl' (\m (Only nid) -> snd (calculateDepth graph m nid)) Map.empty nodes
    let !updateData = [(d, nid) | (nid, d) <- Map.toList finalMemo]
    putStrLn ">> [4/4] Batch Persisting..."
    let batches = chunks 500 updateData
    forM_ (zip [1..] batches) $ \(i, batch) -> do
        executeMany conn "UPDATE nix_packages SET depth = ? WHERE id = ?" batch
        printf "\r>> Progress: %d/%d" (i :: Int) (length batches)
    end <- getCPUTime
    let diff = fromIntegral (end - start) / 10^(12 :: Integer) :: Double
    printf "\n>> Success! Analysis complete in %0.4f seconds.\n" diff
    close conn
