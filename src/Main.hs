{-# LANGUAGE OverloadedStrings #-}
module Main where

import Hasxiom.Database.Loader (loadPackages)
import Hasxiom.Core
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Console.Haskeline
import Control.Monad.IO.Class (liftIO)
import Data.List (sortOn)
import Data.Ord (Down(..))
import Text.Printf (printf)
import Database.PostgreSQL.Simple (connectPostgreSQL)
import System.Environment (getEnv)

-- Tenet: Constants are immutable and pinned
versionTag :: String
versionTag = "v0.1.6-ARROW-LAKE-SIM"

-- | Audit view for high-centrality pillars
displayPillars :: [Package] -> IO ()
displayPillars pkgs = do
    putStrLn $ ">> [Hasxiom] Audit Mode: " ++ versionTag
    putStrLn ">> Identifying System Pillars (High-Depth Packages)..."
    putStrLn "--------------------------------------------------------------------------------"
    putStrLn " RANK | DEPTH | PACKAGE NAME        | ATTRIBUTE NAME"
    putStrLn "--------------------------------------------------------------------------------"
    let sorted = take 10 $ sortOn (Down . depth) pkgs
    mapM_ printPillar (zip [1..] sorted)
    where
        printPillar (i, p) = printf " #%2d  |  %4d | %-18s | %s\n" 
                               (i :: Int) (depth p) 
                               (T.unpack $ T.take 18 $ package_name p) 
                               (T.unpack $ attribute_name p)

main :: IO ()
main = do
    connStr <- getEnv "HASXIOM_DB_CONN"
    conn <- connectPostgreSQL (TE.encodeUtf8 $ T.pack connStr)

    allPkgs <- loadPackages conn
    putStrLn $ ">> Lakehouse Status: " ++ show (length allPkgs) ++ " Nodes Loaded."
     
    displayPillars allPkgs
     
    putStrLn "\n--- Hasxiom Sovereign REPL (20-Core Optimized) ---"
    putStrLn "Commands: 'search <term>', 'trace <pkg>', 'vector <pkg>', 'sim <p1> <p2>', 'status', 'exit'"
     
    runInputT defaultSettings (replLoop allPkgs)

replLoop :: [Package] -> InputT IO ()
replLoop pkgs = do
    minput <- getInputLine "hasxiom> "
    case minput of
        Nothing -> outputStrLn "Exit."
        Just "exit" -> outputStrLn "Sovereign State Suspended."
        Just "status" -> do
            outputStrLn $ ">> Total Managed Nodes: " ++ show (length pkgs)
            replLoop pkgs
        Just "" -> replLoop pkgs
        Just input -> do
            let tInput = T.pack input
            case T.words tInput of
                ["search", term] -> liftIO (handleSearch term pkgs) >> replLoop pkgs
                ["trace", target] -> liftIO (handleTrace target pkgs) >> replLoop pkgs
                ["vector", target] -> liftIO (handleVector target) >> replLoop pkgs
                ["sim", p1, p2] -> liftIO (handleSim p1 p2) >> replLoop pkgs
                _ -> liftIO (handleBlastRadius tInput pkgs) >> replLoop pkgs

handleSearch :: T.Text -> [Package] -> IO ()
handleSearch term pkgs = do
    let results = eval (Search term) pkgs
    putStrLn $ ">> Found " ++ show (length results) ++ " metadata matches."
    mapM_ printSummary (take 5 results)
    where
        printSummary p = putStrLn $ "  - [" ++ T.unpack (attribute_name p) ++ "] " 
                         ++ T.unpack (package_name p)

handleTrace :: T.Text -> [Package] -> IO ()
handleTrace target pkgs = do
    let lineage = getTransitiveDeps target pkgs []
    putStrLn $ ">> Lineage for " ++ T.unpack target ++ ":"
    if null lineage
        then putStrLn "  [Leaf Node or No Downstream Consumers]"
        else mapM_ (putStrLn . ("  -> " ++) . T.unpack) (take 10 lineage)

handleVector :: T.Text -> IO ()
handleVector target = do
    let coords = vectorize target
    printf ">> %s Vector: %s\n" (T.unpack target) (show coords)

handleSim :: T.Text -> T.Text -> IO ()
handleSim p1 p2 = do
    let v1 = vectorize p1
    let v2 = vectorize p2
    let dist = sqrt $ sum $ zipWith (\a b -> (a - b) ** 2) v1 v2
    printf ">> Similarity Distance (%s <-> %s): %.4f\n" (T.unpack p1) (T.unpack p2) dist
    if dist < 0.1 
        then putStrLn ">> STATUS: Structural Siblings (High Affinity)"
        else putStrLn ">> STATUS: Distant Nodes"

handleBlastRadius :: T.Text -> [Package] -> IO ()
handleBlastRadius target pkgs = do
    let results = eval (DependsOn target AllPackages) pkgs
    printf ">> Target: %s | Blast Radius: %d nodes\n" (T.unpack target) (length results)
