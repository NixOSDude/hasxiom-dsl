{-# LANGUAGE OverloadedStrings #-}
module Main where

import Hasxiom.Database.Loader (loadPackages)
import Hasxiom.Core
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Console.Haskeline
import Data.List (sortOn)
import Data.Ord (Down(..))
import Text.Printf (printf)
import Database.PostgreSQL.Simple (connectPostgreSQL)
import System.Environment (getEnv)

-- Tenet: Constants are immutable and pinned
versionTag :: String
versionTag = "v0.1.4-SOVEREIGN-TRACE"

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
     
    putStrLn "\n--- Hasxiom Sovereign REPL ---"
    putStrLn "Commands: 'search <term>', 'trace <pkg>', 'status', 'exit'"
     
    runInputT defaultSettings (replLoop allPkgs)

replLoop :: [Package] -> InputT IO ()
replLoop pkgs = do
    minput <- getInputLine "hasxiom> "
    case minput of
        Nothing -> outputStrLn "Exit."
        Just "exit" -> outputStrLn "Sovereign State Suspended."
        Just "status" -> do
            outputStrLn $ ">> Total Managed Nodes: " ++ show (length pkgs)
            outputStrLn $ ">> Named Entities: " ++ show (length $ filter (\p -> description p /= "") pkgs)
            replLoop pkgs
        Just "" -> replLoop pkgs
        Just input -> do
            let tInput = T.pack input
            if "search " `T.isPrefixOf` tInput
                then handleSearch (T.drop 7 tInput) pkgs
                else if "trace " `T.isPrefixOf` tInput
                    then handleTrace (T.drop 6 tInput) pkgs
                    else handleBlastRadius tInput pkgs
            replLoop pkgs

handleSearch :: T.Text -> [Package] -> InputT IO ()
handleSearch term pkgs = do
    let results = eval (Search term) pkgs
    outputStrLn $ ">> Found " ++ show (length results) ++ " metadata matches."
    mapM_ printSummary (take 5 results)
    where
        printSummary p = outputStrLn $ "  - [" ++ T.unpack (attribute_name p) ++ "] " 
                         ++ T.unpack (package_name p) ++ ": " 
                         ++ T.unpack (T.take 60 (description p)) ++ "..."

handleTrace :: T.Text -> [Package] -> InputT IO ()
handleTrace target pkgs = do
    let lineage = getTransitiveDeps target pkgs []
    outputStrLn $ ">> Lineage for " ++ T.unpack target ++ ":"
    if null lineage
        then outputStrLn "  [Leaf Node or No Downstream Consumers]"
        else mapM_ (outputStrLn . ("  -> " ++) . T.unpack) (take 15 lineage)

handleBlastRadius :: T.Text -> [Package] -> InputT IO ()
handleBlastRadius target pkgs = do
    let results = eval (DependsOn target AllPackages) pkgs
    let impact  = length results
    outputStrLn $ "Target: " ++ T.unpack target
    outputStrLn $ "Blast Radius: " ++ show impact ++ " downstream nodes."
    if impact > 0
        then mapM_ (outputStrLn . ("  - " ++) . T.unpack . attribute_name) (take 5 results)
        else outputStrLn "No downstream impact found."
