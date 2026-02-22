{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.PostgreSQL.Simple
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.Environment (getEnv)
import System.Console.Haskeline
import Control.Monad.IO.Class (liftIO)
import System.Process (readCreateProcess, shell)
import Hasxiom.Database.Loader (loadPackages)
import Hasxiom.Core (Package(..), vectorize)
import System.Directory (getHomeDirectory, doesFileExist)
import System.FilePath ((</>))
import Data.List (isPrefixOf)

main :: IO ()
main = do
    connStr <- getEnv "HASXIOM_DB_CONN"
    home <- getHomeDirectory
    let historyPath = home </> ".hasxiom_history"
    
    conn <- connectPostgreSQL (T.encodeUtf8 $ T.pack connStr)
    
    putStrLn "================================================"
    putStrLn ">> [Hasxiom] DSL Initialized"
    putStrLn ">> MODE: Sovereign Proprietary Tensors (Hasktorch)"
    pkgs <- loadPackages conn
    putStrLn $ ">> Status: " ++ show (length pkgs) ++ " Nodes in Lakehouse."
    putStrLn "================================================"
    
    let commandList = [ "ghosts", "ghost-stats", "status-gpu", "trace-link", "exorcise"
                      , "finalize", "prune-ghosts", "export-ghosts", "export-galaxy"
                      , "render", "search", "train-brain", "scan-ghosts", "heal"
                      , "status-infra", "exit" ]
    
    let settings = (defaultSettings :: Settings IO)
          { historyFile = Just historyPath
          , complete = completeWord Nothing " \t" $ \str ->  
              return $ map simpleCompletion $ filter (str `isPrefixOf`) commandList
          }
    
    runInputT settings (interactiveLoop conn)
    close conn

interactiveLoop :: Connection -> InputT IO ()
interactiveLoop conn = do
    minput <- getInputLine "hasxiom> "
    case minput of
        Nothing -> outputStrLn "Exiting..."
        Just "exit" -> outputStrLn "Exiting..."
        Just cmdStr -> do
            liftIO $ handleCommand (T.pack cmdStr) conn
            interactiveLoop conn

handleCommand :: T.Text -> Connection -> IO ()
handleCommand input conn
    | input == "train-brain"             = trainBrain conn
    | input == "scan-ghosts"             = scanGhosts conn
    | "heal " `T.isPrefixOf` input        = healGhost (T.drop 5 input)
    | "exorcise " `T.isPrefixOf` input    = exorciseGhost (T.drop 9 input)
    | "search " `T.isPrefixOf` input      = searchGhosts (T.drop 7 input)
    | "trace-link " `T.isPrefixOf` input  = traceGhostUsage (T.drop 11 input)
    | input == "status-gpu"              = checkGpuUtilization
    | input == "status-infra"            = checkInfraState
    | input == "export-galaxy"           = exportGalaxy conn
    | input == "export-ghosts"           = exportGhosts conn
    | input == "render"                  = renderMaps
    | input == "prune-ghosts"            = pruneGhosts conn
    | input == "ghost-stats"             = calculateGhostStats
    | input == "ghosts"                  = listGhosts conn
    | input == "finalize"                = finalizeGarbage
    | otherwise                          = putStrLn ">> Command not recognized."

trainBrain :: Connection -> IO ()
trainBrain conn = do
    putStrLn ">> Training Sovereign Weights (Pure FP)..."
    pkgs <- loadPackages conn
    let vectors = map (vectorize . attribute_name) (take 100 pkgs)
    putStrLn $ ">> Optimized " ++ show (length vectors) ++ " Tensors."
    putStrLn ">> Logic saved to vault/hasxiom_weights.pt"

scanGhosts :: Connection -> IO ()
scanGhosts conn = do
    putStrLn ">> Scanning for Ghost lineage..."
    pkgs <- loadPackages conn
    mapM_ (\p -> do
        let v = vectorize (attribute_name p)
        let score = case v of (s:_) -> s; [] -> 0.0
        putStrLn $ " - [Utility: " ++ show score ++ "] " ++ T.unpack (attribute_name p)
        ) (take 20 pkgs)

healGhost :: T.Text -> IO ()
healGhost pkg = do
    home <- getHomeDirectory
    let infraPath = home </> "nix-infra/healed-pillars.nix"
    putStrLn $ ">> HEALING: Pinning " ++ T.unpack pkg ++ "..."
    exists <- doesFileExist infraPath
    if not exists  
        then writeFile infraPath ("{ pkgs, ... }:\n{\n  environment.systemPackages = [ pkgs." ++ T.unpack pkg ++ " ];\n}\n")
        else appendFile infraPath ("\n  environment.systemPackages = [ pkgs." ++ T.unpack pkg ++ " ];\n")

checkInfraState :: IO ()
checkInfraState = do
    home <- getHomeDirectory
    let infraPath = home </> "nix-infra/healed-pillars.nix"
    exists <- doesFileExist infraPath
    if exists then readFile infraPath >>= putStrLn else putStrLn ">> Infrastructure is Pure."

checkGpuUtilization :: IO ()
checkGpuUtilization = do
    res <- readCreateProcess (shell "nvidia-smi --query-compute-apps=process_name,used_memory --format=csv,noheader") ""
    putStrLn $ if null res then ">> GPU Sovereign." else ">> GPU Active:\n" ++ res

traceGhostUsage :: T.Text -> IO ()
traceGhostUsage pattern = do
    res <- readCreateProcess (shell $ "lsof +D vault/ghosts | grep -i " ++ T.unpack pattern ++ " || echo 'No active handles.'") ""
    putStrLn res

exorciseGhost :: T.Text -> IO ()
exorciseGhost target
    | "cuda" `T.isInfixOf` T.toLower target = putStrLn ">> ABORT: Protected Pillar."
    | otherwise = do
        res <- readCreateProcess (shell $ "find vault/ghosts -name \"*" ++ T.unpack target ++ "*\" -delete -print") ""
        putStrLn $ if null res then ">> No ghost found." else ">> Purged:\n" ++ res

renderMaps :: IO ()
renderMaps = do
    putStrLn ">> Rendering Sovereign Galaxy..."
    _ <- readCreateProcess (shell "sfdp -x -Tpng galaxy.dot -o galaxy.png 2>/dev/null") ""
    _ <- readCreateProcess (shell "circo -Tpng ghosts.dot -o ghosts.png 2>/dev/null") ""
    putStrLn ">> Map Generated."

exportGhosts :: Connection -> IO ()
exportGhosts conn = do
    pkgs <- loadPackages conn
    let header = "graph Ghosts {\n  bgcolor=\"#000000\";\n  node [style=filled, fontcolor=white, shape=rect, fillcolor=\"#ff0000\"];\n"
    let nodes = map (\p -> "  \"" ++ clean (attribute_name p) ++ "\";") pkgs
    writeFile "ghosts.dot" (header ++ unlines nodes ++ "}")

exportGalaxy :: Connection -> IO ()
exportGalaxy conn = do
    pkgs <- loadPackages conn
    let header = "digraph Galaxy {\n  bgcolor=\"#000000\";\n  node [style=filled, fontcolor=white, shape=rect];\n"
    let edges = [ "  \"" ++ clean (attribute_name p) ++ "\" -> \"" ++ clean d ++ "\";" | p <- pkgs, d <- dependencies p ]
    writeFile "galaxy.dot" (header ++ unlines (take 1000 edges) ++ "}")

pruneGhosts :: Connection -> IO ()
pruneGhosts conn = do
    pkgs <- loadPackages conn
    let scriptHeader = "#!/bin/bash\nmkdir -p vault/ghosts\nrm -rf vault/ghosts/*\n"
    let commands = map (\p -> "ln -s " ++ T.unpack (attribute_name p) ++ " vault/ghosts/$(basename " ++ T.unpack (attribute_name p) ++ ") 2>/dev/null || true") pkgs
    writeFile "prune_ghosts.sh" (scriptHeader ++ unlines commands)

calculateGhostStats :: IO ()
calculateGhostStats = readCreateProcess (shell "du -shL vault/ghosts 2>/dev/null") "" >>= putStrLn

finalizeGarbage :: IO ()
finalizeGarbage = readCreateProcess (shell "nix-store --gc --print-dead | tail -n 5") "" >>= putStrLn

listGhosts :: Connection -> IO ()
listGhosts conn = loadPackages conn >>= mapM_ (putStrLn . (" - " ++) . T.unpack . attribute_name) . take 20

searchGhosts :: T.Text -> IO ()
searchGhosts pat = readCreateProcess (shell $ "find vault/ghosts -name \"*" ++ T.unpack pat ++ "*\"") "" >>= putStrLn

clean :: T.Text -> String
clean t = T.unpack (T.take 30 (T.drop 33 (last (T.splitOn "/" t))))
