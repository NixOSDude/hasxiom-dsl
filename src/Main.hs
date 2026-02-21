{-# LANGUAGE OverloadedStrings #-}
module Main where

import Hasxiom.Database.Loader (fetchAllPackages)
import Hasxiom.Core
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Console.Haskeline

-- | THE PROFESSIONAL REPL
-- Using Haskeline to support Ctrl-L, history, and clean exits.
main :: IO ()
main = do
    TIO.putStrLn "--- Hasxiom Engine Active [Ultra 7] ---"
    allPkgs <- fetchAllPackages
    TIO.putStrLn $ "Status: Hydrated " <> T.pack (show (length allPkgs)) <> " nodes."
    TIO.putStrLn "Type a package name. Supports Ctrl-L to clear."
    
    -- runInputT handles terminal interactions and Ctrl-L automatically
    runInputT defaultSettings (replLoop allPkgs)

replLoop :: [Package] -> InputT IO ()
replLoop pkgs = do
    minput <- getInputLine "hasxiom> "
    case minput of
        Nothing -> outputStrLn "Exit."
        Just "exit" -> outputStrLn "Shutting down."
        Just input -> do
            let target = T.pack input
            -- Using a fuzzy eval to catch partial matches in the Nix store paths
            let results = eval (DependsOn target AllPackages) pkgs
            let impact  = length results
            
            outputStrLn $ "Target: " ++ input
            outputStrLn $ "Blast Radius: " ++ show impact ++ " downstream nodes."
            
            if impact > 0
                then mapM_ (outputStrLn . ("  - " ++) . T.unpack . attribute_name) (take 10 results)
                else outputStrLn "No downstream impact found. Try a partial name (e.g. 'stdenv')."
            
            replLoop pkgs
