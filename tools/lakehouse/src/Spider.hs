{-# LANGUAGE OverloadedStrings #-}

import Database.PostgreSQL.Simple
import System.Process (readCreateProcess, shell)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (forM_, foldM)
import Control.Exception (try, SomeException)

-- Safely query the Nix store, suppressing stderr
getNixDeps :: Text -> IO [Text]
getNixDeps path = do
    let cmd = shell $ "nix-store -q --references " ++ T.unpack path ++ " 2>/dev/null"
    result <- try (readCreateProcess cmd "") :: IO (Either SomeException String)
    case result of
        Left _ -> return [] 
        Right output -> return $ filter (not . T.null) . map T.pack . lines $ output

main :: IO ()
main = do
    let connInfo = defaultConnectInfo { connectUser = "nixlakehouse", connectDatabase = "nixlakehouse" }
    conn <- connect connInfo
    
    putStrLn "Scanning 1000 nodes for local store matches..."
    -- Use Query_ for raw SQL without parameters
    targets <- query_ conn "SELECT attribute_name FROM nix_packages WHERE id NOT IN (SELECT parent_id FROM package_dependencies) LIMIT 5000" :: IO [Only Text]
    
    foundCount <- foldM (\acc (Only path) -> do
        deps <- getNixDeps path
        case deps of
            [] -> return acc
            _  -> do
                putStrLn $ "[" ++ show (acc + 1) ++ "] Found & Ingested: " ++ T.unpack path
                forM_ deps $ \dep -> do
                    execute conn "INSERT INTO nix_packages (attribute_name, package_name) VALUES (?, ?) ON CONFLICT (attribute_name) DO NOTHING" (dep, dep)
                    execute conn "INSERT INTO package_dependencies (parent_id, child_id) \
                                 \SELECT p.id, c.id FROM nix_packages p, nix_packages c \
                                 \WHERE p.attribute_name = ? AND c.attribute_name = ? \
                                 \ON CONFLICT DO NOTHING" (path, dep)
                return (acc + 1)
        ) (0 :: Int) targets
    
    close conn
    putStrLn $ "Spidering cycle complete. New paths found: " ++ show foundCount
