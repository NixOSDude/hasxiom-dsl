{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Database.PostgreSQL.Simple
import Control.Monad (forM_)
import System.Environment (getArgs)

-- This matches the object value for each store path key
data PackageDetail = PackageDetail
    { references :: [T.Text]
    } deriving (Show, Generic)

instance FromJSON PackageDetail

main :: IO ()
main = do
    args <- getArgs
    conn <- connectPostgreSQL "dbname=hasxiom_db user=nixdude"
    case args of
        [pathToFile] -> do
            raw <- BSL.readFile pathToFile
            -- The JSON is a Map of StorePath -> PackageDetail
            case decode raw :: Maybe (Map.Map T.Text PackageDetail) of
                Nothing -> putStrLn "Error: Structural mismatch in store-graph.json."
                Just graphMap -> do
                    let entries = Map.toList graphMap
                    putStrLn $ "Ingesting " ++ show (length entries) ++ " unique packages..."
                    
                    -- 1. Insert Nodes
                    withTransaction conn $ do
                        forM_ entries $ \(path, _) -> do
                            execute conn "INSERT INTO nix_packages (package_name) VALUES (?) ON CONFLICT DO NOTHING" (Only path)

                    -- 2. Insert Edges
                    putStrLn "Linking dependencies..."
                    withTransaction conn $ do
                        forM_ entries $ \(parent, detail) -> do
                            forM_ (references detail) $ \child -> do
                                execute conn "INSERT INTO package_dependencies (parent_id, child_id) \
                                             \SELECT p.id, c.id FROM nix_packages p, nix_packages c \
                                             \WHERE p.package_name = ? AND c.package_name = ?" (parent, child)
                    putStrLn "Ingestion Complete. Graph is now live."
        _ -> putStrLn "Usage: cabal run ingest-metadata -- <store-graph.json>"
    close conn
