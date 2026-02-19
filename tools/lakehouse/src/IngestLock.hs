{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Database.PostgreSQL.Simple
import Control.Exception (bracket)

data FlakeLock = FlakeLock { nodes :: Map Text Node } deriving (Show, Generic)
data Node = Node { inputs :: Maybe (Map Text Value) } deriving (Show, Generic)

instance FromJSON FlakeLock
instance FromJSON Node

main :: IO ()
main = do
    let connInfo = defaultConnectInfo { connectUser = "nixlakehouse", connectDatabase = "nixlakehouse" }
    content <- B.readFile "flake.lock"
    case (decode content :: Maybe FlakeLock) of
        Nothing -> putStrLn "Error: Could not parse flake.lock."
        Just lock -> bracket (connect connInfo) close $ \conn -> do
            putStrLn "Ingesting Flake Nodes (Total Mapping)..."
            -- We pass the name to both columns to satisfy the NOT NULL constraint
            mapM_ (insertPackage conn) (Map.keys $ nodes lock)
            mapM_ (insertDeps conn lock) (Map.toList $ nodes lock)
            putStrLn "Ingestion Complete."

insertPackage :: Connection -> Text -> IO ()
insertPackage conn name = do
    -- We provide 'name' for both attribute_name and package_name
    _ <- execute conn "INSERT INTO nix_packages (attribute_name, package_name) VALUES (?, ?) ON CONFLICT (attribute_name) DO NOTHING" (name, name)
    return ()

insertDeps :: Connection -> FlakeLock -> (Text, Node) -> IO ()
insertDeps conn lock (parentName, node) = case inputs node of
    Nothing -> return ()
    Just ins -> mapM_ handleInput (Map.elems ins)
  where
    handleInput val = case val of
        String childName -> linkDeps conn parentName childName
        -- Flake locks sometimes use lists for inputs; we extract the first string if it exists
        Array arr -> case (decode (encode arr) :: Maybe [Text]) of
                        Just (child:_) -> linkDeps conn parentName child
                        _ -> return ()
        _ -> return () 

linkDeps :: Connection -> Text -> Text -> IO ()
linkDeps conn p c = do
    _ <- execute conn 
        "INSERT INTO package_dependencies (parent_id, child_id) \
        \SELECT p.id, c.id FROM nix_packages p, nix_packages c \
        \WHERE p.attribute_name = ? AND c.attribute_name = ? \
        \ON CONFLICT DO NOTHING" (p, c)
    return ()
