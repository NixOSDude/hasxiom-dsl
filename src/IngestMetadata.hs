{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Database.PostgreSQL.Simple
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Map.Strict as M
import Data.Aeson
import GHC.Generics
import System.Environment (getEnv)
import Control.Monad (forM_, void)

data PackageMeta = PackageMeta { description :: Maybe T.Text } deriving (Show, Generic)
instance FromJSON PackageMeta where
    parseJSON = withObject "meta" $ \v -> PackageMeta <$> v .:? "description"

data NixPkgInfo = NixPkgInfo
    { meta :: Maybe PackageMeta
    , pname :: Maybe T.Text
    , version :: Maybe T.Text
    } deriving (Show, Generic)

instance FromJSON NixPkgInfo where
    parseJSON = withObject "package" $ \v ->
        NixPkgInfo <$> v .:? "meta" <*> v .:? "pname" <*> v .:? "version"

data PathInfo = PathInfo 
    { outputs :: Maybe (M.Map T.Text (Maybe T.Text)) 
    } deriving (Show, Generic)

instance FromJSON PathInfo where
    parseJSON = withObject "PathInfo" $ \v ->
        PathInfo <$> v .:? "outputs"

main :: IO ()
main = do
    connStr <- getEnv "HASXIOM_DB_CONN"
    conn <- connectPostgreSQL (T.encodeUtf8 $ T.pack connStr)
    
    putStrLn "================================================"
    putStrLn ">> [Hasxiom Ingestor] Sovereignty Mode"
    
    -- Pass 1: Map Attributes to Paths
    putStrLn ">> [1/2] Linking Attributes to Store Paths..."
    pathData <- BL.readFile "actual_paths.json"
    let mPathMap = decode pathData :: Maybe (M.Map T.Text PathInfo)
    
    case mPathMap of
        Nothing -> putStrLn ">> ERROR: actual_paths.json parse failed."
        Just pathMap -> do
            let entries = M.toList pathMap
            putStrLn $ ">> Processing " ++ show (length entries) ++ " path mappings."
            forM_ entries $ \(attr, info) -> do
                case outputs info of
                    Just outs -> case M.lookup "out" outs of
                        Just (Just path) -> do
                            -- Use the store path as the anchor for the UPSERT
                            -- If the path already exists, update its attribute_name
                            void $ execute conn "INSERT INTO nix_packages (attribute_name) VALUES (?) \
                                               \ON CONFLICT DO NOTHING" [path]
                            void $ execute conn "UPDATE nix_packages SET attribute_name = ? WHERE attribute_name = ?" (attr, path)
                        _ -> return ()
                    Nothing -> return ()

    -- Pass 2: Layer Metadata
    putStrLn ">> [2/2] Hydrating Metadata (Descriptions)..."
    pkgData <- BL.readFile "packages.json"
    let mPkgMap = decode pkgData :: Maybe (M.Map T.Text NixPkgInfo)
    
    case mPkgMap of
        Nothing -> putStrLn ">> ERROR: packages.json parse failed."
        Just pkgMap -> do
            let entries = M.toList pkgMap
            putStrLn $ ">> Processing " ++ show (length entries) ++ " metadata entries."
            forM_ entries $ \(attr, info) -> do
                let desc = maybe "" (maybe "" id . description) (meta info)
                -- We update based on attribute_name. Since we dropped the unique constraint, 
                -- this will update all store paths associated with this attribute.
                void $ execute conn "UPDATE nix_packages SET description = ?, package_name = ?, version = ? \
                                   \WHERE attribute_name = ?" (desc, pname info, version info, attr)

    putStrLn ">> Lakehouse Refined. Sovereignty Restored."
    putStrLn "================================================"
    close conn
