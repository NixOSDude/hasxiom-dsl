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

-- Flexible enough to capture the path regardless of nesting
data PathInfo = PathInfo { path :: Maybe T.Text } deriving (Show, Generic)
instance FromJSON PathInfo

main :: IO ()
main = do
    args <- getArgs
    conn <- connectPostgreSQL "dbname=hasxiom_db user=nixdude"
    case args of
        [pathToFile] -> do
            raw <- BSL.readFile pathToFile
            -- Try to decode as Map first, then List
            let decodedMap = decode raw :: Maybe (Map.Map T.Text PathInfo)
            let decodedList = decode raw :: Maybe [PathInfo]
            
            let finalPaths = case (decodedMap, decodedList) of
                    (Just m, _) -> Map.keys m
                    (_, Just l) -> [p | PathInfo (Just p) <- l]
                    _           -> []

            if null finalPaths 
                then putStrLn "Error: Could not find paths in JSON. Check format."
                else do
                    putStrLn $ "Matching " ++ show (length finalPaths) ++ " hashes..."
                    withTransaction conn $ do
                        forM_ finalPaths $ \fullPath -> do
                            let hashPart = T.take 44 fullPath 
                            let parts = T.splitOn "-" fullPath
                            let humanName = if length parts > 1 
                                            then T.intercalate "-" (drop 1 parts) 
                                            else fullPath
                            
                            _ <- execute conn 
                                "UPDATE nix_packages SET attribute_name = ? \
                                \WHERE package_name LIKE ? AND attribute_name IS NULL" 
                                (humanName, hashPart <> "%")
                            return ()
                    putStrLn "Metadata Hydration Complete."
        _ -> putStrLn "Usage: cabal run update-attributes -- <current_shell_meta.json>"
    close conn
