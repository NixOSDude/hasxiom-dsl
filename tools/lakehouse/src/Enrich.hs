{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Database.PostgreSQL.Simple
import Data.Text (Text)
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map as Map
import Control.Exception (bracket)
import Data.Maybe (fromMaybe)
import Control.Monad (forM_)

data PkgMetadata = PkgMetadata { description :: Maybe Text } deriving (Show)

instance FromJSON PkgMetadata where
    parseJSON = withObject "metadata" $ \v -> do
        meta <- v .:? "meta" .!= KM.empty
        desc <- meta .:? "description"
        return $ PkgMetadata desc

data PkgEntry = PkgEntry { version :: Maybe Text, metadata :: PkgMetadata } deriving (Show)

instance FromJSON PkgEntry where
    parseJSON = withObject "entry" $ \v ->
        PkgEntry <$> v .:? "version"
                 <*> parseJSON (Object v)

main :: IO ()
main = do
    putStrLn "Reading Deep Axiom (packages.json)..."
    rawJSON <- B.readFile "packages.json"
    let Just pkgMap = decode rawJSON :: Maybe (Map.Map Text PkgEntry)
    
    let connInfo = defaultConnectInfo { connectUser = "nixlakehouse", connectDatabase = "nixlakehouse" }
    bracket (connect connInfo) close $ \conn -> do
        putStrLn "Pass 3: Extracting metadata..."
        idRows <- query_ conn "SELECT attribute_name FROM nix_packages WHERE description IS NULL" :: IO [Only Text]
        
        let updates = [ (fromMaybe "No description" (description (metadata entry)), 
                         fromMaybe "unknown" (version entry), 
                         attr) 
                      | Only attr <- idRows, Just entry <- [Map.lookup attr pkgMap] ]
        
        putStrLn $ "Applying metadata to " ++ show (length updates) ++ " packages..."
        
        -- Use forM_ for sequential updates to avoid executeMany template errors
        forM_ updates $ \(desc, ver, attr) -> 
            execute conn "UPDATE nix_packages SET description = ?, version = ? WHERE attribute_name = ?" (desc, ver, attr)
        
        putStrLn "Enrichment Complete."
