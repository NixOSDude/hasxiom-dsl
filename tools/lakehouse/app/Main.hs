{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Database.PostgreSQL.Simple
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Data.Aeson.Types (Object, Value(..))
import qualified Data.Aeson.KeyMap as KM
import GHC.Generics
import Control.Exception (bracket)
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Data.Set as Set
import Data.Maybe (mapMaybe)
import Data.List (unfoldr)

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = unfoldr (\xs -> if null xs then Nothing else Just (splitAt n xs))

parseDeps :: Object -> [Text]
parseDeps obj = case KM.lookup "deps" obj of
    Just (Array a) -> [t | String t <- V.toList a]
    _              -> []

main :: IO ()
main = do
    putStrLn "Reading Verified Axiom (edges.json)..."
    rawJSON <- B.readFile "edges.json"
    let Just pkgMap = decode rawJSON :: Maybe (Map.Map Text Object)
    
    let connInfo = defaultConnectInfo { connectUser = "nixlakehouse", connectDatabase = "nixlakehouse" }
    bracket (connect connInfo) close $ \conn -> do
        
        putStrLn "Pass 1: Extracting Unique Nodes (Attributes + Store Paths)..."
        let attributes = Map.keys pkgMap
        let allDeps = concatMap parseDeps (Map.elems pkgMap)
        -- Pure Set Union to satisfy O(n log n) uniqueness
        let uniqueNodes = Set.toList $ Set.fromList (attributes ++ allDeps)
        
        putStrLn $ "Ingesting " ++ show (length uniqueNodes) ++ " Nodes..."
        let nodeBatch = [ (n, last (T.splitOn "-" (if T.isInfixOf "/" n then n else "attr-" <> n))) | n <- uniqueNodes ]
        executeMany conn "INSERT INTO nix_packages (attribute_name, package_name) VALUES (?, ?) ON CONFLICT DO NOTHING" nodeBatch

        putStrLn "Pass 2: Indexing IDs into Memory..."
        idRows <- query_ conn "SELECT attribute_name, id FROM nix_packages" :: IO [(Text, Int)]
        let idMap = Map.fromList idRows
        
        putStrLn "Pass 3: Building Edge list..."
        let allLinks = concatMap (\(attr, obj) ->
                let parentID = Map.lookup attr idMap
                    depPaths = parseDeps obj
                    children = mapMaybe (`Map.lookup` idMap) depPaths
                in case parentID of
                    Just p -> [ (p, c) | c <- children ]
                    Nothing -> []
                ) (Map.toList pkgMap)
        
        putStrLn $ "Processing " ++ show (length allLinks) ++ " edges..."
        let linkChunks = chunksOf 5000 allLinks
        mapM_ (\batch -> do
            executeMany conn "INSERT INTO package_dependencies (parent_id, child_id) VALUES (?, ?) ON CONFLICT DO NOTHING" batch
            putStrLn "Batch Committed."
            ) linkChunks
        putStrLn "Graph Axiom Fully Established."
