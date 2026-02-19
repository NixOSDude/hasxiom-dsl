{-# LANGUAGE OverloadedStrings #-}
import Database.PostgreSQL.Simple
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T

main :: IO ()
main = do
    putStrLn "Reading edges.json to extract Nodes..."
    raw <- B.readFile "edges.json"
    let Just mapping = decode raw :: Maybe (HM.HashMap Text Object)
    conn <- connect defaultConnectInfo { connectUser = "nixlakehouse", connectDatabase = "nixlakehouse" }
    
    putStrLn "Ingesting Nodes into nix_packages..."
    let rows = HM.keys mapping
    forM_ rows $ \attr -> do
        -- Pure Transformation: "nixos.pkg" -> ("nixos.pkg", "pkg")
        let pkgName = last (T.splitOn "." attr)
        execute conn "INSERT INTO nix_packages (attribute_name, package_name) VALUES (?, ?) ON CONFLICT DO NOTHING" (attr, pkgName)
    
    putStrLn "Nodes Ingested."
