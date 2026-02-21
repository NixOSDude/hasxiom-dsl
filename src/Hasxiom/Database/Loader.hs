{-# LANGUAGE OverloadedStrings #-}
module Hasxiom.Database.Loader where

import Database.PostgreSQL.Simple
import Hasxiom.Core (Package)
import qualified Data.Text as T

-- | PURE CONFIGURATION
-- HOST: Ultra 7 (192.168.68.53) | Database: hasxiom_db
connInfo :: ConnectInfo
connInfo = defaultConnectInfo 
    { connectHost     = "localhost"
    , connectDatabase = "hasxiom_db"
    , connectUser     = "nixdude"
    }

-- | THE LOADER
-- Enforcing NO NULLS by filtering attribute_name in the WHERE clause.
fetchAllPackages :: IO [Package]
fetchAllPackages = do
    conn <- connect connInfo
    pkgs <- query_ conn q
    close conn
    return pkgs
  where
    q = "SELECT p.attribute_name, p.depth, COALESCE(ARRAY_AGG(d.package_name) FILTER (WHERE d.package_name IS NOT NULL), '{}') \
        \FROM nix_packages p \
        \LEFT JOIN package_dependencies pd ON p.id = pd.parent_id \
        \LEFT JOIN nix_packages d ON pd.child_id = d.id \
        \WHERE p.attribute_name IS NOT NULL \
        \GROUP BY p.id, p.attribute_name, p.depth"
