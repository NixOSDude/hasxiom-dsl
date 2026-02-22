{-# LANGUAGE OverloadedStrings #-}

module Hasxiom.Database.Loader (loadPackages) where

import Database.PostgreSQL.Simple
import Hasxiom.Core (Package(..))

-- Tenet: No Nulls. Use COALESCE to handle 'Ghost' nodes without metadata.
loadPackages :: Connection -> IO [Package]
loadPackages conn = do
    putStrLn ">> Querying Lakehouse for Infrastructure Graph..."
    query_ conn $ mconcat
        [ "SELECT "
        , "  attribute_name, "
        , "  COALESCE(package_name, ''), "
        , "  COALESCE(version, ''), "
        , "  COALESCE(description, ''), "
        , "  COALESCE(depth, 0), "
        , "  dependencies "
        , "FROM nix_packages"
        ]
