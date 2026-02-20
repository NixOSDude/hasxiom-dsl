{-# LANGUAGE OverloadedStrings #-}

module Hasxiom.Database.Postgres where

import Database.PostgreSQL.Simple
import Data.Text (Text)

-- Tenet: No Nulls. We assume the environment provides the connection info.
-- This connects Node A (Ultra 7) to the Postgres instance on Node B (nixlakehouse)
getLakehouseConn :: IO Connection
getLakehouseConn = connect defaultConnectInfo
    { connectHost     = "192.168.68.56"
    , connectDatabase = "hasxiom_db"
    , connectUser     = "nixlakehouse"
    , connectPassword = "" -- Ensure pg_hba.conf allows internal LAN access
    }

-- Theory: Solving the Malte 13k Node Challenge
-- This function retrieves the 'coherency status' of a specific package node.
queryNodeStatus :: Connection -> Text -> IO (Maybe Text)
queryNodeStatus conn name = do
    results <- query conn "SELECT status FROM packages WHERE name = ?" (Only name)
    case results of
        [Only status] -> return (Just status)
        _             -> return Nothing -- Total function: returns Nothing if not found
