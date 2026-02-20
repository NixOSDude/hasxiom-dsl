{-# LANGUAGE OverloadedStrings #-}
module Main where

import Hasxiom.Core
import Hasxiom.Database.Postgres
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
    putStrLn ">> [Hasxiom] Compiling DSL to SQL..."
    
    -- Our complex relationship query
    -- FilterByDepth 117 (DependsOn "aeson" Identity)
    let query = FilterByDepth 117 (DependsOn "aeson" Identity)
    
    let sql = compileQuery query
    
    putStrLn "Generated SQL for nixlakehouse:"
    TIO.putStrLn sql
