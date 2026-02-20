{-# LANGUAGE OverloadedStrings #-}
module Main where

import Hasxiom.Core
import Hasxiom.Database.Postgres
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
    putStrLn ">> [Hasxiom] Verifying Final Foundation..."
    
    -- (Depth > 100 AND DependsOn aeson) AND (NOT DependsOn lens)
    let query = And 
                  (And (FilterByDepth 100 Identity) (DependsOn "aeson" Identity))
                  (Not (DependsOn "lens" Identity))
    
    let sql = compileQuery query
    TIO.putStrLn $ "Generated SQL: " <> sql
