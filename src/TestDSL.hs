{-# LANGUAGE OverloadedStrings #-}
module Main where

import Hasxiom.Core

main :: IO ()
main = do
    putStrLn ">> [Hasxiom] Running Relationship Query..."
    
    -- Mocking a small slice of the Lakehouse
    let mockStore = 
          [ Package "aeson" 116 []
          , Package "postgresql-simple" 118 ["aeson"]
          , Package "hasxiom-engine" 120 ["postgresql-simple", "aeson"]
          , Package "random-lib" 50 ["base"]
          ]
    
    -- DSL Expression: "Find everything depending on 'aeson' with depth > 117"
    let query = FilterByDepth 117 (DependsOn "aeson" Identity)
    
    let results = evaluate query mockStore
    
    putStrLn "Results (Depth > 117 + Depends on 'aeson'):"
    print results
