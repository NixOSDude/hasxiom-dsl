{-# LANGUAGE OverloadedStrings #-}
module Hasxiom.Core where

import qualified Data.Text as T
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)

-- | Tenet 1: Total Data Records.
data Package = Package 
    { attrName     :: T.Text
    , depth        :: Int
    , dependencies :: [T.Text] 
    } deriving (Show, Eq)

-- | Bridge to the Lakehouse: Defining the instance where the type lives.
instance FromRow Package where
    fromRow = Package <$> field <*> field <*> (field >>= return . parseDeps)
      where
        parseDeps :: Maybe [T.Text] -> [T.Text]
        parseDeps (Just xs) = xs
        parseDeps Nothing   = []

-- | The Hasxiom DSL Vocabulary
data HasxiomExpr
    = FilterByDepth Int HasxiomExpr
    | DependsOn T.Text HasxiomExpr
    | Identity
    deriving (Show, Eq)

-- | The Engine: Pure Recursive Evaluation
evaluate :: HasxiomExpr -> [Package] -> [Package]
evaluate expr pkgs = case expr of
    Identity -> pkgs
    FilterByDepth d sub -> filter (\p -> depth p > d) (evaluate sub pkgs)
    DependsOn target sub -> filter (\p -> target `elem` dependencies p) (evaluate sub pkgs)
