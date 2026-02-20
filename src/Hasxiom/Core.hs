{-# LANGUAGE OverloadedStrings #-}
module Hasxiom.Core where

import qualified Data.Text as T
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
import Database.PostgreSQL.Simple.Types (PGArray(..))
import Data.List (intersect, union)

data Package = Package 
    { attrName     :: T.Text
    , depth        :: Int
    , dependencies :: [T.Text] 
    } deriving (Show, Eq)

instance FromRow Package where
    fromRow = Package <$> field <*> field <*> ((\(PGArray xs) -> xs) <$> field)

-- | THE COMPLETE HASXIOM AST
data HasxiomExpr
    = Identity
    | FilterByDepth Int HasxiomExpr
    | DependsOn T.Text HasxiomExpr
    | And HasxiomExpr HasxiomExpr
    | Or HasxiomExpr HasxiomExpr
    | Not HasxiomExpr              -- Added for completeness
    deriving (Show, Eq)

-- | Semantics: How our language "thinks"
evaluate :: HasxiomExpr -> [Package] -> [Package]
evaluate expr pkgs = case expr of
    Identity             -> pkgs
    FilterByDepth d sub  -> filter (\p -> depth p > d) (evaluate sub pkgs)
    DependsOn target sub -> filter (\p -> target `elem` dependencies p) (evaluate sub pkgs)
    And e1 e2            -> (evaluate e1 pkgs) `intersect` (evaluate e2 pkgs)
    Or e1 e2             -> (evaluate e1 pkgs) `union` (evaluate e2 pkgs)
    Not e1               -> pkgs `filterOut` (evaluate e1 pkgs)
      where
        filterOut all out = filter (\p -> p `notElem` out) all
