{-# LANGUAGE OverloadedStrings #-}

module Hasxiom.Core where

import qualified Data.Text as T
import Data.Hashable (hash)
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types (PGArray(..))

-- Tenet: Immutable records. No Nulls.   
-- Expanded to 6 fields to support the reified Lakehouse.
data Package = Package
    { attribute_name :: T.Text
    , package_name   :: T.Text
    , version        :: T.Text
    , description    :: T.Text
    , depth          :: Int
    , dependencies   :: [T.Text]
    } deriving (Show, Eq)

instance FromRow Package where
    fromRow = do
        attr  <- field -- 1. attribute_name (Text)
        pname <- field -- 2. package_name (Text)
        ver   <- field -- 3. version (Text)
        desc  <- field -- 4. description (Text)
        d     <- field -- 5. depth (Int/int4)
        (PGArray deps) <- field -- 6. dependencies (Array)
        return $ Package attr pname ver desc d deps

-- DSL for Symbolic Reasoning
data HasxiomExpr
    = AllPackages
    | Search T.Text
    | DependsOn T.Text HasxiomExpr
    | And HasxiomExpr HasxiomExpr
    deriving (Show, Eq)

-- Recursive walk to identify lineage.   
-- Tail-recursive style preserved via accumulator.
getTransitiveDeps :: T.Text -> [Package] -> [T.Text] -> [T.Text]
getTransitiveDeps target allPkgs acc =
    let direct = [ attribute_name p | p <- allPkgs, any (target `T.isInfixOf`) (dependencies p) ]
        newDeps = [ d | d <- direct, d `notElem` acc ]
    in if null newDeps
       then acc
       else foldr (\d currentAcc -> getTransitiveDeps d allPkgs currentAcc) (acc ++ newDeps) newDeps

-- Tenet: Pure functions. O(n) complexity for filtering.
eval :: HasxiomExpr -> [Package] -> [Package]
eval AllPackages pkgs = pkgs
eval (Search term) pkgs =  
    let lowerTerm = T.toLower term
    in filter (\p -> lowerTerm `T.isInfixOf` T.toLower (description p) ||  
                     lowerTerm `T.isInfixOf` T.toLower (attribute_name p)) pkgs
eval (DependsOn n sub) pkgs =   
    let scope = eval sub pkgs
        transitiveNames = getTransitiveDeps n pkgs []
    in filter (\p -> attribute_name p `elem` transitiveNames) scope
eval (And e1 e2) pkgs = eval e2 (eval e1 pkgs)

-- Sovereign Vectorizer (Internal Tensor Logic)
vectorize :: T.Text -> [Float]
vectorize path =    
    let p = T.toLower path
        f1 = if "cuda" `T.isInfixOf` p then 1.0 else 0.0
        f2 = if "haskell" `T.isInfixOf` p || "ghc" `T.isInfixOf` p then 1.0 else 0.0
        f3 = if "lib" `T.isInfixOf` p then 1.0 else 0.0
        f4 = fromIntegral (abs (hash (T.unpack p)) `mod` 1000) / 1000.0
    in [f1, f2, f3, f4]
