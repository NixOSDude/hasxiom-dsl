{-# LANGUAGE OverloadedStrings #-}
module Hasxiom.Core where

import qualified Data.Text as T
import Data.List (nub)
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.Types (PGArray(..))

data Package = Package
    { attribute_name :: T.Text
    , depth          :: Int
    , dependencies   :: [T.Text]
    } deriving (Show, Eq)

instance FromRow Package where
    fromRow = do
        attr <- field
        d    <- field
        (PGArray deps) <- field 
        return $ Package attr d deps

-- | THE DSL
data HasxiomExpr
    = AllPackages
    | DependsOn T.Text HasxiomExpr
    | And HasxiomExpr HasxiomExpr
    deriving (Show, Eq)

-- | FUZZY TAIL RECURSIVE TRANSITIVE CLOSURE
-- Now uses T.isInfixOf to find targets inside full Nix store paths.
getTransitiveDeps :: T.Text -> [Package] -> [T.Text] -> [T.Text]
getTransitiveDeps target allPkgs acc =
    let direct = [ attribute_name p | p <- allPkgs, any (target `T.isInfixOf`) (dependencies p) ]
        newDeps = [ d | d <- direct, d `notElem` acc ]
    in if null newDeps
       then acc
       else foldr (\d currentAcc -> getTransitiveDeps d allPkgs currentAcc) (acc ++ newDeps) newDeps

eval :: HasxiomExpr -> [Package] -> [Package]
eval AllPackages pkgs = pkgs
eval (DependsOn n sub) pkgs = 
    let scope = eval sub pkgs
        transitiveNames = getTransitiveDeps n pkgs []
    in filter (\p -> attribute_name p `elem` transitiveNames) scope
eval (And e1 e2) pkgs = eval e2 (eval e1 pkgs)
