{-# LANGUAGE OverloadedStrings #-}

module Hasxiom.Database.Postgres where

import Hasxiom.Core
import qualified Data.Text as T

-- | Predicate generator: Converts DSL verbs into SQL logic fragments
toPredicate :: HasxiomExpr -> [T.Text]
toPredicate expr = case expr of
    Identity -> []
    FilterByDepth d sub -> 
        ("depth > " <> T.pack (show d)) : toPredicate sub
    DependsOn target sub -> 
        ("'" <> target <> "' = ANY(dependencies)") : toPredicate sub

-- | The Compiler: Joins predicates with 'AND' and adds the 'WHERE' clause
compileQuery :: HasxiomExpr -> T.Text
compileQuery expr =
    let base = "SELECT attribute_name, depth FROM nix_packages"
        preds = toPredicate expr
    in case preds of
        [] -> base <> ";"
        _  -> base <> " WHERE " <> T.intercalate " AND " preds <> ";"
