{-# LANGUAGE OverloadedStrings #-}

module Hasxiom.Database.Postgres where

import Hasxiom.Core
import qualified Data.Text as T

-- | toPredicate: The core of the compiler. 
-- It transforms our high-level AST into low-level SQL fragments.
toPredicate :: HasxiomExpr -> [T.Text]
toPredicate expr = case expr of
    Identity -> []
    
    FilterByDepth d sub -> 
        ("depth > " <> T.pack (show d)) : toPredicate sub
        
    DependsOn target sub -> 
        ("'" <> target <> "' = ANY(dependencies)") : toPredicate sub

    And e1 e2 -> 
        toPredicate e1 ++ toPredicate e2

    -- Logic: Wrap OR in parentheses to maintain operator precedence
    Or e1 e2 -> 
        let p1 = T.intercalate " AND " (toPredicate e1)
            p2 = T.intercalate " AND " (toPredicate e2)
        in [ "(" <> p1 <> " OR " <> p2 <> ")" ]

    -- Logic: Using SQL NOT for Boolean Completeness
    Not e1 -> 
        [ "NOT (" <> T.intercalate " AND " (toPredicate e1) <> ")" ]

-- | compileQuery: The "Entry Point" for the SQL backend.
compileQuery :: HasxiomExpr -> T.Text
compileQuery expr =
    let base = "SELECT attribute_name, depth, dependencies FROM nix_packages"
        preds = toPredicate expr
    in case preds of
        [] -> base <> ";"
        _  -> base <> " WHERE " <> T.intercalate " AND " preds <> ";"
