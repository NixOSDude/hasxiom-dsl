{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Hasxiom.Language
Description : The "Fluent API" for the Hasxiom DSL.

This module provides the "Combinators" and "Infix Operators" that allow us to 
write queries that look like natural sentences. This is a "Shallow Embedding" 
pattern where we use Haskell functions to construct a "Deep" Abstract 
Syntax Tree (the HasxiomExpr).
-}
module Hasxiom.Language where

import Hasxiom.Core
import qualified Data.Text as T

-- | THE LEAF (Identity)
-- In any recursive tree, you need a "base case." 
-- 'everything' represents the unfiltered state of the Lakehouse.
-- Concept: This is the identity element of our algebra.
everything :: HasxiomExpr
everything = Identity

-- | THE DEPTH COMBINATOR
-- Type: Takes an Int, then an existing expression, and returns a new expression.
-- 
-- Theory (Currying): 'depthOver 100' is a partially applied function. 
-- It is a "Transformation" waiting for a subject (the 'sub' expression).
-- Example: depthOver 100 everything
depthOver :: Int -> HasxiomExpr -> HasxiomExpr
depthOver d sub = FilterByDepth d sub

-- | THE DEPENDENCY COMBINATOR
-- Type: T.Text -> HasxiomExpr -> HasxiomExpr
--
-- Logic: This wraps the subject 'sub' in a 'DependsOn' constructor.
-- Mastery Note: Because Haskell is lazy, this tree isn't "evaluated" until 
-- we pass it to the 'evaluate' function in Core.hs.
dependsOn :: T.Text -> HasxiomExpr -> HasxiomExpr
dependsOn pkg sub = DependsOn pkg sub

-- | THE 'AND' OPERATOR (Conjunction)
-- Concept: This is an Alias for the 'And' constructor.
-- By using symbols (<&>), we make the code look like Logic/Math.
--
-- Type: HasxiomExpr -> HasxiomExpr -> HasxiomExpr
(<&>) :: HasxiomExpr -> HasxiomExpr -> HasxiomExpr
(<&>) = And

-- | THE 'OR' OPERATOR (Disjunction)
-- Concept: Combines two trees. If either branch is true, the package is kept.
(<|>) :: HasxiomExpr -> HasxiomExpr -> HasxiomExpr
(<|>) = Or

-- | THE 'NOT' OPERATOR (Negation)
-- Concept: Inverts the logic of the expression provided.
-- Usage: isNot (dependsOn "lens" everything)
isNot :: HasxiomExpr -> HasxiomExpr
isNot = Not

-- | FIXITY DECLARATIONS
-- This is where we define the "Grammar" of our language.
--
-- 'infixl' means Left-Associative: A <&> B <&> C becomes (A <&> B) <&> C.
-- The number (0-9) is Precedence: Higher numbers bind tighter.
--
-- We give <&> (AND) priority 3 and <|> (OR) priority 2.
-- Just like in math (Multiplication before Addition), logic usually 
-- processes AND before OR. 
-- Example: A <|> B <&> C  ==>  A <|> (B <&> C)
infixl 3 <&>
infixl 2 <|>

{- 
  STUDY NOTE: POINT-FREE STYLE
  Observe that (<&>) = And.
  We could have written: (<&>) a b = And a b.
  But since the "right-most variables" match the constructor's arguments, 
  we can drop them. This is "Eta Reduction." 
  It treats the constructor itself as the function.
-}
