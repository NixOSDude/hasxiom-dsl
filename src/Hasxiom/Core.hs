module Hasxiom.Core where
import qualified Data.Text as T

-- The 4 Tenets: Total data records
data Package = Package { attrName :: T.Text, depth :: Int } deriving (Show, Eq)

-- The AST: Our DSL verbs
data HasxiomExpr
    = FilterByDepth Int HasxiomExpr
    | Identity
    deriving (Show, Eq)

-- The Engine: Pure recursive evaluation
evaluate :: HasxiomExpr -> [Package] -> [Package]
evaluate expr pkgs = case expr of
    Identity -> pkgs
    FilterByDepth d sub -> filter (\p -> depth p > d) (evaluate sub pkgs)
