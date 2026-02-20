module Main where
import Hasxiom.Core
main :: IO ()
main = do
    let mock = [Package "aeson" 116, Package "libtorch" 50]
    let query = FilterByDepth 100 Identity
    putStrLn ">> [Hasxiom] Evaluating DSL against mock store..."
    print $ evaluate query mock
