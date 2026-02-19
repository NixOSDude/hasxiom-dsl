{-# LANGUAGE OverloadedStrings #-}

import Database.PostgreSQL.Simple
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import Control.Exception (bracket)
import qualified Data.Set as Set
import Control.Monad (foldM)

-- Recursively find all parents (Inverse Graph Traversal)
findImpact :: Connection -> Set.Set Text -> Text -> IO (Set.Set Text)
findImpact conn seen target = do
    -- Find immediate parents
    res <- query conn 
        "SELECT p.attribute_name FROM nix_packages p \
        \JOIN package_dependencies pd ON p.id = pd.parent_id \
        \JOIN nix_packages c ON c.id = pd.child_id \
        \WHERE c.attribute_name = ?" (Only target)
    
    let parents = [p | Only p <- res]
    -- Only recurse on parents we haven't processed yet
    let newParents = filter (\p -> not (Set.member p seen)) parents
    
    foldM (\acc p -> findImpact conn acc p) (Set.union seen (Set.fromList newParents)) newParents

main :: IO ()
main = do
    args <- getArgs
    let target = case args of
                   (x:_) -> T.pack x
                   []    -> "/nix/store/vr7ds8vwbl2fz7pr221d5y0f8n9a5wda-glibc-2.40-218"
    
    let connInfo = defaultConnectInfo { connectUser = "nixlakehouse", connectDatabase = "nixlakehouse" }
    bracket (connect connInfo) close $ \conn -> do
        putStrLn $ "Calculating Impact Radius for: " ++ T.unpack target
        impactSet <- findImpact conn Set.empty target
        putStrLn $ "Total Impacted Packages: " ++ show (Set.size impactSet)
        -- Print first 10 for a quick look
        mapM_ TIO.putStrLn (take 10 $ Set.toList impactSet)
        putStrLn "... (truncated)"
