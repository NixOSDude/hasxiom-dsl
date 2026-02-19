{-# LANGUAGE OverloadedStrings #-}

import Database.PostgreSQL.Simple
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import Control.Exception (bracket)
import qualified Data.Set as Set
import Control.Monad (foldM)

data PkgNode = PkgNode Text [PkgNode] | AlreadySeen Text

fetchChildren :: Connection -> Text -> IO [Text]
fetchChildren conn parentAttr = do
    res <- query conn 
        "SELECT c.attribute_name FROM nix_packages p \
        \JOIN package_dependencies pd ON p.id = pd.parent_id \
        \JOIN nix_packages c ON c.id = pd.child_id \
        \WHERE p.attribute_name = ?" (Only parentAttr)
    return [name | Only name <- res]

buildGraph :: Connection -> Set.Set Text -> Int -> Text -> IO (PkgNode, Set.Set Text)
buildGraph conn seen depth attr
    | Set.member attr seen = return (AlreadySeen attr, seen)
    | depth <= 0           = return (PkgNode attr [], Set.insert attr seen)
    | otherwise = do
        let seen' = Set.insert attr seen
        childNames <- fetchChildren conn attr
        (nodes, finalSeen) <- foldM (\(accNodes, currentSeen) name -> do
            (newNode, nextSeen) <- buildGraph conn currentSeen (depth - 1) name
            return (accNodes ++ [newNode], nextSeen)
            ) ([], seen') childNames
        return (PkgNode attr nodes, finalSeen)

drawTree :: Int -> PkgNode -> IO ()
drawTree indent node = case node of
    AlreadySeen name -> TIO.putStrLn $ T.replicate (indent * 2) " " <> "- " <> name <> " (recursive - seen)"
    PkgNode name children -> do
        TIO.putStrLn $ T.replicate (indent * 2) " " <> "- " <> name
        mapM_ (drawTree (indent + 1)) children

main :: IO ()
main = do
    args <- getArgs
    let target = case args of
                   (x:_) -> T.pack x
                   []    -> "nixos.haskellPackages.gi-gtksheet"
    
    let connInfo = defaultConnectInfo { connectUser = "nixlakehouse", connectDatabase = "nixlakehouse" }
    bracket (connect connInfo) close $ \conn -> do
        (tree, finalSeen) <- buildGraph conn Set.empty 10 target
        drawTree 0 tree
        putStrLn "\n--- Closure Analysis ---"
        putStrLn $ "Target: " ++ T.unpack target
        putStrLn $ "Unique Packages in DB Closure: " ++ show (Set.size finalSeen)
        putStrLn "------------------------"
