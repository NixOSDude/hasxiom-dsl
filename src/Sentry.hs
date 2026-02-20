{-# LANGUAGE OverloadedStrings #-}
module Main where
import Database.PostgreSQL.Simple
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Exit (exitFailure)

main :: IO ()
main = do
    conn <- connectPostgreSQL "dbname=hasxiom_db user=nixdude"
    putStrLn ">> [Hasxiom] Sentry: Validating Pillar Integrity..."
    -- We use Only to wrap the single-column return from Postgres
    livePillars <- query_ conn "SELECT attribute_name FROM nix_packages WHERE depth >= 110" :: IO [Only T.Text]
    localPins <- TIO.readFile "pinned-packages.nix"
    let liveNames = map fromOnly livePillars
    let broken = filter (\n -> not (n `T.isInfixOf` localPins)) liveNames
    if null broken
        then putStrLn ">> [STATUS: GREEN] All 119-layer pillars are stable."
        else do
            putStrLn "!! [STATUS: RED] DRIFT DETECTED IN PILLARS:"
            mapM_ TIO.putStrLn broken
            exitFailure
    close conn
