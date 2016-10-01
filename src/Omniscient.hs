{-# LANGUAGE OverloadedStrings #-}
module Omniscient
    ( defaultMain
    ) where

import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist.Sqlite

import Omniscient.Server
import Omniscient.Server.Models

defaultMain :: IO ()
defaultMain = do
    pool <- runStderrLoggingT $ do
        createSqlitePool "omniscient.db" 5
    runSqlPool (runMigration migrateAll) pool
    runOmniscientServer 8080 pool
