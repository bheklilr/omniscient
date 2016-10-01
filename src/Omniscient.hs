{-# LANGUAGE OverloadedStrings #-}
module Omniscient
    ( defaultMain
    ) where

import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist.Sqlite

import Omniscient.Server

-- |The default entry point to the application.  Sets up the database, performs
-- migrations, and starts the server
defaultMain :: IO ()
defaultMain = do
    -- create a pool, logging any problems out to stderr because we probably
    -- don't care
    pool <- runStderrLoggingT $ createSqlitePool "omniscient.db" 5
    -- migrate the database over. Isn't it nice that this happens automatically?
    runSqlPool (runMigration migrateAll) pool
    -- start the server itself on the given port (maybe this should be
    -- configurable in the future when config becomes a thing?)
    runOmniscientServer 8080 pool
