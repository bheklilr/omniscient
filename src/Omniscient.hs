{-# LANGUAGE OverloadedStrings #-}
module Omniscient
    ( defaultMain
    ) where

import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist.Sqlite

import Omniscient.Server

defaultMain :: IO ()
defaultMain = do
    pool <- runStderrLoggingT $
        createSqlitePool "omniscient.db" 5
    runOmniscientServer 8080 pool
