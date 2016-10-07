{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Omniscient.Server.Core where

import Control.Monad.Reader
import Control.Monad.Logger
import Control.Monad.Except

import Database.Persist.Sql
import Servant

type OmniscientT m =
    LoggingT (ReaderT ConnectionPool (ExceptT ServantErr m))

class
    ( MonadIO m
    , MonadError ServantErr m
    , MonadReader ConnectionPool m
    , MonadLogger m
    ) => Omni m where
instance MonadIO m => Omni (OmniscientT m)

db :: Omni app => _ -> app a
db = (ask >>=) . liftSqlPersistMPool
