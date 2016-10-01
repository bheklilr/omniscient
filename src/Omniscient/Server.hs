{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Omniscient.Server
    ( runOmniscientServer
    ) where

import Control.Monad.Reader
import Control.Monad.Logger

import Database.Persist.Sql (ConnectionPool)

import Network.Socket
import Network.Wai.Handler.Warp as Warp

import Servant

import Omniscient.Server.Types
import Omniscient.Server.API


omniscientServer :: ServerT OmniscientAPI (OmniscientT IO)
omniscientServer
    =    newAppHandler
    :<|> updateHandler
    :<|> queryHandler


runOmniscientServer :: Port -> ConnectionPool -> IO ()
runOmniscientServer port pool =
    Warp.run port
        $ serve omniscientAPI
        $ enter unwrapOmniscientT omniscientServer
    where
        unwrapOmniscientT' :: forall a. OmniscientT IO a -> Handler a
        unwrapOmniscientT' app = do
            readerT <- return $ runStderrLoggingT app
            handler <- liftIO $ runReaderT readerT pool
            return handler
        unwrapOmniscientT :: OmniscientT IO :~> Handler
        unwrapOmniscientT = Nat unwrapOmniscientT'


type OmniscientT m =
    LoggingT (ReaderT ConnectionPool m)

class
    ( MonadIO m
    , MonadReader ConnectionPool m
    , MonadLogger m
    ) => Omni m where
instance MonadIO m => Omni (OmniscientT m) where


newAppHandler :: Omni app => NewAppRequest -> SockAddr -> app NewAppResponse
newAppHandler request host = do
    $logDebugSH request
    return $ NewAppResponse $ Left $ NewAppError "Unimplemented"


updateHandler :: Omni app => Int -> UpdateRequest -> SockAddr -> app UpdateResponse
updateHandler appID request host = do
    $logDebugSH appID
    $logDebugSH request
    return $ UpdateResponse $ Left $ UpdateError "Unimplemented"

queryHandler :: Omni app => Int -> QueryRequest -> app QueryResponse
queryHandler appID request = do
    $logDebugSH appID
    $logDebugSH request
    return $ QueryResponse $ Left $ QueryError "Unimplemented"
