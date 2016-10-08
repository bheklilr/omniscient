{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Omniscient.Server
    ( module Omniscient.Server.Types
    , module Omniscient.Server.API
    , module Omniscient.Server.Models
    , omniscientServer
    , runOmniscientServer
    , OmniscientT
    , Omni
    , db
    , newAppHandler
    , updateHandler
    , queryHandler
    ) where

import Control.Monad.Reader
import Control.Monad.Logger

import Database.Persist.Sql hiding ((==.), (<.), (>.))

import Network.Wai.Handler.Warp as Warp

import Servant
import Servant.JS

import Omniscient.Server.Types
import Omniscient.Server.Models
import Omniscient.Server.API
import Omniscient.Server.Core
import Omniscient.Server.Backend
import Omniscient.Server.Frontend


omniscientServer :: ServerT OmniscientAPI (OmniscientT IO)
omniscientServer
    =    backend
    :<|> frontend

runOmniscientServer :: Port -> ConnectionPool -> IO ()
runOmniscientServer port pool = do
    writeJSForAPI backendAPI vanillaJS "static/api.js"
    Warp.run port
        $ serve (Proxy :: Proxy (OmniscientAPI :<|> ("static" :> Raw)))
        $ enter unwrapOmniscientT omniscientServer :<|> serveDirectory "static"
    where
        unwrapOmniscientT' :: forall a. OmniscientT IO a -> Handler a
        unwrapOmniscientT' app = do
            readerT <- return $ runStderrLoggingT app
            handler <- runReaderT readerT pool
            return handler
        unwrapOmniscientT :: OmniscientT IO :~> Handler
        unwrapOmniscientT = Nat unwrapOmniscientT'
