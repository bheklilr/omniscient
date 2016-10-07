{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Omniscient.Server.API
    ( NewAppAPI
    , UpdateAPI
    , QueryAPI
    , BackendAPI
    , FrontendAPI
    , OmniscientAPI
    , omniscientAPI
    , module Omniscient.Server.Types
    , module Servant
    ) where

import Data.Int
import Data.Proxy (Proxy)

import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html

import Omniscient.Server.Types


-- |A new application request comes in on /app with a JSON body and captures
-- the hostname of the client.
type NewAppAPI
    =  "app"
    :> ReqBody '[JSON] NewAppRequest
    :> RemoteHost  -- I guess this is called RemoteHost when it should be RemoteHostname?
    :> Post '[JSON] NewAppResponse


-- |A new update request comes in on /update/<app_id> with a JSON body and
-- captures the hostname of the client.
type UpdateAPI
    =  "update"
    :> Capture "app" Int64
    :> ReqBody '[JSON] UpdateRequest
    :> RemoteHost
    :> Post '[JSON] UpdateResponse

-- |A new query request comes in on /query/<app_id> with a JSON body.
type QueryAPI
    =  "query"
    :> Capture "app" Int64
    :> ReqBody '[JSON] QueryRequest
    :> Get '[JSON] QueryResponse

type BackendAPI
    =    NewAppAPI
    :<|> UpdateAPI
    :<|> QueryAPI

type Home = Get '[HTML] Html
type View = "view" :> Capture "appName" String :> Get '[HTML] Html

type FrontendAPI = Home :<|> View
-- |The overall server consists of the simple endpoints
-- /new/app, /update/<app_id>, and /query/<app_id>
type OmniscientAPI
    =    BackendAPI
    :<|> FrontendAPI

-- | The overall API type for the application
omniscientAPI :: Proxy OmniscientAPI
omniscientAPI = Proxy
