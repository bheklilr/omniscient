{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Omniscient.Server.API
    ( NewAppAPI
    , UpdateAPI
    , QueryAPI
    , OmniscientAPI
    , omniscientAPI
    ) where

import Data.Int
import Data.Proxy (Proxy)

import Servant

import Omniscient.Server.Types


-- |A new application request comes in on /new/app with a JSON body and captures
-- the hostname of the client.
type NewAppAPI
    =  "new"
    :> "app"
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


-- |The overall server consists of the simple endpoints
-- /new/app, /update/<app_id>, and /query/<app_id>
type OmniscientAPI
    =    NewAppAPI
    :<|> UpdateAPI
    :<|> QueryAPI

omniscientAPI :: Proxy OmniscientAPI
omniscientAPI = Proxy
