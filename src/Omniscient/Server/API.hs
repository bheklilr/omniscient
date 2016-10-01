{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Omniscient.Server.API
    ( NewAppAPI
    , UpdateAPI
    , QueryAPI
    , OmniscientAPI
    , omniscientAPI
    ) where

import Data.Proxy (Proxy)

import Servant

import Omniscient.Server.Types



type NewAppAPI
    =  "new"
    :> "app"
    :> ReqBody '[JSON] NewAppRequest
    :> Post '[JSON] NewAppResponse

type UpdateAPI
    =  "update"
    :> Capture "app" Int
    :> ReqBody '[JSON] UpdateRequest
    :> Post '[JSON] UpdateResponse

type QueryAPI
    =  "query"
    :> Capture "app" Int
    :> ReqBody '[JSON] QueryRequest
    :> Get '[JSON] QueryResponse


type OmniscientAPI
    =    NewAppAPI
    :<|> UpdateAPI
    :<|> QueryAPI

omniscientAPI :: Proxy OmniscientAPI
omniscientAPI = Proxy
