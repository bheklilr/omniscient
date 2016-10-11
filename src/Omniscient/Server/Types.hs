{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Omniscient.Server.Types
    ( NewAppRequest(..)
    , NewAppResponse(..)
    , UpdateRequest(..)
    , UpdateResponse(..)
    , QueryRequest(..)
    , QueryResponse(..)
    , NewAppError(..)
    , UpdateError(..)
    , QueryError(..)
    , AppID
    , UpdateID
    , QueryType(..)
    , TimeWindow(..)
    , Query(..)
    , defQuery
    , QueryResults(..)
    , Evt(..)
    , newAppRequestFailed, newAppRequestSucceeded
    , updateRequestFailed, updateRequestSucceeded
    , queryRequestFailed, queryRequestSucceeded
    ) where

import GHC.Generics
import Data.Time
import Data.Int
import Data.Aeson
import Data.Aeson.Types
import Database.Persist.Sql

data Evt
    = ButtonClicked
    | MenuClicked
    | TextEdited
    | OptionSelected
    | WidgetUsed
    | ErrorDisplayed
    | ProgramCrashed
    | MyCpuUsage
    | CpuUtilization
    | MyMemUsage
    | MemUtilization
    | DiskSpaceUsed
    | DiskSpaceAvailable
    deriving (Eq, Show, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)
instance PersistField Evt where
    toPersistValue = toPersistValue . fromEnum
    fromPersistValue = fmap toEnum . fromPersistValue
instance PersistFieldSql Evt where
    sqlType _ = SqlInt32

data TimeWindow
    = Seconds Int
    | Minutes Int
    | Hours Int
    | Days Int
    | Weeks Int
    | Months Int
    | Years Int
    | TimeWindowSum TimeWindow TimeWindow
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

data QueryType
    = TopUsedFeatures Int
    | LeastUsedFeatures Int
    | Counts TimeWindow
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Query = Query
    { queryType :: QueryType
    , limitToEvents :: Maybe [Evt]
    , excludeEvents :: Maybe [Evt]
    , limitToSources :: Maybe [String]
    , excludeSources :: Maybe [String]
    , fromDate :: Maybe UTCTime
    , toDate :: Maybe UTCTime
    } deriving (Eq, Show, Generic)

defQuery :: Query
defQuery = Query (TopUsedFeatures 0) Nothing Nothing Nothing Nothing Nothing Nothing

instance FromJSON Query
instance ToJSON Query where
    toEncoding = genericToEncoding $ defaultOptions { omitNothingFields = True }

data NewAppError
    = NewAppError String
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

data UpdateError
    = UpdateError String
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

type AppID = Int64
type UpdateID = Int64

data QueryError
    = QueryError String
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

data QueryResults
    = UsedFeaturesResult [(Evt, String, Int)]
    | CountsResult [(UTCTime, [(Evt, String, Int)])]
    deriving (Eq, Show, Generic, ToJSON, FromJSON)


data NewAppRequest = NewAppRequest
    { appName :: String
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data NewAppResponse = NewAppResponse
    { newAppID :: Either NewAppError AppID
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

newAppRequestFailed :: String -> NewAppResponse
newAppRequestFailed = NewAppResponse . Left . NewAppError

newAppRequestSucceeded :: AppID -> NewAppResponse
newAppRequestSucceeded = NewAppResponse . Right

data UpdateRequest = UpdateRequest
    { eventName :: String
    , eventType :: Evt
    , eventValue :: String
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data UpdateResponse = UpdateResponse
    { updateID :: Either UpdateError UpdateID
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

updateRequestFailed :: String -> UpdateResponse
updateRequestFailed = UpdateResponse . Left . UpdateError

updateRequestSucceeded :: UpdateID -> UpdateResponse
updateRequestSucceeded = UpdateResponse . Right

data QueryRequest = QueryRequest
    { query :: Query
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data QueryResponse = QueryResponse
    { queryResults :: Either QueryError QueryResults
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

queryRequestFailed :: String -> QueryResponse
queryRequestFailed = QueryResponse . Left . QueryError

queryRequestSucceeded :: QueryResults -> QueryResponse
queryRequestSucceeded = QueryResponse . Right
