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
import Database.Persist.Sql

data Evt
    = ButtonClicked
    | MenuClicked
    | TextEdited
    | ErrorDisplayed
    | ProgramCrashed
    deriving (Eq, Show, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)
instance PersistField Evt where
    toPersistValue = toPersistValue . fromEnum
    fromPersistValue = fmap toEnum . fromPersistValue
instance PersistFieldSql Evt where
    sqlType _ = SqlInt32

data Query
    -- Primitives
    = TopUsedFeatures Int
    | LeastUsedFeatures Int
    | RatiosOfFeaturesUsed
    -- Combinators and ways to apply conditions
    | LimitToEventType Evt Query
    | ExcludeEventType Evt Query
    | FromDate UTCTime Query
    | ToDate UTCTime Query
    deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

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
    = QueryResults
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
