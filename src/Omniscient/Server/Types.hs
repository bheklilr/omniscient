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
    , UpdateID
    , QueryError(..)
    , QueryResults(..)
    , Evt(..)
    ) where

import              GHC.Generics

import              Data.Map (Map)
import              Data.Time

import qualified    Data.Text as T
import              Data.Text (Text)
import              Data.Aeson

import              Database.Persist.TH
import              Database.Persist.Sql

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

data NewAppError
    = NewAppError Text
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

data UpdateError
    = UpdateError Text
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

type UpdateID = Int

data QueryError
    = QueryError Text
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

data QueryResults
    = QueryResults
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

data NewAppRequest = NewAppRequest
    { appName :: Text
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data NewAppResponse = NewAppResponse
    { newAppID :: Either NewAppError Int
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)


data UpdateRequest = UpdateRequest
    { eventName :: Text
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data UpdateResponse = UpdateResponse
    { updateID :: Either UpdateError UpdateID
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)


data QueryRequest = QueryRequest
    { query :: ()
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data QueryResponse = QueryResponse
    { queryResults :: Either QueryError QueryResults
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)
