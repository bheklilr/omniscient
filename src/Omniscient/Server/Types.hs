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
    , Query(..)
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
import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Maybe

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
    -- Combinators and ways to apply conditions
    | LimitToEventType Evt Query
    | ExcludeEventType Evt Query
    | LimitToSource String Query
    | ExcludeSource String Query
    | FromDate UTCTime Query
    | ToDate UTCTime Query
    deriving (Eq, Show, Ord)

instance FromJSON Query where
    parseJSON (Object v) = do
        primitive <- parsePrimitive v
        limit_to_events <- parseLimitToEvents v
        exclude_events <- parseExcludeEvents v
        limit_to_sources <- parseLimitToSources v
        exclude_sources <- parseExcludeSources v
        from_date <- v .:? "from" >>= return . fromMaybe id . fmap FromDate
        to_date   <- v .:? "to"   >>= return . fromMaybe id . fmap ToDate
        return
            $ to_date $ from_date
            $ exclude_sources $ limit_to_sources
            $ exclude_events $ limit_to_events
            $ primitive
        where
            parsePrimitive obj = do
                top_used'   <- obj .:? "top_used"
                least_used' <- obj .:? "least_used"
                let top_used   = Alt $ TopUsedFeatures   <$> top_used'
                    least_used = Alt $ LeastUsedFeatures <$> least_used'
                case getAlt $ top_used <|> least_used of
                    Nothing -> mzero
                    Just primitive -> return primitive
            parseList key constructor obj = do
                values <- fromMaybe [] <$> obj .:? key
                return $ appEndo $ mconcat $ map (Endo . constructor) values
            parseLimitToEvents  = parseList "limit_to_events"  LimitToEventType
            parseExcludeEvents  = parseList "exclude_events"   ExcludeEventType
            parseLimitToSources = parseList "limit_to_sources" LimitToSource
            parseExcludeSources = parseList "exclude_sources"  ExcludeSource

    parseJSON _ = mzero

instance ToJSON Query where
    toJSON = undefined

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
