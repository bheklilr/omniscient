{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
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

import Data.Int
import Data.Function
import Data.Time
import Data.Maybe
import Data.List (group)

import Control.Monad.Reader
import Control.Monad.Logger
import Control.Arrow ((&&&))

import Database.Persist.Sql hiding ((==.), (<.), (>.))
import Database.Esqueleto

import Network.Socket
import Network.Wai.Handler.Warp as Warp

import Servant

import Omniscient.Server.Types
import Omniscient.Server.Models
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
instance MonadIO m => Omni (OmniscientT m)

db :: Omni app => _ -> app a
db = (ask >>=) . liftSqlPersistMPool

newAppHandler :: Omni app => NewAppRequest -> SockAddr -> app NewAppResponse
newAppHandler request host = do
    $logDebug "Checking if application already exists"
    existingEntity <- db.getBy $ UniqueApp (request & appName)
    case existingEntity of
        Just appID -> do
            $logDebug "Application already exists, returning existing appID"
            return $ newAppRequestSucceeded $ fromSqlKey $ entityKey appID
        Nothing -> do
            $logDebug "Attempting to set up new application"
            appID <- db.insert $ App (request & appName) (show host)
            $logDebug "New application set up"
            return $ newAppRequestSucceeded $ fromSqlKey appID


updateHandler :: Omni app => Int64 -> UpdateRequest -> SockAddr -> app UpdateResponse
updateHandler appID request host = do
    let appKey = toSqlKey appID
    existingEntity <- db.get $ appKey
    case existingEntity of
        Nothing -> do
            $logDebug "Received event update without matching app ID"
            return $ updateRequestFailed $ "No app with ID " ++ show appID
        Just _  -> do
            $logDebug "Inserting event"
            event <- Event
                appKey
                (request & eventName)
                (request & eventType)
                (request & eventValue)
                (show host)
                <$> liftIO getCurrentTime
            updateID' <- db.insert $ event
            $logDebug "Inserted event successfully"
            return $ updateRequestSucceeded $ fromSqlKey updateID'

queryHandler :: Omni app => Int64 -> QueryRequest -> app QueryResponse
queryHandler appID request = do
    let appKey = toSqlKey appID :: Key App
    existingEntity <- db.get $ appKey
    case existingEntity of
        Nothing -> do
            $logDebug "Received query request without matching app ID"
            return $ queryRequestFailed $ "No app with ID " ++ show appID
        Just _ -> do
            $logDebug "Performing query"
            let q = request & query
            result <- case q & queryType of
                TopUsedFeatures lim -> getTopUsedFeatures appKey q lim
                LeastUsedFeatures lim -> getLeastUsedFeatures appKey q lim
                Counts timeWindow -> getCounts appKey q timeWindow
            $logDebug "Query performed"
            return result
    where
        getTopUsedFeatures = getUsedFeatures desc

        getLeastUsedFeatures = getUsedFeatures asc

        applyQueryFilters appKey q event = do
            where_ (event ^. EventEventApp ==. val appKey)
            when (isJust $ q & fromDate) $
                where_ (event ^. EventEventTime >. val (fromJust (q & fromDate)))
            when (isJust $ q & toDate) $
                where_ (event ^. EventEventTime <. val (fromJust (q & toDate)))
            let vlMaybe field = valList . fromMaybe [] . field
            where_ (event ^. EventEventType   `in_`   vlMaybe limitToEvents  q)
            where_ (event ^. EventEventType   `notIn` vlMaybe excludeEvents  q)
            where_ (event ^. EventEventSource `in_`   vlMaybe limitToSources q)
            where_ (event ^. EventEventSource `notIn` vlMaybe excludeSources q)

        getUsedFeatures sortBy appKey q lim = do
            values <- db $ select $ from $ \event -> do
                applyQueryFilters appKey q event
                groupBy (event ^. EventEventType, event ^. EventEventName)
                let countRows' = countRows
                orderBy [sortBy countRows']
                limit $ fromIntegral lim
                return (event ^. EventEventType, event ^. EventEventName, countRows')
            asList <- forM values $ \(Value evt, Value name, Value cnt) -> return (evt, name, cnt :: Int)
            return $ queryRequestSucceeded $ UsedFeaturesResult asList

        getCounts appKey q timeWindow = do
            now <- liftIO getCurrentTime
            values <- db $ select $ from $ \event -> do
                applyQueryFilters appKey q event
                orderBy [asc $ event ^. EventEventTime, asc $ event ^. EventEventType, asc $ event ^. EventEventName]
                return (event ^. EventEventType, event ^. EventEventName, event ^. EventEventTime)
            asList <- forM values $ \(Value evt, Value name, Value time) -> return (evt, name, time)
            let timeWindowDiff = negate $ fromInteger $ toInteger $ toSeconds timeWindow
                groupedByTime = go now asList
                go _ [] = []
                go t events =
                    let inWindow = takeWhile ((< t) . (\(_, _, time) -> time)) events
                        outsideOfWindow = drop (length inWindow) events
                        timesStripped = map (\(evt, name, _) -> (evt, name)) inWindow
                        groupedByEvt = group timesStripped
                        cnts = map (head &&& length) groupedByEvt
                        squashedCnts = map (\((evt, name), cnt) -> (evt, name, cnt)) cnts
                    in (t, squashedCnts) : go (addUTCTime timeWindowDiff t) outsideOfWindow

                toSeconds (Seconds i) = i
                toSeconds (Minutes i) = i *  60
                toSeconds (Hours i)   = i *  60 * toSeconds (Minutes 1)
                toSeconds (Days i)    = i *  24 * toSeconds (Hours 1)
                toSeconds (Weeks i)   = i *   7 * toSeconds (Days 1)
                toSeconds (Months i)  = i *  30 * toSeconds (Days 1)
                toSeconds (Years i)   = i * 365 * toSeconds (Days 1)
                toSeconds (TimeWindowSum a b) = toSeconds a + toSeconds b
            return $ queryRequestSucceeded $ CountsResult groupedByTime
