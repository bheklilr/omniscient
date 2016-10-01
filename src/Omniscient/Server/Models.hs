{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Omniscient.Server.Models where

import           Data.Time
import           Database.Persist.TH

import           Omniscient.Server.Types

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
App
    appName String
    createdBy String
    UniqueApp appName
    deriving Show

Event
    eventApp    AppId
    eventName   String
    eventType   Evt
    eventValue  String
    eventSource String
    eventTime   UTCTime default=CURRENT_TIME
    UniqueEvent eventApp eventName eventTime eventSource
    deriving Show
|]
