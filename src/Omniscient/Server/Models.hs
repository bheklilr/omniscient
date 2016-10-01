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
    appID   Int
    appName String
    createdBy String
    deriving Show

Event
    eventApp    AppId
    eventName   String
    eventType   Evt
    eventValue  String
    eventSource String
    eventTime   UTCTime default=CURRENT_TIME
    eventOrigin String
    deriving Show
|]
