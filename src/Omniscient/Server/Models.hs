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
module Omniscient.Server.Models where

import              GHC.Generics

import              Data.Map (Map)
import              Data.Time

import qualified    Data.Text as T
import              Data.Text (Text)
import              Data.Aeson

import Database.Persist.TH
import Database.Persist.Sql

import Omniscient.Server.Types

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
App
    appID   Int
    appName Text
    deriving Eq Show Generic

Event
    eventApp    AppId
    eventName   Text
    eventType   Evt
    eventValue  Text
    eventTime   UTCTime default=CURRENT_TIME
    eventOrigin Text
    deriving Eq Show Generic
|]
