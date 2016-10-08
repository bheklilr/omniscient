{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Omniscient.Server.Frontend where

import Data.Monoid

import Control.Monad

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Database.Persist.Sql hiding ((==.), (<.), (>.))
import Database.Esqueleto as E

import Omniscient.Server.Models
import Omniscient.Server.Core
import Omniscient.Server.Backend
import Omniscient.Server.Types
import Omniscient.Server.API


frontend
    ::   Omni app
    =>   (app Html)
    :<|> (String -> app Html)
frontend = home :<|> view

home :: Omni app => app Html
home = do
    apps <- db $ E.select $ from $ \(app :: SqlExpr (Entity App)) -> do
        return (app ^. AppProgramName)
    return $ docTypeHtml $ do
        header $ H.title $ toHtml "Omniscient"
        body $ do
            h1 $ toHtml "Apps"
            ul $ forM_ apps $ \(Value appName) ->
                a ! href (stringValue $ "./view/" <> appName) $ li $ toHtml appName

view :: Omni app => String -> app Html
view appName = do
    maybeAppEntity <- db.getBy $ UniqueApp appName
    case maybeAppEntity of
        Nothing -> throwError err404
        Just appEntity -> do
            let appId = fromSqlKey $ entityKey appEntity
            (QueryResponse (Right (UsedFeaturesResult topFeatures))) <-
                queryHandler appId $ QueryRequest $ defQuery { queryType = TopUsedFeatures 10 }
            (QueryResponse (Right (UsedFeaturesResult leastFeatures))) <-
                queryHandler appId $ QueryRequest $ defQuery { queryType = LeastUsedFeatures 10 }
            let featureListAsTable caption_ features = do
                    table $ do
                        caption $ toHtml caption_
                        tr $ do
                            th $ toHtml "Event Type"
                            th $ toHtml "Event Name"
                            th $ toHtml "Count"
                        forM_ features $ \(evt, name, cnt) -> do
                            tr $ do
                                td $ toHtml $ show evt
                                td $ toHtml name
                                td $ toHtml $ show cnt
            return $ docTypeHtml $ do
                header $ H.title $ toHtml $ "Omniscient - " <> appName
                body $ do
                    h1 $ toHtml appName
                    featureListAsTable "Top 10 features" topFeatures
                    br
                    featureListAsTable "Bottom 10 features" leastFeatures
