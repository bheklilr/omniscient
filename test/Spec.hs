{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
import Omniscient.Server
import Omniscient.Server.Types
import Database.Persist
import Database.Persist.Sql (ConnectionPool)
import Database.Persist.Sqlite
import Control.Monad.Reader
import Control.Monad.Logger
import Network.Socket
import Data.Either
import Data.Function
import Data.Time

import Test.Hspec

setupPool :: IO ConnectionPool
setupPool = do
    pool <- runStderrLoggingT $ createSqlitePool "test.db" 5
    runSqlPool (runMigration migrateAll) pool
    return pool

runTest test = runIO $ do
    pool <- setupPool
    runReaderT (runStderrLoggingT test) pool

testNewAppHandler = undefined

testUpdateHandler = undefined

testQueryHandler :: Spec
testQueryHandler = do
    (a, b, c) <- runTest $ do
        startTime <- liftIO $ getCurrentTime
        let host = SockAddrInet 1234 $ tupleToHostAddress (192, 168, 1, 100)
        (NewAppResponse (Right appID)) <- newAppHandler (NewAppRequest "test") host
        replicateM 10 $ updateHandler appID (UpdateRequest "a" ButtonClicked "") host
        replicateM 20 $ updateHandler appID (UpdateRequest "b" ButtonClicked "") host
        replicateM 30 $ updateHandler appID (UpdateRequest "c" ButtonClicked "") host
        (QueryResponse (Right a)) <-
            queryHandler appID $ QueryRequest $
                Query (TopUsedFeatures 1)
                    Nothing Nothing
                    Nothing Nothing
                    (Just startTime) Nothing
        (QueryResponse (Right b)) <-
            queryHandler appID $ QueryRequest $
                Query (TopUsedFeatures 2)
                    Nothing Nothing
                    Nothing Nothing
                    (Just startTime) Nothing
        (QueryResponse (Right c)) <-
            queryHandler appID $ QueryRequest $
                Query (TopUsedFeatures 3)
                    Nothing Nothing
                    Nothing Nothing
                    (Just startTime) Nothing
        return (a, b, c)
    describe "testQueryHandler" $ do
        it "a should be ButtonClicked \"c\" 30" $
            a `shouldBe` UsedFeaturesResult [(ButtonClicked, "c", 30)]
        it "b should be ButtonClicked \"c\" 30, ButtonClicked \"b\" 20" $
            b `shouldBe` UsedFeaturesResult [(ButtonClicked, "c", 30), (ButtonClicked, "b", 20)]
        it "c should be ButtonClicked \"c\" 30, ButtonClicked \"b\" 20, ButtonClicked, \"a\" 10" $
            c `shouldBe` UsedFeaturesResult [(ButtonClicked, "c", 30), (ButtonClicked, "b", 20), (ButtonClicked, "a", 10)]


main :: IO ()
main = hspec $ do
    testQueryHandler
