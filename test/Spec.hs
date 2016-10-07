{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
import Omniscient.Server
import Omniscient.Server.Types
-- import Database.Persist
import Database.Persist.Sql (ConnectionPool)
import Database.Persist.Sqlite (createSqlitePool)
import Database.Esqueleto
import Control.Monad.Reader
import Control.Monad.Logger
import Network.Socket
import Data.Either
import Data.Function
import Data.Time

import Test.Hspec

host :: SockAddr
host = SockAddrInet 1234 $ tupleToHostAddress (192, 168, 1, 100)

requestNewApp name = newAppHandler (NewAppRequest name) host

setupConnection :: IO ConnectionPool
setupConnection = do
    pool <- runStderrLoggingT $ createSqlitePool "test.db" 5
    runSqlPool (runMigration migrateAll) pool
    -- Empty the database
    flip runSqlPool pool $ do
        delete $ from $ \(event :: SqlExpr (Entity Event)) -> return ()
        delete $ from $ \(app   :: SqlExpr (Entity App))   -> return ()
    return pool

runTest test = join $ runIO $ do
    pool <- setupConnection
    runReaderT (runStderrLoggingT test) pool

testNewAppHandler :: Spec
testNewAppHandler = runTest $ do
    (NewAppResponse (Right appID1)) <- requestNewApp "test1"
    (NewAppResponse (Right appID2)) <- requestNewApp "test2"
    (NewAppResponse (Right appID3)) <- requestNewApp "test1"
    (NewAppResponse (Right appID4)) <- requestNewApp "test2"
    return $ describe "testNewAppHandler" $ do
        it "appID1 should be 1" $ appID1 `shouldBe` 1
        it "appID2 should be 2" $ appID2 `shouldBe` 2
        it "appID3 should be appID1" $ appID3 `shouldBe` appID1
        it "appID4 should be appID2" $ appID4 `shouldBe` appID2

testUpdateHandler :: Spec
testUpdateHandler = undefined

testGetTopUsedFeatures :: Spec
testGetTopUsedFeatures = runTest $ do
    startTime <- liftIO $ getCurrentTime
    (NewAppResponse (Right appID)) <- requestNewApp "test"
    let button name value = UpdateRequest name ButtonClicked value
        sendButton name value = updateHandler appID (button name value) host
    replicateM 1 $ sendButton "a" "1"
    replicateM 2 $ sendButton "b" "1"
    replicateM 3 $ sendButton "c" "1"
    let getTopUsed i = queryHandler appID $ QueryRequest $
            Query (TopUsedFeatures i)
            Nothing Nothing Nothing Nothing
            (Just startTime) Nothing
    (QueryResponse (Right a)) <- getTopUsed 1
    (QueryResponse (Right b)) <- getTopUsed 2
    (QueryResponse (Right c)) <- getTopUsed 3
    let clicked name cnt = (ButtonClicked, name, cnt)
    return $ describe "testGetTopUsedFeatures" $ do
        it "a should be ButtonClicked \"c\" 3" $
            a `shouldBe` UsedFeaturesResult [clicked "c" 3]
        it "b should be ButtonClicked \"c\" 3, ButtonClicked \"b\" 2" $
            b `shouldBe` UsedFeaturesResult [clicked "c" 3, clicked "b" 2]
        it "c should be ButtonClicked \"c\" 3, ButtonClicked \"b\" 2, ButtonClicked, \"a\" 1" $
            c `shouldBe` UsedFeaturesResult [clicked "c" 3, clicked "b" 2, clicked "a" 1]


main :: IO ()
main = hspec $ do
    testNewAppHandler
    testGetTopUsedFeatures
