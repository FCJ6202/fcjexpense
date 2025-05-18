{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
module Storage.Queries.UserToken where

import Prelude hiding (id)
import Data.Text hiding (head)
import Data.Time
import Database.SQLite.Simple
import Domain.Types.UserToken
import Control.Exception
import Servant.Server (ServerError, err500, errBody)
import Data.Aeson (encode)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Error.Class (throwError, MonadError)
import Database.SQLite.Simple (SQLError)

createUserToken :: (MonadIO m, MonadError ServerError m) => UserToken -> m ()
createUserToken userToken = do
    conn <- liftIO $ open "expenseDB"
    let sql = "INSERT INTO user_tokens (id, user_id, token, created_at, updated_at) VALUES (?, ?, ?, ?, ?)"
    let params = (id userToken, userId userToken, token userToken, createdAt userToken, updatedAt userToken)
    result <- liftIO $ try @SQLError $ execute conn sql params
    case result of
        Left err -> do
            liftIO $ putStrLn $ "Error creating user token: " <> show err
            throwError $ err500 { errBody = encode ("Error creating user token: " <> show err :: String) }
        Right _  -> return ()


getTokenByUserId :: (MonadIO m, MonadError ServerError m) => Text -> m (Maybe UserToken)
getTokenByUserId userId = do
    conn <- liftIO $ open "expenseDB"
    let sql = "SELECT * FROM user_tokens WHERE user_id = ? ORDER BY created_at DESC LIMIT 1"
    let params = Only userId
    result <- liftIO $ try @SQLError $ query conn sql params
    case result of
        Left err -> do
            liftIO $ putStrLn $ "Error getting token by user id: " <> show err
            throwError $ err500 { errBody = encode ("Error getting token by user id: " <> show err :: String) }
        Right tokens -> case tokens of
            [] -> return Nothing
            (t:_) -> return $ Just t

getUserIdByToken :: (MonadIO m, MonadError ServerError m) => Text -> m (Maybe Text)
getUserIdByToken token = do
    conn <- liftIO $ open "expenseDB"
    let sql = "SELECT * FROM user_tokens WHERE token = ? ORDER BY created_at DESC LIMIT 1"
    let params = Only token
    result <- liftIO $ try @SQLError $ query conn sql params
    case result of
        Left err -> do
            liftIO $ putStrLn $ "Error getting user id by token: " <> show err
            throwError $ err500 { errBody = encode ("Error getting user id by token: " <> show err :: String) }
        Right tokens -> case tokens of
            [] -> return Nothing
            (t:_) -> return $ Just (userId t)
