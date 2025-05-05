{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.User where

import Prelude hiding (id)
import Control.Exception
import Domain.Types.User
import Database.SQLite.Simple
import Data.Text hiding (head)
import Database.SQLite.Simple (Only(..))
import Servant.Server (ServerError, err500, errBody)
import Data.Aeson (encode)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Error.Class (throwError, MonadError)
import Database.SQLite.Simple (SQLError)



-- Create a new user
createUser :: (MonadIO m, MonadError ServerError m) => User -> m ()
createUser user = do
    conn <- liftIO $ open "expenseDB"
    let sql = "INSERT INTO users (id, name, age, email, password, created_at, updated_at) VALUES (?, ?, ?, ?, ?, ?, ?)"
    let params = (id user, name user, age user, email user, password user, createdAt user, updatedAt user)
    result <- liftIO $ try @SQLError $ execute conn sql params
    case result of
        Left err -> do
            liftIO $ putStrLn $ "Error creating user: " <> show err
            throwError $ err500 { errBody = encode ("Error creating user: " <> show err :: String) }
        Right _  -> return ()

getUserById :: (MonadIO m, MonadError ServerError m) => Text -> m (Maybe User)
getUserById userId = do -- We must have to verify this is valid user which can be done vai newtype Id with domain
    conn <- liftIO $ open "expenseDB"
    let sql = "SELECT * FROM users WHERE id = ?"
    let params = Only userId
    result <- liftIO $ try @SQLError $ query conn sql params
    case result of
        Left err -> do
            liftIO $ putStrLn $ "Error getting user by id: " <> show err
            throwError $ err500 { errBody = encode ("Error getting user by id: " <> show err :: String) }
        Right users -> return $ Just (head users)

removeUser :: (MonadIO m, MonadError ServerError m) => Text -> m ()
removeUser userId = do -- We must have to verify this is valid user which can be done vai newtype Id with domain
    conn <- liftIO $ open "expenseDB"
    let sql = "DELETE FROM users WHERE id = ?"
    let params = Only userId
    result <- liftIO $ try @SQLError $ execute conn sql params
    case result of
        Left err -> do
            liftIO $ putStrLn $ "Error removing user: " <> show err
            throwError $ err500 { errBody = encode ("Error removing user: " <> show err :: String) }
        Right _  -> return ()
