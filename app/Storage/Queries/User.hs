{-# LANGUAGE OverloadedStrings #-}

module Storage.Queries.User where

import Prelude hiding (id)
import Control.Exception
import Domain.Types.User
import Database.SQLite.Simple



-- Create a new user
createUser :: User -> IO (Either String User)
createUser user = do
    conn <- open "expenseDB"
    let sql = "INSERT INTO users (id, name, age, email, password, created_at, updated_at) VALUES (?, ?, ?, ?, ?, ?, ?)"
    let params = (id user, name user, age user, email user, password user, createdAt user, updatedAt user)
    result <- try $ execute conn sql params :: IO (Either SQLError ())
    case result of
        Left err -> return $ Left (show err)
        Right _  -> return $ Right user

