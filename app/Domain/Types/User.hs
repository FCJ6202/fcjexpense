{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Domain.Types.User where

import Prelude hiding (id)
import Data.Aeson
import Data.Text
import Data.Time
import GHC.Generics
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField (toField)

data User = User {
    id :: Text,
    name :: Text,
    age :: Int,
    email :: Text,
    password :: Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
} deriving (Show, Eq, Generic)

-- ToJSON instance - converts User to JSON
instance ToJSON User where
    toJSON user = object [
        "id" .= id user,
        "name" .= name user,
        "age" .= age user,
        "email" .= email user,
        "password" .= password user,
        "created_at" .= createdAt user,
        "updated_at" .= updatedAt user
        ]

-- FromJSON instance - parses JSON to User
instance FromJSON User where
    parseJSON = withObject "User" $ \v -> User
        <$> v .: "id"
        <*> v .: "name"
        <*> v .: "age"
        <*> v .: "email"
        <*> v .: "password"
        <*> v .: "created_at"
        <*> v .: "updated_at"

instance FromRow User where
    fromRow = User
        <$> field
        <*> field
        <*> field
        <*> field
        <*> field
        <*> field
        <*> field

instance ToRow User where
    toRow user = [
        toField (id user),
        toField (name user),
        toField (age user),
        toField (email user),
        toField (password user),
        toField (createdAt user),
        toField (updatedAt user)
        ]


data UserResponse = UserResponse {
    userId :: Text,
    token :: Text
} deriving (Show, Eq, Generic)

instance ToJSON UserResponse where
    toJSON userResponse = object [
        "userId" .= userId userResponse,
        "token" .= token userResponse
        ]

instance FromJSON UserResponse where
    parseJSON = withObject "UserResponse" $ \v -> UserResponse
        <$> v .: "userId"
        <*> v .: "token"

data UserSignUpRequest = UserSignUpRequest {
    userName :: Text,
    userAge :: Int,
    userEmail :: Text,
    userPassword :: Text
} deriving (Show, Eq, Generic)

instance ToJSON UserSignUpRequest where
    toJSON userSignUpRequest = object [
        "name" .= userName userSignUpRequest,
        "age" .= userAge userSignUpRequest,
        "email" .= userEmail userSignUpRequest,
        "password" .= userPassword userSignUpRequest
        ]

instance FromJSON UserSignUpRequest where
    parseJSON = withObject "UserSignUpRequest" $ \v -> UserSignUpRequest
        <$> v .: "userName"
        <*> v .: "userAge"
        <*> v .: "userEmail"
        <*> v .: "userPassword"

data UserLoginRequest = UserLoginRequest {
    loginEmail :: Text,
    loginPassword :: Text
} deriving (Show, Eq, Generic, FromJSON)