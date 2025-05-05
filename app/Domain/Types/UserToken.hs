{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.Types.UserToken where

import Prelude hiding (id)
import Data.Aeson
import Data.Text
import Data.Time
import GHC.Generics
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField (toField)

data UserToken = UserToken {
    id :: Text,
    userId :: Text,
    token :: Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
} deriving (Show, Eq, Generic)

instance ToJSON UserToken where
    toJSON userToken = object [
        "id" .= id userToken,
        "userId" .= userId userToken,
        "token" .= token userToken,
        "createdAt" .= createdAt userToken,
        "updatedAt" .= updatedAt userToken
        ]

instance FromJSON UserToken where
    parseJSON = withObject "UserToken" $ \v -> UserToken
        <$> v .: "id"
        <*> v .: "userId"
        <*> v .: "token"
        <*> v .: "createdAt"
        <*> v .: "updatedAt"

instance ToRow UserToken where
    toRow userToken = [
        toField (id userToken),
        toField (userId userToken),
        toField (token userToken),
        toField (createdAt userToken),
        toField (updatedAt userToken)
        ]

instance FromRow UserToken where
    fromRow = UserToken
        <$> field
        <*> field
        <*> field
        <*> field
        <*> field
