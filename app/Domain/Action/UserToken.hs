{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}


module Domain.Action.UserToken where

import Prelude hiding (id)
import Domain.Types.UserToken
import Storage.Queries.UserToken as UT
import Data.UUID.V4
import Data.UUID
import Data.Time
import Data.Text
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Error.Class (throwError, MonadError)
import Servant.Server (ServerError, err500, errBody)
import Data.Aeson (encode)

generateToken :: (MonadIO m, MonadError ServerError m) => Text -> m Text
generateToken userId = do
    userData <- UT.getTokenByUserId userId
    case userData of
        Just user -> return $ token user
        Nothing  -> do
            userTokenData <- makeUserToken userId
            UT.createUserToken userTokenData
            return $ token userTokenData


makeUserToken :: (MonadIO m, MonadError ServerError m) => Text -> m UserToken
makeUserToken userId = do
    tokenId <- liftIO $ toText <$> nextRandom
    token <- liftIO $ toText <$> nextRandom
    createdAt <- liftIO getCurrentTime
    updatedAt <- liftIO getCurrentTime
    return $ UserToken {
        id = tokenId,
        userId = userId,
        token = token,
        createdAt = createdAt,
        updatedAt = updatedAt
    }


