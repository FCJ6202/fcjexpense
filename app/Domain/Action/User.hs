{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}


module Domain.Action.User where

import Domain.Action.UserToken
import Prelude hiding (id)
import Data.Text
import Servant
import Domain.Types.User
import Storage.Queries.User as QUser
import Storage.Queries.UserToken as QUserToken
import Data.Aeson
import Control.Monad.IO.Class (liftIO, MonadIO)
import Servant.Server (ServerError, err401, errBody)
import Control.Monad.Error.Class (throwError, MonadError)
import Data.Aeson (encode)
import Data.UUID.V4
import Data.UUID
import Data.Time


type UserAPI = "user" :> 
    (
        "signup" :> ReqBody '[JSON] UserSignUpRequest :> Post '[JSON] UserResponse
        :<|> "login" :> ReqBody '[JSON] UserLoginRequest :> Post '[JSON] UserResponse
        :<|> "details" :> Header' '[Required] "Authorization" Text :> Get '[JSON] User
    )

handler :: Server UserAPI
handler = signup :<|> login :<|> userDetails

signup :: UserSignUpRequest -> Handler UserResponse
signup userSignUpRequest = do
    user <- liftIO $ makeUser userSignUpRequest
    QUser.createUser user
    tokenResult <- generateToken (id user)
    return $ UserResponse {
        userId = id user,
        token = tokenResult
    }

login :: UserLoginRequest -> Handler UserResponse
login userLoginRequest = do
    user <- QUser.getUserByEmail (loginEmail userLoginRequest) >>= fromMaybeM (throwError $ err401 { errBody = encode ("Invalid email or password" :: String) })
    let isPasswordValid = verifyPassword (loginPassword userLoginRequest) (password user)
    let isPasswordValid = verifyPassword (loginPassword userLoginRequest) (password user)
    if not isPasswordValid then
        throwError $ err401 { errBody = encode ("Invalid email or password" :: String) }
    else do
        tokenResult <- generateToken (id user)
        return $ UserResponse {
            userId = id user,
            token = tokenResult
        }

userDetails :: Text -> Handler User
userDetails token = do
    userId <- QUserToken.getUserIdByToken token >>= fromMaybeM (throwError $ err401 { errBody = encode ("Invalid token" :: String) })
    user <- QUser.getUserById userId >>= fromMaybeM (throwError $ err401 { errBody = encode ("Invalid token" :: String) })
    return user

makeUser :: UserSignUpRequest -> IO User
makeUser userSignUpRequest = do
    userId <- toText <$> nextRandom
    createdAt <- getCurrentTime
    updatedAt <- getCurrentTime
    return $ User{
        id = userId,
        name = userName userSignUpRequest,
        age = userAge userSignUpRequest,
        email = userEmail userSignUpRequest,
        password = userPassword userSignUpRequest,
        createdAt = createdAt,
        updatedAt = updatedAt
    }

verifyPassword :: Text -> Text -> Bool
verifyPassword userPassword userPasswordFromDB = userPassword == userPasswordFromDB


fromMaybeM :: (MonadIO m, MonadError ServerError m) => m a -> Maybe a -> m a
fromMaybeM errorValue maybeValue = do
    case maybeValue of
        Nothing -> errorValue
        Just x -> return x
