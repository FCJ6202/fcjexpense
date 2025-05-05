{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}


module Domain.Action.User where

import Domain.Action.UserToken
import Prelude hiding (id)
import Data.Text
import Servant
import Domain.Types.User
import Storage.Queries.User
import Data.Aeson
import Control.Monad.IO.Class (liftIO)
import Servant.Server (err500)
import Control.Monad.Error.Class (throwError)
import Data.Aeson (encode)
import Data.UUID.V4
import Data.UUID
import Data.Time


type UserAPI = "user" :> "signup" :> ReqBody '[JSON] UserSignUpRequest :> Post '[JSON] UserSignupResponse

handler :: Server UserAPI
handler = signup

signup :: UserSignUpRequest -> Handler UserSignupResponse
signup userSignUpRequest = do
    user <- liftIO $ makeUser userSignUpRequest
    createUser user
    tokenResult <- generateToken (id user)
    case tokenResult of
        Nothing -> do
            removeUser (id user)
            throwError $ err500 { errBody = encode ("An error occurred while generating token" :: String) }
        Just token -> return $ UserSignupResponse {
                                userId = id user,
                                token = token
                            }


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
