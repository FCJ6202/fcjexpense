{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}


module Domain.Action.User where

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

generateToken :: User -> Text
generateToken _ = "token"

signup :: UserSignUpRequest -> Handler UserSignupResponse
signup userSignUpRequest = do
    user <- liftIO $ makeUser userSignUpRequest
    result <- liftIO $ createUser user
    case result of
        Left err -> throwError $ err500 { errBody = encode err }
        Right user' -> return $ UserSignupResponse (id user') (generateToken user')


makeUser :: UserSignUpRequest -> IO User
makeUser userSignUpRequest = do
    userId <- toText <$> nextRandom
    createdAt <- getCurrentTime
    updatedAt <- getCurrentTime
    return $ User userId (userName userSignUpRequest) (userAge userSignUpRequest) (userEmail userSignUpRequest) (userPassword userSignUpRequest) createdAt updatedAt
