module Handlers.Auth where

import Imports
import Session.User
import Sql.Auth

import Data.Char

-- Data validation.

validateUsername :: Text -> Bool
validateUsername = all isAlphaNum

usernameField :: Field Handler Text
usernameField = checkBool
    validateUsername
    ("Invalid username." :: Text)
    textField

-- Handlers.

getLoginR :: Handler Value
getLoginR = jsonResult $ do
    user <- getUser
    forM user $ runSQL . getUserName

postLoginR :: Handler Value
postLoginR = jsonResult $ do
    (username, password) <- runInputPost loginForm
    uid <- runSQL (findUser username password)
           `orElseM` fail "Invalid username or password."
    setUser (Just uid)
  where
    loginForm = (,)
        <$> ireq usernameField "username"
        <*> ireq passwordField "password"

postLogoutR :: Handler Value
postLogoutR = jsonResult $ do
    _ <- requireUser
    setUser Nothing

postRegisterR :: Handler Value
postRegisterR = jsonResult $ do
    (username, password) <- runInputPost registerForm
    uid <- runSQL $ do
        alreadyExists <- userWithNameExists username
        when alreadyExists $ fail "Username already exists."
        uid <- createUser username password
        return uid
    setUser (Just uid)
  where
    registerForm = (,)
        <$> ireq usernameField "username"
        <*> ireq passwordField "password"

postDeleteR :: Handler Value
postDeleteR = jsonResult $ do
    user <- requireUser
    runSQL $ deleteUser user
