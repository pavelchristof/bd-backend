module Handlers.Auth where

import Imports
import Sql.User
import Sql.Schema
import Session.User

import Data.Char

-- Data validation.

validateUsername :: Text -> Bool
validateUsername = all (\c -> isAlphaNum c)

usernameField :: Field Handler Text
usernameField = checkBool
    validateUsername
    ("Invalid username." :: Text)
    textField

-- Handlers.

getLoginR :: Handler Value
getLoginR = jsonResult $ do
    user <- getUser
    forM user $ runDB . getUserName

postLoginR :: Handler Value
postLoginR = jsonResult $ do
    (username, password) <- runInputPost loginForm
    uid <- runDB (findUser username password)
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
    uid <- runDB $ do
        alreadyExists <- userWithNameExists username
        when alreadyExists $ fail "Username already exists."
        uid <- createUser username password
        createSchema uid
        return uid
    setUser (Just uid)
  where
    registerForm = (,)
        <$> ireq usernameField "username"
        <*> ireq passwordField "password"

postDeleteR :: Handler Value
postDeleteR = jsonResult $ do
    user <- requireUser
    runDB $ do
        dropSchema user
        deleteUser user
