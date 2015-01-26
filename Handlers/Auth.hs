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

getLogin :: Handler Value
getLogin = jsonResult $ do
    user <- getUser
    forM user $ runDB . getUserName

postLogin :: Handler Value
postLogin = jsonResult $ do
    (username, password) <- runInputPost loginForm
    mbId <- runDB $ findUser username password
    case mbId of
      Nothing -> fail "Invalid username or password."
      Just uid -> setUser (Just uid)
  where
    loginForm = (,)
        <$> ireq usernameField "username"
        <*> ireq passwordField "password"

postLogout :: Handler Value
postLogout = jsonResult $ do
    _ <- requireUser
    setUser Nothing

postRegister :: Handler Value
postRegister = jsonResult $ do
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

postDelete :: Handler Value
postDelete = jsonResult $ do
    user <- requireUser
    runDB $ do
        dropSchema user
        deleteUser user
