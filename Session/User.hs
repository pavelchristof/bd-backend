{-# LANGUAGE DeriveGeneric #-}
module Session.User (
    UserId,
    getUser,
    requireUser,
    setUser
    ) where

import Imports
import Sql.User
import Data.Aeson

-- | Get the currently logged in user.
getUser :: Handler (Maybe UserId)
getUser = do
    mbJson <- lookupSessionBS "user"
    let mbUser = mbJson >>= decodeStrict'
    case mbUser of
      Nothing -> return Nothing
      Just user -> do
        -- The user could be deleted, check if it still exists.
        ok <- runDB $ userWithIdExists user
        if ok
           then return (Just user)
           else do
             setUser Nothing
             return Nothing

-- | Fails if not logged in.
requireUser :: Handler UserId
requireUser = do
    mbUser <- getUser
    case mbUser of
      Nothing -> fail "Login required."
      Just user -> return user

-- | Set the currently logged in user.
setUser :: Maybe UserId -> Handler ()
setUser mbUser =
    case mbUser of
      Nothing -> deleteSession "user"
      Just user -> setSessionBS "user" $ toStrict $ encode $ user
