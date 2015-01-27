{-# LANGUAGE DeriveGeneric #-}
module Session.User (
    UserId,
    getUser,
    requireUser,
    setUser
    ) where

import Data.Aeson
import Imports
import Sql.Auth

newtype UserData = UserData { userId :: UserId }
    deriving (Generic)

instance ToJSON UserData
instance FromJSON UserData

-- | Get the currently logged in user.
getUser :: Handler (Maybe UserId)
getUser = do
    mbJson <- lookupSessionBS "user"
    let mbUser = mbJson >>= decodeStrict'
    case mbUser of
      Nothing -> return Nothing
      Just (UserData uid) -> do
        -- The user could be deleted, check if it still exists.
        ok <- runSQL $ userWithIdExists uid
        if ok
           then return (Just uid)
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
      Just uid -> setSessionBS "user" $ toStrict $ encode $ UserData uid
