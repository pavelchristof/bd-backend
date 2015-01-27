{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sql.User where

import Imports

import qualified Data.Text as T

-- | Replaces all dollars with the users schema name.
sqlForUser :: UserId -> Text -> Text
sqlForUser uid sql = T.replace "$." schemaName sql
  where
    schemaName = "user_" <> pack (show uid) <> "_"

-- | SQL queries which depend on currently logged in user.
newtype UserSQL a = UserSQL (ReaderT UserId SQL a)
    deriving (Functor, Applicative, Monad, MonadIO)

instance MonadSQL UserSQL where
    queryMaybeSingle query params = UserSQL $ ReaderT $ \uid ->
        queryMaybeSingle (sqlForUser uid query) params
    querySingle query params = UserSQL $ ReaderT $ \uid ->
        querySingle (sqlForUser uid query) params
    queryMany query params = UserSQL $ ReaderT $ \uid ->
        queryMany (sqlForUser uid query) params
    execStmt query params = UserSQL $ ReaderT $ \uid ->
        execStmt (sqlForUser uid query) params

-- | Executes queries in context of the given user.
runUserSQL :: UserId -> UserSQL a -> Handler a
runUserSQL uid (UserSQL m) = runSQL $ runReaderT m uid
