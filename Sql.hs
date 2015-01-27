{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sql where

import ClassyPrelude.Yesod
import Data.FileEmbed
import Database.Persist.Sql
import Foundation
import Utils

-- | Creates the database.
createDB :: MonadIO m => ReaderT SqlBackend m ()
createDB = rawExecute sql []
  where
    sql = decodeUtf8 $(embedFile "sql/create_db.sql")

-- | Types that can be read from an Sql row.
class RawSql (RawSqlType a) => SqlRow a where
    type RawSqlType a :: *
    type RawSqlType a = Single a

    fromRawSql :: RawSqlType a -> a
    default fromRawSql :: RawSqlType a ~ Single a => RawSqlType a -> a
    fromRawSql = unSingle

instance SqlRow Int
instance SqlRow Bool
instance SqlRow Text

instance (SqlRow a, SqlRow b) => SqlRow (a, b) where
    type RawSqlType (a, b) = (RawSqlType a, RawSqlType b)
    fromRawSql (a, b) = (fromRawSql a, fromRawSql b)

instance (SqlRow a, SqlRow b, SqlRow c) => SqlRow (a, b, c) where
    type RawSqlType (a, b, c) = (RawSqlType a, RawSqlType b, RawSqlType c)
    fromRawSql (a, b, c) = (fromRawSql a, fromRawSql b, fromRawSql c)

-- | Monads which can execute SQL queries.
class MonadIO m => MonadSQL m where
    -- | Runs a query that returns a single value or nothing.
    queryMaybeSingle :: SqlRow a => Text -> [PersistValue] -> m (Maybe a)

    -- | Runs a query that returns a single value.
    querySingle :: SqlRow a => Text -> [PersistValue] -> m a

    -- | Runs a query that returns a list of values.
    queryMany :: SqlRow a => Text -> [PersistValue] -> m [a]

    -- | Executes an SQL statement.
    execStmt :: Text -> [PersistValue] -> m ()

-- | A normal SQL query.
newtype SQL a = SQL (ReaderT SqlBackend Handler a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader SqlBackend)

instance MonadSQL SQL where
    queryMaybeSingle query params = do
        result <- SQL $ rawSql query params
        return (fmap fromRawSql $ listToMaybe result)

    querySingle query params =
        queryMaybeSingle query params `orElseM` fail "Database query error."

    queryMany query params = do
        results <- SQL $ rawSql query params
        return $ map fromRawSql results

    execStmt query params = SQL $ rawExecute query params

runSQL :: SQL a -> Handler a
runSQL (SQL m) = runDB m
