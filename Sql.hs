{-# LANGUAGE DefaultSignatures #-}
module Sql where

import ClassyPrelude.Yesod
import Database.Persist.Sql
import Utils

-- | A composable SQL query.
type SQL a = (MonadIO m, Functor m) => ReaderT SqlBackend m a

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

-- | Runs a query that returns a single value or nothing.
queryMaybeSingle :: forall a. SqlRow a => Text -> [PersistValue] -> SQL (Maybe a)
queryMaybeSingle query params = do
    (result :: [RawSqlType a]) <- rawSql query params
    return (fmap fromRawSql $ listToMaybe result)

-- | Runs a query that returns a single value.
querySingle :: forall a. SqlRow a => Text -> [PersistValue] -> SQL a
querySingle query params =
    queryMaybeSingle query params `orElseM` fail "Database query error."

-- | Runs a query that returns a list of values.
queryMany :: forall a. SqlRow a => Text -> [PersistValue] -> SQL [a]
queryMany query params = do
    (results :: [RawSqlType a]) <- rawSql query params
    return $ map fromRawSql results
