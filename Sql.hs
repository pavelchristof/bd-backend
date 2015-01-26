module Sql where

import ClassyPrelude.Yesod
import Database.Persist.Sql

-- | A composable SQL query.
type SQL a = (MonadIO m, Functor m) => ReaderT SqlBackend m a

-- | Runs a query that returns a single value or nothing.
queryMaybeSingle :: forall a. PersistField a => Text -> [PersistValue] -> SQL (Maybe a)
queryMaybeSingle query params = do
    (result :: [Single a]) <- rawSql query params
    return (fmap unSingle $ listToMaybe result)

-- | Runs a query that returns a single value.
querySingle :: forall a. PersistField a => Text -> [PersistValue] -> SQL a
querySingle query params = do
    mbValue <- queryMaybeSingle query params
    case mbValue of
      Nothing -> fail "Database query error."
      Just value -> return value
