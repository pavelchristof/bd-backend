module Sql.Schema where

import Imports

import qualified Data.Text as T
import Data.FileEmbed

-- | Creates the Users table.
createUsers :: SQL ()
createUsers = rawExecute sql []
  where
    sql = decodeUtf8 $(embedFile "sql/create_users.sql")

-- | User-specific schema name.
schemaName :: UserId -> Text
schemaName uid = "user_" <> pack (show uid)

-- | Creates a schema for an user based on a template.
createSchema :: UserId -> SQL ()
createSchema uid = rawExecute (sqlForUser uid template) []
  where
    template = decodeUtf8 $(embedFile "sql/create_schema.sql")

-- | Drops the schema.
dropSchema :: UserId -> SQL ()
dropSchema uid = rawExecute (sqlForUser uid template) []
  where
    template = decodeUtf8 $(embedFile "sql/drop_schema.sql")

sqlForUser :: UserId -> Text -> Text
sqlForUser uid sql = T.replace "$." (schemaName uid <> "_") sql
