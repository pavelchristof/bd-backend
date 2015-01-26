module Sql.Schema where

import Imports
import Session.User

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
createSchema uid = rawExecute sql []
  where
    template = decodeUtf8 $(embedFile "sql/create_schema.sql")
    sql = T.replace "$" (schemaName uid) template

-- | Drops the schema.
dropSchema :: UserId -> SQL ()
dropSchema user = rawExecute sql []
  where
    template = decodeUtf8 $(embedFile "sql/drop_schema.sql")
    sql = T.replace "$" (schemaName user) template

-- | Returns the
tableOfUser :: UserId -> Text -> Text
tableOfUser user table = schemaName user <> "_" <> table
