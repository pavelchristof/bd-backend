module Sql.Auth where

import Imports

userWithIdExists :: UserId -> SQL Bool
userWithIdExists uid = querySingle
    "SELECT EXISTS (SELECT 1 FROM Users WHERE id=?)"
    [toPersistValue uid]

userWithNameExists :: Text -> SQL Bool
userWithNameExists username = querySingle
    "SELECT EXISTS (SELECT 1 FROM Users WHERE username=?)"
    [toPersistValue username]

getUserName :: UserId -> SQL Text
getUserName uid = querySingle
    "SELECT username FROM Users WHERE id=?"
    [toPersistValue uid]

-- TODO: Hash the passwords.

createUser :: Text -> Text -> SQL UserId
createUser username password = querySingle
    "INSERT INTO Users (username, password) VALUES (?, ?) RETURNING id"
    [toPersistValue username, toPersistValue password]

findUser :: Text -> Text -> SQL (Maybe UserId)
findUser username password = queryMaybeSingle
    "SELECT id FROM Users WHERE username=? AND password=?"
    [toPersistValue username, toPersistValue password]

deleteUser :: UserId -> SQL ()
deleteUser uid = execStmt
    "DELETE FROM Users WHERE id=?"
    [toPersistValue uid]
