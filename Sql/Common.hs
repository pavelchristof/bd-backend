module Sql.Common where

import Imports
import Sql.User

getTypeId :: Text -> UserSQL (Maybe TypeId)
getTypeId name = queryMaybeSingle
    "SELECT id \
    \FROM $.Types \
    \WHERE name = ?"
    [toPersistValue name]

createDecl :: DBDecl -> UserSQL DeclId
createDecl decl = querySingle
    "INSERT INTO $.Declarations (file) \
    \VALUES (?) \
    \RETURNING id"
    [toPersistValue (declFile decl)]

createType :: DBType -> UserSQL TypeId
createType ty = do
    typeId <- createDecl (typeDecl ty)
    execStmt
      "INSERT INTO $.Types (id, name) \
      \VALUES (?, ?)"
      [toPersistValue typeId, toPersistValue (typeName ty)]
    return typeId

createValue :: DBValue -> UserSQL ValueId
createValue val = do
    vid <- createDecl (valueDecl val)
    execStmt
      "INSERT INTO $.Values (id, name) \
      \VALUES (?, ?)"
      [toPersistValue vid, toPersistValue (valueIdent val)]
    return vid

deleteDecl :: DeclId -> UserSQL ()
deleteDecl declId = execStmt
    "DELETE FROM $.Declarations \
    \WHERE id = ?"
    [toPersistValue declId]
