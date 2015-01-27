module Sql.Primitives where

import Imports
import Sql.User
import Sql.Common

getPrimitiveId :: Text -> UserSQL (Maybe PrimitiveId)
getPrimitiveId name = queryMaybeSingle
    "SELECT $.Primitives.id \
    \FROM $.Primitives \
    \  INNER JOIN $.Types ON $.Types.id = $.Primitives.id \
    \WHERE name = ?"
    [toPersistValue name]

getPrimitive :: Text -> UserSQL (Maybe DBType)
getPrimitive name = queryMaybeSingle
    "SELECT $.Declarations.file, $.Types.name \
    \FROM $.Primitives \
    \  INNER JOIN $.Types ON $.Types.id = $.Primitives.id \
    \  INNER JOIN $.Declarations ON $.Declarations.id = $.Primitives.id \
    \WHERE $.Types.name = ?"
    [toPersistValue name]

getPrimitives :: UserSQL [DBType]
getPrimitives = queryMany
    "SELECT $.Declarations.file, $.Types.name \
    \FROM $.Primitives \
    \  INNER JOIN $.Types ON $.Types.id = $.Primitives.id \
    \  INNER JOIN $.Declarations ON $.Declarations.id = $.Primitives.id"
    []

createPrimitive :: DBType -> UserSQL PrimitiveId
createPrimitive ty = do
    primId <- createType ty
    execStmt
      "INSERT INTO $.Primitives (id) \
      \VALUES (?)"
      [toPersistValue primId]
    return primId
