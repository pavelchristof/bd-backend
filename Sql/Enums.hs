module Sql.Enums where

import Imports
import Sql.User
import Sql.Common

getEnumId :: Text -> UserSQL (Maybe EnumId)
getEnumId name = queryMaybeSingle
    "SELECT $.Enumerations.id \
    \FROM $.Enumerations \
    \  INNER JOIN $.Types ON $.Types.id = $.Enumerations.id \
    \WHERE name = ?"
    [toPersistValue name]

getEnumItems :: EnumId -> UserSQL [Text]
getEnumItems enumId = queryMany
    "SELECT $.Enumerators.name \
    \FROM $.Enumerators \
    \WHERE $.Enumerators.enum = ?"
    [toPersistValue enumId]

consEnum :: Text -> EnumId -> UserSQL DBEnum
consEnum enumName enumId = do
    items <- getEnumItems enumId
    decl <- getDecl enumId
            `orElseM` fail "The enum does not exist."
    return (DBEnum (DBType decl enumName) items)

getEnum :: Text -> UserSQL (Maybe DBEnum)
getEnum name = do
    mbEnumId <- getEnumId name
    forM mbEnumId $ consEnum name

getEnums :: UserSQL [DBEnum]
getEnums = do
    enums <- queryMany
        "SELECT $.Types.name, $.Enumerations.id \
        \FROM $.Enumerations \
        \  INNER JOIN $.Types ON $.Types.id = $.Enumerations.id"
        []
    mapM (uncurry consEnum) enums

createEnum :: DBEnum -> UserSQL EnumId
createEnum e = do
    enumId <- createType (enumType e)
    execStmt
        "INSERT INTO $.Enumerations (id) \
        \VALUES (?)"
        [toPersistValue enumId]
    forM_ (enumItems e) $ \item -> do
        itemId <- createValue (DBValue (typeDecl (enumType e)) item)
        execStmt
            "INSERT INTO $.Enumerators (id, name, enum) \
            \VALUES (?, ?, ?)"
            [toPersistValue itemId, toPersistValue item, toPersistValue enumId]
    return enumId
