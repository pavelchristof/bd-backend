module Sql.Classes where

import Imports
import Sql.Schema

getTypeId :: UserId -> Text -> SQL (Maybe TypeId)
getTypeId uid name = queryMaybeSingle
    (sqlForUser uid
      "SELECT id \
      \FROM $.Types \
      \WHERE name = ?")
    [toPersistValue name]

getClassId :: UserId -> Text -> SQL (Maybe ClassId)
getClassId uid name = queryMaybeSingle
    (sqlForUser uid
      "SELECT id \
      \FROM $.Classes \
      \  INNER JOIN $.Types ON $.Types.id = $.Classes.id \
      \WHERE $.Types.name = ?")
    [toPersistValue name]

getClass :: UserId -> Text -> SQL (Maybe DBClass)
getClass uid name = queryMaybeSingle
    (sqlForUser uid
      "SELECT $.Declarations.file, $.Types.name, $.Classes.isStruct \
      \FROM $.Classes \
      \  INNER JOIN $.Types ON $.Types.id = $.Classes.id \
      \  INNER JOIN $.Declarations ON $.Declarations.id = $.Classes.id \
      \WHERE $.Types.name = ?")
    [toPersistValue name]

getFields :: UserId -> ClassId -> SQL [DBField]
getFields uid cid = queryMany
    (sqlForUser uid
      "SELECT $.Declarations.file, $.Fields.name, $.Fields.static, $.Types.name \
      \FROM $.Fields \
      \  INNER JOIN $.Types ON $.Fields.type = $.Types.id \
      \  INNER JOIN $.Declarations ON $.Declarations.id = $.Fields.id \
      \WHERE $.Fields.class = ?")
    [toPersistValue cid]

getField :: UserId -> ClassId -> Text -> SQL (Maybe DBField)
getField uid cid name = queryMaybeSingle
    (sqlForUser uid
      "SELECT $.Declarations.file, $.Fields.name, $.Fields.static, $.Types.name \
      \FROM $.Fields \
      \  INNER JOIN $.Types ON $.Fields.type = $.Types.id \
      \  INNER JOIN $.Declarations ON $.Declarations.id = $.Fields.id \
      \WHERE $.Fields.class = ? AND $.Fields.name = ?")
    [toPersistValue cid, toPersistValue name]

createDecl :: UserId -> DBDecl -> SQL DeclId
createDecl uid decl = querySingle
    (sqlForUser uid
      "INSERT INTO $.Declarations (file) \
      \VALUES (?) \
      \RETURNING id")
    [toPersistValue (declFile decl)]

createType :: UserId -> DBType -> SQL TypeId
createType uid ty = do
    typeId <- createDecl uid (typeDecl ty)
    rawExecute
      (sqlForUser uid
        "INSERT INTO $.Types (id, name) \
        \VALUES (?, ?)")
      [toPersistValue typeId, toPersistValue (typeName ty)]
    return typeId

createValue :: UserId -> DBValue -> SQL ValueId
createValue uid val = do
    vid <- createDecl uid (valueDecl val)
    rawExecute
      (sqlForUser uid
        "INSERT INTO $.Values (id, name) \
        \VALUES (?, ?)")
      [toPersistValue vid, toPersistValue (valueIdent val)]
    return vid

createClass :: UserId -> DBClass -> SQL ClassId
createClass uid cl = do
    classId <- createType uid (classType cl)
    rawExecute
      (sqlForUser uid
        "INSERT INTO $.Classes (id, isStruct) \
        \VALUES (?, ?)")
      [toPersistValue classId, toPersistValue (classIsStruct cl)]
    return classId

createField :: UserId -> ClassId -> DBField -> SQL FieldId
createField uid classId f = do
    typeId <- getTypeId uid (fieldTypeName f)
              `orElseM` fail "Invalid field type."
    fieldId <- createValue uid (fieldValue f)
    rawExecute
      (sqlForUser uid
        "INSERT INTO $.Fields (id, name, class, static, type) \
        \VALUES (?, ?, ?, ?, ?)")
      [ toPersistValue fieldId
      , toPersistValue (valueIdent $ fieldValue f)
      , toPersistValue classId
      , toPersistValue (fieldStatic f)
      , toPersistValue typeId
      ]
    return fieldId
