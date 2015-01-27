module Sql.Queries where

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
      "SELECT $.Classes.id \
      \FROM $.Classes \
      \  INNER JOIN $.Types ON $.Types.id = $.Classes.id \
      \WHERE $.Types.name = ?")
    [toPersistValue name]

getFieldId :: UserId -> ClassId -> Text -> SQL (Maybe FieldId)
getFieldId uid classId fieldIdent = queryMaybeSingle
    (sqlForUser uid
      "SELECT $.Fields.id \
      \FROM $.Fields \
      \WHERE $.Fields.class = ? AND $.Fields.name = ?")
    [toPersistValue classId, toPersistValue fieldIdent]

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

getMethodArgs :: UserId -> MethodId -> SQL DBMethodArgs
getMethodArgs uid methId = DBMethodArgs <$> queryMany
    (sqlForUser uid
      "SELECT $.Types.name \
      \FROM $.Arguments \
      \  INNER JOIN $.Types ON $.Types.id = $.Arguments.type \
      \WHERE $.Arguments.func = ? \
      \ORDER BY $.Arguments.index")
    [toPersistValue methId]

getMethods :: UserId -> ClassId -> SQL [(DBMethod, DBMethodArgs)]
getMethods uid classId = do
    methods <- queryMany
      (sqlForUser uid
        "SELECT $.Methods.id, $.Declarations.file, $.Methods.name, $.Methods.static, $.Types.name \
        \FROM $.Methods \
        \  INNER JOIN $.Declarations ON $.Declarations.id = $.Methods.id \
        \  INNER JOIN $.Types ON $.Types.id = $.Methods.returnType \
        \WHERE $.Methods.class = ?")
      [toPersistValue classId]
    args <- mapM (getMethodArgs uid . methodId) methods
    return $ zip methods args

getMethod :: UserId -> MethodId -> SQL (Maybe (DBMethod, DBMethodArgs))
getMethod uid methId = do
    mbMethod <- queryMaybeSingle
      (sqlForUser uid
        "SELECT $.Methods.id, $.Declarations.file, $.Methods.name, $.Methods.static, $.Types.name \
        \FROM $.Methods \
        \  INNER JOIN $.Declarations ON $.Declarations.id = $.Methods.id \
        \  INNER JOIN $.Types ON $.Types.id = $.Methods.returnType \
        \WHERE $.Methods.id = ?")
      [toPersistValue methId]
    forM mbMethod $ \method -> do
        args <- getMethodArgs uid methId
        return (method, args)

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

createMethod :: UserId -> ClassId -> DBMethod -> DBMethodArgs -> SQL MethodId
createMethod uid classId m (DBMethodArgs args) = do
    mId <- createValue uid (methodValue m)
    retType <- getTypeId uid (methodReturnType m)
               `orElseM` fail ("Invalid method return type: "
                              <> unpack (methodReturnType m))

    rawExecute
      (sqlForUser uid
        "INSERT INTO $.Methods (id, name, class, static, returnType) \
        \VALUES (?, ?, ?, ?, ?)")
      [ toPersistValue mId
      , toPersistValue (valueIdent $ methodValue m)
      , toPersistValue classId
      , toPersistValue (methodStatic m)
      , toPersistValue retType
      ]

    forM_ (zip args [0..]) $ \(arg, idx :: Int) -> do
        typeId <- getTypeId uid arg
                  `orElseM` fail ("Invalid method argument type: "
                                 <> unpack arg)
        rawExecute
          (sqlForUser uid
            "INSERT INTO $.Arguments (func, index, type) \
            \VALUES (?, ?, ?)")
          [toPersistValue mId, toPersistValue idx, toPersistValue typeId]

    return mId

deleteDecl :: UserId -> DeclId -> SQL ()
deleteDecl uid declId = rawExecute
    (sqlForUser uid
      "DELETE FROM $.Declarations \
      \WHERE id = ?")
    [toPersistValue declId]
